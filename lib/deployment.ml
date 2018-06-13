(* compatible with JSON returned by XenRT,
 * with extra fields needed for our testing *)
open Rresult
open Lwt.Infix
open Context

type ipaddr = Ipaddr.t

let typ_of_ipaddr =
  Rpc.Types.Abstract
    { aname= "ipaddr"
    ; test_data= [Ipaddr.of_string_exn "127.0.0.1"]
    ; rpc_of= (fun t -> Rpc.String (Ipaddr.to_string t))
    ; of_rpc=
        (function
          | Rpc.String s -> (
            try Ok (Ipaddr.of_string_exn s)
            with Ipaddr.Parse_error (msg, arg) -> R.error_msgf "%s: %s" msg arg
            )
          | r ->
              R.error_msgf "typ_of_ipaddr: expected rpc string got %s"
                (Rpc.to_string r)) }


type access =
  {hostname: string; ipaddress: ipaddr; password: string; username: string}
  [@@deriving rpcty]

type os = {family: string} [@@deriving rpcty]

module StringMap = Map.Make (String)

type string_map = string StringMap.t

let typ_of_string_map =
  Rpc.Types.Abstract
    { aname= "stringmap"
    ; test_data= [StringMap.singleton "a" "b"]
    ; rpc_of=
        (fun t -> Rpc.Dict StringMap.(t |> map Rpc.rpc_of_string |> bindings))
    ; of_rpc=
        (function
          | Rpc.Dict s -> (
            try
              Ok
                (List.fold_left
                   (fun m (k, v) -> StringMap.add k (Rpc.string_of_rpc v) m)
                   StringMap.empty s)
            with Ipaddr.Parse_error (msg, arg) -> R.error_msgf "%s: %s" msg arg
            )
          | r ->
              R.error_msgf "typ_of_string_map: expected rpc dict got %s"
                (Rpc.to_string r)) }


type sr =
  { name: string
  ; _type: string [@key "type"]
  ; uuid: string option
  ; device_config: string_map option }
  [@@deriving rpcty]

type host = {access: access; os: os; srs: sr list} [@@deriving rpcty]

type t = {hosts: host list; physical_pool: host list option} [@@deriving rpcty]

let lwt_of_result = function
  | Ok v -> Lwt.return v
  | Error `Msg m -> Lwt.fail_invalid_arg m


let of_file path =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun ch ->
      Lwt_io.read ch
      >>= fun str ->
      str |> Jsonrpc.of_string |> Rpcmarshal.unmarshal typ_of |> lwt_of_result
  )


let to_file path t =
  Lwt_io.with_file ~mode:Lwt_io.Output path (fun ch ->
      t |> Rpcmarshal.marshal typ_of |> Jsonrpc.to_string |> Lwt_io.write ch )


open Xen_api_lwt_unix

let is_virtual host =
  Astring.String.is_prefix ~affix:"vhost" host.access.hostname


let with_host ~default host msg f =
  Lwt.catch
    (fun () ->
      Context.with_login ~uname:host.access.username ~pwd:host.access.password
        (Ipaddr.to_string host.access.ipaddress) (fun conn -> step conn msg f
      ) )
    (function
        | Api_errors.Server_error (code, _)
          when code = Api_errors.host_is_slave ->
            debug (fun m -> m "Skipping slave %s" host.access.hostname) ;
            Lwt.return default
        | e -> Lwt.fail e)


let pp_device_config =
  Fmt.iter_bindings StringMap.iter Fmt.(Dump.pair string string)


let srs_of ctx host =
  Lwt.catch
    (fun () ->
      host.srs
      |> Lwt_list.filter_map_p (fun sr ->
             match sr.uuid with
             | Some uuid ->
                 rpc ctx @@ SR.get_by_uuid ~uuid
                 >>= fun self ->
                 rpc ctx @@ SR.get_record ~self >>= Lwt.return_some
             | None -> Lwt.return_none ) )
    (function
        | Api_errors.Server_error (code, args)
          when code = Api_errors.uuid_invalid ->
            (* probably got changed since deployed, get latest list of SRs *)
            debug (fun m -> m "Skipping non-existent %a" Fmt.(list string) args) ;
            rpc ctx @@ SR.get_all_records
            >>= Lwt_list.filter_map_p (fun (_, sr) ->
                    if sr.API.sR_type = "iso" || not sr.API.sR_shared then
                      Lwt.return_none
                    else Lwt.return_some sr )
        | e -> Lwt.fail e)


let update_host_device_configs ctx host =
  Context.this_host ctx
  >>= fun host_ref ->
  srs_of ctx host
  >>= Lwt_list.map_p (fun sr ->
          Lwt_list.filter_map_p
            (fun self ->
              rpc ctx @@ PBD.get_record ~self
              >>= fun pbd ->
              if pbd.API.pBD_host = host_ref then
                let device_config =
                  List.fold_left
                    (fun m (k, v) -> StringMap.add k v m)
                    StringMap.empty pbd.API.pBD_device_config
                in
                Lwt.return_some
                  { name= sr.API.sR_name_label
                  ; _type= sr.API.sR_type
                  ; uuid= Some sr.API.sR_uuid
                  ; device_config= Some device_config }
              else Lwt.return_none )
            sr.API.sR_PBDs )
  >|= fun srs ->
  srs |> List.concat
  |> List.sort_uniq (fun a b ->
         match (a.device_config, b.device_config) with
         | None, _ -> -1
         | _, None -> 1
         | Some a, Some b -> StringMap.compare String.compare a b )
  |> fun srs -> {host with srs}


let initialize t =
  let physical_pool =
    t.hosts |> List.filter (fun host -> not (is_virtual host))
    |> function [] -> None | lst -> Some lst
  in
  let update_host host =
    if is_virtual host then
      with_host ~default:host host "get device configs" (fun ctx ->
          update_host_device_configs ctx host )
    else Lwt.return host
  in
  Lwt_list.map_p update_host t.hosts
  >>= fun hosts -> Lwt.return {t with physical_pool; hosts}


let ensure_vhost_snapshot t =
  match t.physical_pool with
  | None -> Lwt.return_unit
  | Some lst ->
      Lwt_list.iter_p
        (fun host ->
          with_host ~default:() host "ensure vhost snapshot"
            Rollback.ensure_pool_snapshot )
        lst

let run () =
  of_file "job.json"
  >>= fun t ->
  initialize t
  >>= fun t -> ensure_vhost_snapshot t >>= fun () -> to_file "myconfig.json" t
