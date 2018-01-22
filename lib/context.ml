open Xen_api_lwt_unix

let src =
  Logs.Src.create "testarossa" ~doc:"logs testarossa events"

include (val (Logs_lwt.src_log src : (module Logs_lwt.LOG)))
type 'a log = 'a Logs_lwt.log


let version = "1.1"

let originator = "testarossa"

type rpc = Rpc.call -> Rpc.response Lwt.t

type session_info =
  { master: string
  ; ctx_rpc: rpc
  ; session_id: API.ref_session
  ; master_ref: API.ref_host
  ; pool_ref: API.ref_pool
  }

type t =
  {rpc: rpc; session: session_info Singleton.t; max_expiration_retry: int}

let login ?(max_expiration_retry= 3) ?(timeout= 5.0) ~uname ~pwd master =
  let open Lwt.Infix in
  let rpc = make ~timeout ("https://" ^ master) in
  let login () =
    debug (fun m -> m "Logging in to %s as %s" master uname)
    >>= fun () ->
    Session.login_with_password ~rpc ~uname ~pwd ~version ~originator
    >>= fun session_id ->
    Pool.get_all ~rpc ~session_id
    >>= function
      | [pool_ref] ->
          Pool.get_master ~rpc ~session_id ~self:pool_ref
          >>= fun master_ref ->
          Lwt.return {master; ctx_rpc=rpc; session_id; master_ref; pool_ref}
      | [] ->
          err (fun m -> m "No pools found on %s" master)
          >>= fun () -> Lwt.fail_with "No pools found"
      | _ :: _ :: _ ->
          err (fun m -> m "Too many pools on %s" master)
          >>= fun () -> Lwt.fail_with "Too many pools"
  in
  let r = {rpc; max_expiration_retry; session= Singleton.create login} in
  Singleton.get r.session
  >>= fun info ->
  debug (fun m ->
      let session_id = info.session_id in
      Session.get_uuid ~rpc:r.rpc ~session_id ~self:session_id
      >>= fun uuid -> m "Got session id %s" uuid )
  >>= fun () -> Lwt.return r

let rpc' info f = f ~rpc:info.ctx_rpc ~session_id:info.session_id

(* call [f ~rpc ~session_id] and retry by logging in again on [Api_errors.session_invalid] *)
let rpc ctx f =
  let open Lwt.Infix in
  let rec retry = function
    | n when n > ctx.max_expiration_retry ->
        Logs.debug (fun m -> m "Maximum number of retries exceeded") ;
        Lwt.fail_with "Maximum number of retries exceeded"
    | n ->
        Singleton.get ctx.session
        >>= fun session ->
        Lwt.catch
          (fun () -> f session ~rpc:session.ctx_rpc ~session_id:session.session_id)
          (function
              | Api_errors.Server_error (code, _)
                when code = Api_errors.session_invalid ->
                  debug (fun m -> m "Session is not valid (try #%d)!" n)
                  >>= fun () ->
                  Singleton.invalidate ctx.session session ;
                  retry (n + 1)
              | e ->
                  Lwt.fail e)
  in
  retry 0

type host = {name: string; ip: string}
let get_host_pp ctx self =
  let name = rpc' ctx @@ Host.get_name_label ~self
  and ip = rpc' ctx @@ Host.get_address ~self in
  name >>= fun name ->
  ip >>= fun ip ->
  Lwt.return { name; ip }
                                            

module PP = struct
  open Fmt
  let dict = Dump.(pair string string |> list)
  let rpc_t = using Jsonrpc.to_string string
  let feature = using API.rpc_of_feature_t rpc_t
  let records pp_elt = Dump.(using snd pp_elt |> list)

  let features = using Features.to_compact_string string

  let host ppf h =
    Fmt.pf ppf "@[%s(%s)@]" h.name h.ip
end
