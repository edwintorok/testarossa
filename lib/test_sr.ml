open Xen_api_lwt_unix
open Context

let get_management_pifs ctx =
  rpc' ctx PIF.get_all >>=
    Lwt_list.filter_p (fun self -> rpc' ctx @@ PIF.get_management ~self)

let enable_clustering t =
  rpc t (fun ctx ~rpc ~session_id ->
      debug (fun m -> m "Checking for existing cluster on pool") >>= fun () ->
      rpc' ctx Cluster.get_all >>= function
      | _ :: _ :: _ -> Lwt.fail_with "Too many clusters"
      | [cluster] ->
         debug (fun m -> m "Found cluster") >>= fun () ->
         rpc' ctx Cluster_host.get_all >>=
           Lwt_list.iter_p (fun self ->
               rpc' ctx @@ Cluster_host.get_enabled ~self >>= function
               | false ->
                  debug (fun m -> m "Enabling cluster host" ) >>= fun () ->
                  rpc' ctx @@ Cluster_host.enable ~self
               | true -> Lwt.return_unit
             ) >>= fun () ->
         debug (fun m -> m "All cluster hosts are enabled") >>= fun () ->
         Lwt.return cluster
      | [] ->
         get_management_pifs ctx >>= function
         | [] -> Lwt.fail_with "No management interface found"
         | pif :: _ as pifs ->
            debug (fun m -> m "Setting disallow unplug") >>= fun () ->
            Lwt_list.iter_p (fun self ->
                rpc' ctx @@ PIF.set_disallow_unplug ~self ~value:true) pifs >>= fun () ->

            rpc' ctx @@ PIF.get_network ~self:pif >>= fun network ->

            debug (fun m -> m "Creating cluster on pool") >>= fun () ->
            rpc' ctx @@ Cluster.pool_create ~network ~cluster_stack:"corosync" ~token_timeout:20.0 ~token_timeout_coefficient:1.0
    )

let device_config ?(provider="iscsi") ~ip ~iqn ?scsiid () =
  let open Ezjsonm in
  let conf = [ "provider", `String provider;
    "ips", `String (Ipaddr.V4.to_string ip);
    "iqns", `String iqn;
    "journal_size", `String "8"
  ] in
  (match scsiid with
  | Some id -> ("ScsiId", `String id) :: conf
  | None -> conf) |> dict |> to_string ~minify:true |> fun x -> ["uri", x]

let probe ctx ~iscsi ~iqn =
  (* probe as if it was an iSCSI SR, since we don't have probe for GFS2 yet *)
  Lwt.catch
    (fun () ->
       debug (fun m -> m "Probing %a" Ipaddr.V4.pp_hum iscsi) >>= fun () ->
       rpc' ctx @@ SR.probe ~host:ctx.master_ref
                ~device_config:["target", Ipaddr.V4.to_string iscsi; "targetIQN", iqn]
                ~_type:"lvmoiscsi" ~sm_config:[])
    (fun e -> match e with
       | Api_errors.Server_error (_,[_;_;xml]) -> Lwt.return xml
       | e ->
         Logs.err (fun m -> m "Got another error: %s\n" (Printexc.to_string e));
         Lwt.fail_with "<bad xml>")
  >>= fun xml ->
  let open Ezxmlm in
  debug (fun m -> m "Got probe XML: %s" xml) >>= fun () ->
  let (_,xmlm) = from_string xml in
  let scsiid = xmlm |> member "iscsi-target" |> member "LUN" |> member "SCSIid"
         |> data_to_string in
  debug (fun m -> m "SR Probed: SCSIid=%s\n%!" scsiid) >>= fun () ->
  Lwt.return scsiid

let create_gfs2_sr ctx ~iscsi ~iqn ?scsiid () =
  (match scsiid with
  | None -> probe ctx ~iscsi ~iqn
  | Some scsiid -> Lwt.return scsiid)
  >>= fun scsiid ->
  let device_config = device_config ~ip:iscsi ~iqn ~scsiid () in
  debug (fun m -> m "Creating SR, device_config: %a" PP.dict device_config) >>= fun () ->
  rpc' ctx @@ SR.create ~host:ctx.master_ref ~device_config ~physical_size:0L ~name_label:"gfs2-sr"
    ~name_description:"" ~_type:"gfs2" ~content_type:"" ~shared:true ~sm_config:[]

let plug_pbds ctx ~sr =
  rpc' ctx @@ SR.get_PBDs ~self:sr >>= fun pbds ->
  (* no parallel plug support yet in XAPI *)
  Lwt_list.iter_p (fun pbd ->
      rpc' ctx @@ PBD.get_currently_attached ~self:pbd >>= function
      | true -> Lwt.return_unit
      | false ->
         Logs.debug (fun m -> m "PBD plug");
         rpc' ctx @@ PBD.plug ~self:pbd) pbds

let unplug_pbds ctx ~sr =
  rpc' ctx @@ SR.get_PBDs ~self:sr >>= fun pbds ->
  (* no parallel unplug support yet in XAPI *)
  Lwt_list.iter_p (fun pbd ->
      rpc' ctx @@ PBD.get_currently_attached ~self:pbd >>= function
      | false -> Lwt.return_unit
      | true ->
         Logs.debug (fun m -> m "PBD unplug");
         rpc' ctx @@ PBD.unplug ~self:pbd) pbds

let get_gfs2_sr ctx ~iscsi ~iqn ?scsiid () =
  debug (fun m -> m "Looking for GFS2 SR") >>= fun () ->
  rpc' ctx @@ SR.get_all_records_where ~expr:{|field "type" = "gfs2"|} >>= function
  | (sr, _) :: _ ->
     debug (fun m -> m "Found existing GFS2 SR") >>= fun () ->
     plug_pbds ctx ~sr >>= fun () ->
     Lwt.return sr
  | [] -> create_gfs2_sr ctx ~iscsi ~iqn ?scsiid ()


let do_ha ctx sr =
  rpc' ctx @@ Pool.get_ha_enabled ~self:ctx.pool_ref >>= function
  | true ->
      debug (fun m -> m "HA is already enabled")
  | false ->
      debug (fun m -> m "Enabling HA") >>= fun () ->
      rpc' ctx @@ Pool.enable_ha ~heartbeat_srs:[sr] ~configuration:[] >>= fun () ->
      debug (fun m -> m "HA enabled")


let undo_ha ctx =
  rpc' ctx @@ Pool.get_ha_enabled ~self:ctx.pool_ref >>= function
    | false ->
        debug (fun m -> m "HA is already disabled")
    | true ->
        debug (fun m -> m "Disabling HA") >>= fun () ->
        rpc' ctx Pool.disable_ha >>= fun () ->
        debug (fun m -> m "HA disabled")

let unplug_pbds ctx ~sr =
  rpc' ctx @@ SR.get_PBDs ~self:sr >>= fun pbds ->
  Lwt_list.iter_p (fun pbd ->
    Logs.debug (fun m -> m "PBD unplug");
    rpc' ctx @@ PBD.unplug ~self:pbd) pbds

let plug_pbds ctx ~sr =
  rpc' ctx @@ SR.get_PBDs ~self:sr >>= fun pbds ->
  Lwt_list.iter_p (fun pbd ->
    Logs.debug (fun m -> m "PBD plug");
    rpc' ctx @@ PBD.plug ~self:pbd) pbds

let detach_sr ctx ~sr =
  unplug_pbds ctx ~sr >>= fun () ->
  debug (fun m -> m "Forgetting SR") >>= fun () ->
  rpc' ctx @@ SR.forget ~sr

let rec repeat n f =
  if n = 0 then Lwt.return_unit
  else f () >>= fun () ->
    repeat (n-1) f

let pool_reboot ctx =
  rpc' ctx Host.get_all_records >>= fun hosts ->
  rpc' ctx @@ Host.get_uuid ~self:ctx.master_ref >>= fun master_uuid ->
  hosts
  |> List.filter (fun (_, hostr) -> hostr.API.host_uuid <> master_uuid)
  |> Lwt_list.iter_p (fun (host, hostr) ->
    rpc' ctx @@ Host.disable ~host >>= fun () ->
    debug (fun m -> m "Rebooting host %s" hostr.API.host_name_label) >>= fun () ->
    rpc' ctx @@ Host.reboot ~host >>= fun () ->
    debug (fun m -> m "Rebooted host %s" hostr.API.host_name_label))

let get_master ~uname ~pwd ip =
  Lwt.try_bind (fun () ->
      login ~uname ~pwd (Ipaddr.V4.to_string ip))
    (fun _ ->
      debug (fun m -> m "Host %a is a master" Ipaddr.V4.pp_hum ip) >>= fun () ->
      Lwt.return ip)
    (function
      | Api_errors.Server_error("HOST_IS_SLAVE", [master]) ->
        let master = Ipaddr.V4.of_string_exn master in
        debug (fun m -> m "Host %a is a slave of %a" Ipaddr.V4.pp_hum ip Ipaddr.V4.pp_hum master) >>= fun () ->
        Lwt.return master
      | e -> Lwt.fail e)

let choose_master masters =
  let pool_master =
    List.sort Ipaddr.V4.compare masters |>
    List.fold_left (fun (got_master, prev) ip ->
        let master =
          if Ipaddr.V4.compare prev ip = 0 then
            if got_master <> None &&
            got_master <> Some ip then failwith "multiple pools found"
            else Some ip
          else got_master in
        master, ip) (None, Ipaddr.V4.any) |> fst in
  match masters, pool_master with
  | master :: _, None ->
    (* all individual hosts, just pick the 1st *)
    Some master
  | _, Some master -> Some master
  | [], _ -> None

let rec wait_enabled ctx () =
  debug (fun m -> m "Waiting for all hosts to be enabled in the pool") >>= fun () ->
  rpc' ctx Host.get_all_records >>= fun hrecs ->
  if List.exists (fun (_,r) ->
         match r.API.host_enabled with
         | true -> false
         | false ->
            Logs.debug (fun m -> m "Host %s(%s) is not enabled" r.API.host_name_label r.API.host_address);
            true
       ) hrecs then
    Lwt_unix.sleep 0.3 >>= wait_enabled ctx
  else Lwt.return_unit

let fix_management_interfaces ctx =
  (* enabling HA will fail with "Not_found" if the host address IP doesn't match the IP used to join the servers,
     i.e. the management interface must be set to the interface containing the master's IP *)
  rpc' ctx @@ Host.get_address ~self:ctx.master_ref >>= fun ip ->
  rpc' ctx PIF.get_all_records >>= fun pifs ->
  debug (fun m -> m "Looking for PIF with IP %s" ip) >>= fun () ->
  match List.find_all (fun (_, pifr) ->
      Logs.debug (fun m -> m "PIF %a -> %s" PP.pif pifr pifr.API.pIF_IP);
      pifr.API.pIF_IP = ip) pifs with
  | [(_, pifr)] ->
    Logs.debug (fun m -> m "Found master PIF %a" PP.pif pifr);
    let network = pifr.API.pIF_network in
    pifs |>
    List.filter (fun (_, pifr) -> pifr.API.pIF_network = network) |>
    Lwt_list.iter_p (fun (pif, pifr) ->
        if not pifr.API.pIF_management then begin
          Logs.debug (fun m -> m "PIF %a is not management, reconfiguring" PP.pif pifr);
          rpc' ctx @@ Host.management_reconfigure ~pif
        end else begin
          Logs.debug (fun m -> m "PIF %a is already management, nothing to do" PP.pif pifr);
          Lwt.return_unit
        end)
  | _ :: _ ->
    Lwt.fail_with ("Multiple PIFs with same IP: " ^ ip)
  | [] ->
     Lwt.fail_with ("Cannot find PIF for master IP: " ^ ip)

let make_pool ~uname ~pwd ips =
  Lwt_list.map_p (get_master ~uname ~pwd) ips >>= fun masters ->
  match choose_master masters with
  | None ->
    Lwt.fail_with "No masters found in the pool"
  | Some master ->
    let slaves = List.filter (fun ip -> Ipaddr.V4.compare ip master <> 0) masters in
    Logs.debug (fun m -> m "We need to join %a to %a" Fmt.(Dump.list Ipaddr.V4.pp_hum) slaves Ipaddr.V4.pp_hum master);
    let master_address = Ipaddr.V4.to_string master in
    slaves |>
    Lwt_list.iter_p (fun ip ->
        login ~uname ~pwd (Ipaddr.V4.to_string ip) >>= fun t ->
        rpc t @@ fun _ -> Pool.join ~master_address ~master_username:uname ~master_password:pwd) >>= fun () ->
    login ~uname ~pwd (Ipaddr.V4.to_string master) >>= fun t ->
    rpc t (fun ctx ~rpc ~session_id ->
        wait_enabled ctx () >>= fun () ->
        fix_management_interfaces ctx >>= fun () ->
        debug (fun m -> m "All hosts enabled in the pool, master is: %a" Ipaddr.V4.pp_hum master) >>= fun () ->
        Lwt.return t
      )

