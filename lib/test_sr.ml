open Xen_api_lwt_unix
open Context

let get_management_pifs ctx =
  rpc ctx PIF.get_all >>= Lwt_list.filter_p (fun self -> rpc ctx @@ PIF.get_management ~self)


let enable_clustering t =
  step t "Enable clustering"
  @@ fun ctx ->
  debug (fun m -> m "Checking for existing cluster on pool") ;
  rpc ctx Cluster.get_all
  >>= function
  | _ :: _ :: _ -> Lwt.fail_with "Too many clusters"
  | [cluster] ->
    debug (fun m -> m "Found cluster") ;
    rpc ctx @@ Cluster.pool_resync ~self:cluster
    >>= fun () ->
    rpc ctx Cluster_host.get_all
    >>= Lwt_list.iter_p (fun self ->
        rpc ctx @@ Cluster_host.get_enabled ~self
        >>= function
        | false ->
          debug (fun m -> m "Enabling cluster host") ;
          rpc ctx @@ Cluster_host.enable ~self
        | true -> Lwt.return_unit )
    >>= fun () ->
    debug (fun m -> m "All cluster hosts are enabled") ;
    Lwt.return cluster
  | [] ->
    rpc ctx PIF.get_all >>= Lwt_list.iter_p (fun self ->
        rpc ctx @@ PIF.set_disallow_unplug ~self ~value:true
    ) >>= fun () ->
    get_management_pifs ctx
    >>= function
    | [] -> Lwt.fail_with "No management interface found"
    | pif :: _->
      rpc ctx @@ PIF.get_network ~self:pif
      >>= fun network ->
      debug (fun m -> m "Creating cluster on pool") ;
      rpc ctx
      @@ Cluster.pool_create ~network ~cluster_stack:"corosync" ~token_timeout:1.0
        ~token_timeout_coefficient:0.65

let device_config ?(provider= "iscsi") ~ip ~iqn ?scsiid () =
  let open Ezjsonm in
  let conf =
    [ ("provider", provider)
    ; ("target", (Ipaddr.V4.to_string ip))
    ; ("targetIQN", iqn)
    ; ("journal_size", "8") ]
  in
  (match scsiid with Some id -> ("SCSIid", id) :: conf | None -> conf)

let create_gfs2_sr ctx ~iscsi ?iqn ?scsiid () =
  get_pool_master ctx
  >>= fun (_pool, master) ->
  (* probe *)
  let probe_iqn device_config =
    match iqn with
    | Some iqn ->
      Lwt.return (("targetIQN", iqn) :: device_config)
    | None ->
      rpc ctx @@ SR.probe_ext ~host:master ~device_config ~_type:"gfs2" ~sm_config:[]
      >>= function
      | [] -> Lwt.fail_with "Probe found nothing, not even a SCSIid"
      | probed :: _ ->
        Lwt.return probed.API.probe_result_configuration
  in
  let device_config =
    [("provider", "iscsi"); ("target", Ipaddr.V4.to_string iscsi)] in
  probe_iqn device_config >>= fun device_config ->
  rpc ctx
  @@ SR.probe_ext ~host:master ~device_config ~_type:"gfs2" ~sm_config:[]
  >>= function
    | [] -> Lwt.fail_with "Probe found nothing, not even a SCSIid"
    | probed :: _ ->
      let device_config = probed.API.probe_result_configuration in
      rpc ctx
      @@ SR.probe_ext ~host:master ~device_config ~_type:"gfs2" ~sm_config:[]
    >>= function
    | [] -> Lwt.fail_with "Probe found nothing, not even a SCSIid"
    | probed ->
        let name_label = "gfs2-sr" in
        let name_description = "" in
        let _type = "gfs2" in
        let content_type = "" in
        let shared = true in
        let sm_config = [] in
        Lwt_list.filter_map_p
          (fun probe ->
            match probe.API.probe_result_sr with
            | Some sr -> Lwt.return sr.API.sr_stat_uuid
            | None -> Lwt.return_none )
          probed
        >>= function
          | [] ->
              info (fun m -> m "No SR: creating GFS2") ;
              debug (fun m ->
                  m "Creating SR, device_config: %a" PP.dict device_config ) ;
              rpc ctx
              @@ SR.create ~host:master ~device_config ~physical_size:0L
                   ~name_label ~name_description ~_type ~content_type ~shared
                   ~sm_config
          | uuid :: _ ->
              info (fun m -> m "Found SR, introducing %a" PP.dict device_config) ;
              rpc ctx
              @@ SR.introduce ~uuid ~name_label ~name_description ~_type
                   ~content_type ~shared ~sm_config
              >>= fun sR ->
              rpc ctx
              @@ PBD.create ~host:master ~sR ~device_config ~other_config:[]
              >>= fun pBD ->
              rpc ctx
              @@ PBD.plug ~self:pBD
              >>= fun () ->
              Lwt.return sR

let plug_pbds ctx ~sr =
  rpc ctx @@ SR.get_PBDs ~self:sr
  >>= fun pbds ->
  (* no parallel plug support yet in XAPI *)
  Lwt_list.iter_p
    (fun pbd ->
       rpc ctx @@ PBD.get_currently_attached ~self:pbd
       >>= function
       | true -> Lwt.return_unit
       | false ->
         debug (fun m -> m "PBD plug") ;
         rpc ctx @@ PBD.plug ~self:pbd )
    pbds


let get_gfs2_sr t ~iscsi ?iqn ?scsiid () =
  step t "GFS2 SR"
  @@ fun ctx ->
  debug (fun m -> m "Looking for GFS2 SR") ;
  rpc ctx @@ SR.get_all_records_where ~expr:{|field "type" = "gfs2"|}
  >>= function
  | (sr, _) :: _ ->
    debug (fun m -> m "Found existing GFS2 SR") ;
    plug_pbds ctx ~sr >>= fun () -> Lwt.return sr
  | [] -> create_gfs2_sr ctx ~iscsi ?iqn ?scsiid ()


let do_ha t sr =
  step t "Enable HA"
  @@ fun ctx ->
  get_pool_master ctx
  >>= fun (pool, _master) ->
  rpc ctx @@ Pool.get_ha_enabled ~self:pool
  >>= function
  | true ->
    debug (fun m -> m "HA is already enabled") ;
    Lwt.return_unit
  | false ->
    debug (fun m -> m "Enabling HA") ;
    rpc ctx @@ Pool.enable_ha ~heartbeat_srs:[sr] ~configuration:[]
    >>= fun () ->
    debug (fun m -> m "HA enabled") ;
    Lwt.return_unit


let undo_ha t =
  step t "Disable HA"
  @@ fun ctx ->
  get_pool_master ctx
  >>= fun (pool, _master) ->
  rpc ctx @@ Pool.get_ha_enabled ~self:pool
  >>= function
  | false ->
    debug (fun m -> m "HA is already disabled") ;
    Lwt.return_unit
  | true ->
    debug (fun m -> m "Disabling HA") ;
    rpc ctx Pool.disable_ha
    >>= fun () ->
    debug (fun m -> m "HA disabled") ;
    Lwt.return_unit


let unplug_pbds ctx ~sr =
  rpc ctx @@ SR.get_PBDs ~self:sr
  >>= fun pbds ->
  Lwt_list.iter_p
    (fun pbd ->
       debug (fun m -> m "PBD unplug") ;
       rpc ctx @@ PBD.unplug ~self:pbd )
    pbds


let plug_pbds ctx ~sr =
  rpc ctx @@ SR.get_PBDs ~self:sr
  >>= fun pbds ->
  Lwt_list.iter_p
    (fun pbd ->
       debug (fun m -> m "PBD plug") ;
       rpc ctx @@ PBD.plug ~self:pbd )
    pbds


let detach_sr ctx ~sr =
  unplug_pbds ctx ~sr
  >>= fun () ->
  debug (fun m -> m "Forgetting SR") ;
  rpc ctx @@ SR.forget ~sr


let rec repeat n f = if n = 0 then Lwt.return_unit else f () >>= fun () -> repeat (n - 1) f

let pool_reboot t =
  step t "Reboot pool"
  @@ fun ctx ->
  get_pool_master ctx
  >>= fun (_pool, master) ->
  rpc ctx Host.get_all_records
  >>= fun hosts ->
  rpc ctx @@ Host.get_uuid ~self:master
  >>= fun master_uuid ->
  hosts |> List.filter (fun (_, hostr) -> hostr.API.host_uuid <> master_uuid)
  |> Lwt_list.iter_p (fun (host, hostr) ->
      rpc ctx @@ Host.disable ~host
      >>= fun () ->
      debug (fun m -> m "Rebooting host %s" hostr.API.host_name_label) ;
      rpc ctx @@ Host.reboot ~host
      >>= fun () ->
      debug (fun m -> m "Rebooted host %s" hostr.API.host_name_label) ;
      Lwt.return_unit )


let get_master ~uname ~pwd ip =
  Lwt.catch
    (fun () ->
       with_login ~uname ~pwd (Ipaddr.V4.to_string ip) (fun _ ->
           debug (fun m -> m "Host %a is a master" Ipaddr.V4.pp_hum ip) ;
           Lwt.return ip ) )
    (function
      | Api_errors.Server_error (code, [master]) when code = Api_errors.host_is_slave ->
        let master = Ipaddr.V4.of_string_exn master in
        debug (fun m ->
            m "Host %a is a slave of %a" Ipaddr.V4.pp_hum ip Ipaddr.V4.pp_hum master ) ;
        Lwt.return master
      | e -> Lwt.fail e)


let choose_master masters =
  let pool_master =
    List.sort Ipaddr.V4.compare masters
    |> List.fold_left
      (fun (got_master, prev) ip ->
         let master =
           if Ipaddr.V4.compare prev ip = 0 then
             if got_master <> None && got_master <> Some ip then failwith "multiple pools found"
             else Some ip
           else got_master
         in
         (master, ip) )
      (None, Ipaddr.V4.any)
    |> fst
  in
  match (masters, pool_master) with
  | master :: _, None -> (* all individual hosts, just pick the 1st *)
    Some master
  | _, Some master -> Some master
  | [], _ -> None


let wait_enabled ctx =
  rpc ctx Host.get_all_records
  >>= fun hrecs ->
  debug (fun m -> m "Waiting for all hosts to be enabled in the pool") ;
  let rec loop () =
    rpc ctx @@ Host.is_in_emergency_mode >>= function
    | true -> Lwt.fail_with "Host is in emergency mode"
    | false ->
    Lwt_list.exists_p
      (fun (host, hr) ->
         rpc ctx @@ Host.get_enabled ~self:host
         >>= function
         | true -> Lwt.return_false
         | false ->
           debug (fun m ->
               m "Host %s(%s) is not enabled" hr.API.host_name_label hr.API.host_address ) ;
           Lwt.return_true )
      hrecs
    >>= function true -> Lwt_unix.sleep 0.3 >>= loop | false -> Lwt.return_unit
  in
  loop ()


let fix_management_interfaces ctx =
  (* enabling HA will fail with "Not_found" if the host address IP doesn't match the IP used to join the servers,
     i.e. the management interface must be set to the interface containing the master's IP *)
  get_pool_master ctx
  >>= fun (_pool, master) ->
  rpc ctx @@ Host.get_address ~self:master
  >>= fun ip ->
  rpc ctx PIF.get_all_records
  >>= fun pifs ->
  debug (fun m -> m "Looking for PIF with IP %s" ip) ;
  match
    List.find_all
      (fun (_, pifr) ->
         debug (fun m -> m "PIF %a -> %s" PP.pif pifr pifr.API.pIF_IP) ;
         pifr.API.pIF_IP = ip )
      pifs
  with
  | [(_, pifr)] ->
    debug (fun m -> m "Found master PIF %a" PP.pif pifr) ;
    let network = pifr.API.pIF_network in
    pifs |> List.filter (fun (_, pifr) -> pifr.API.pIF_network = network)
    |> Lwt_list.iter_p (fun (pif, pifr) ->
        if not pifr.API.pIF_management then (
          debug (fun m -> m "PIF %a is not management, reconfiguring" PP.pif pifr) ;
          rpc ctx @@ Host.management_reconfigure ~pif )
        else (
          debug (fun m -> m "PIF %a is already management, nothing to do" PP.pif pifr) ;
          Lwt.return_unit ) )
  | _ :: _ -> Lwt.fail_with ("Multiple PIFs with same IP: " ^ ip)
  | [] -> Lwt.fail_with ("Cannot find PIF for master IP: " ^ ip)


let make_pool ~uname ~pwd conf ips =
  debug (fun m -> m "Getting masters before pool join");
  Lwt_list.map_p (get_master ~uname ~pwd) ips
  >>= fun masters ->
  match choose_master masters with
  | None -> Lwt.fail_with "No masters found in the pool"
  | Some master ->
    let slaves = List.filter (fun ip -> Ipaddr.V4.compare ip master <> 0) masters in
    debug (fun m ->
        m "We need to join %a to %a"
          Fmt.(Dump.list Ipaddr.V4.pp_hum)
          slaves Ipaddr.V4.pp_hum master ) ;
    let master_address = Ipaddr.V4.to_string master in
    slaves
    |> Lwt_list.iter_p (fun ip ->
        with_login ~uname ~pwd (Ipaddr.V4.to_string ip) (fun t ->
            step t "Apply license if needed" (fun ctx ->
                get_pool_master ctx
                >>= fun (_, host) ->
                License.maybe_license_host ctx conf host)
            >>= fun () ->
            step t "Detach shared SRs" (fun ctx ->
                  rpc ctx @@ SR.get_all_records
                  >>= Lwt_list.iter_p (fun (sr, srr) ->
                      if srr.API.sR_shared then
                        detach_sr ctx ~sr
                      else
                        Lwt.return_unit)
            ) >>= fun () ->
            step t "Pool join"
            @@ fun ctx ->
            rpc ctx @@ Pool.join ~master_address ~master_username:uname ~master_password:pwd
          ) )
    >>= fun () ->
    with_login ~uname ~pwd (Ipaddr.V4.to_string master) (fun t ->
        step t "Wait enabled" (fun ctx ->
            wait_enabled ctx
            >>= fun () ->
            fix_management_interfaces ctx
            >>= fun () ->
            debug (fun m ->
                m "All hosts enabled in the pool, master is: %a" Ipaddr.V4.pp_hum master ) ;
            Lwt.return t ) )

let destroy_pool ctx =
  Context.this_host ctx >>= fun this_host ->
  rpc ctx @@ VM.get_all_records
  >>= fun vms ->
  Lwt_list.iter_p (fun (vm, vmr) ->
      if vmr.API.vM_power_state <> `Halted && not vmr.API.vM_is_control_domain then
        rpc ctx @@ VM.hard_shutdown ~vm
      else Lwt.return_unit
    ) vms
  >>= fun () ->
  rpc ctx @@ PBD.get_all_records
  >>= Lwt_list.iter_p (fun (pbd, pbdr) ->
      if pbdr.API.pBD_currently_attached then
        rpc ctx @@ PBD.unplug ~self:pbd
      else Lwt.return_unit)
  >>= fun () ->
  rpc ctx @@ Cluster.get_all
  >>= Lwt_list.iter_s (fun self ->
      rpc ctx @@ Cluster.pool_force_destroy ~self)
  >>= fun () ->
  rpc ctx @@ Host.get_all_records
  >>= Lwt_list.iter_s (fun (host, hostr) ->
      debug (fun m -> m "Host %s is %s" (Ref.string_of host) hostr.API.host_name_label);
      if host <> this_host then
        rpc ctx @@ Pool.eject ~host
      else Lwt.return_unit)

let destroy_pools ~uname ~pwd ips =
  debug (fun m -> m "Destroying pool(s)");
  Lwt_list.map_p (get_master ~uname ~pwd) ips
  >>= fun masters ->
  List.sort_uniq Ipaddr.V4.compare masters
  |> Lwt_list.iter_p (fun master ->
    let ipstr = Ipaddr.V4.to_string master in
    with_login ~uname ~pwd ipstr (fun t ->
        step t "destroy pool" destroy_pool)
  )

let find_templates ctx label =
  rpc ctx @@ VM.get_by_name_label ~label
  >>= Lwt_list.filter_map_p (fun vm_ref ->
      rpc ctx @@ VM.get_record ~self:vm_ref
      >>= fun vmr -> if vmr.API.vM_is_a_template then Lwt.return_some (vm_ref,
                                                                       vmr)
      else Lwt.return_none) >>= fun vms ->
  debug (fun m -> m "found templates with name \"%s\":@,%a" label Fmt.(list (using snd PP.vm_record)) vms);
  Lwt.return vms

let find_vagrant_vms ctx =
  rpc ctx @@ VM.get_all_records >>= fun lst ->
  lst
  |> List.filter (fun (vmref, vmr) ->
      not vmr.API.vM_is_a_template &&
      not vmr.API.vM_is_a_snapshot &&
      not vmr.API.vM_is_control_domain &&
      List.mem_assoc "box_name" vmr.API.vM_other_config)
  |> Lwt.return

let update_map f map to_update =
  let open Astring in
  let map = String.Map.of_list map in
  let updated =
    List.fold_left (fun accum (k, v) -> String.Map.add k v accum) map to_update in
  if String.Map.equal String.equal map updated then begin
    debug (fun m -> m "map has not changed: %a" String.(Map.dump dump) map);
    Lwt.return_unit
  end
  else begin
    debug (fun m -> m "map updated to: %a" String.(Map.dump dump) updated);
    f ~value:(String.Map.bindings updated)
  end

let make_xenserver_template ctx (template_ref, template_rec) =
  update_map (fun ~value -> rpc ctx @@ VM.set_platform ~self:template_ref ~value)
    template_rec.API.vM_platform [
    "nic_type", "e1000";
    "exp-nested-hvm", "1";
  ]

let optimize_vms t =
  step t "Optimize vms"
  @@ fun ctx ->
  find_templates ctx "XenServer"
  >>= fun lst -> Lwt_list.iter_p (make_xenserver_template ctx) lst
  >>= fun () -> find_vagrant_vms ctx
  >>= fun vms -> Lwt_list.iter_p (make_xenserver_template ctx) vms
