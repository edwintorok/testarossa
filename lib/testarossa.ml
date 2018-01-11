open Xen_api_lwt_unix
open Lwt.Infix    

let src = Logs.Src.create "testarossa" ~doc:"logs testarossa events"
module Log = (val Logs.src_log src : Logs.LOG)

type rpc = Rpc.call -> Rpc.response Lwt.t
type vm = API.ref_VM * API.vM_t
type vm_list = vm list

type context = {
  uname: string;
  pwd: string;
  machine: string;
  rpc: rpc;
  session_id: API.ref_session;
}

module Ipset = Set.Make(Ipaddr.V4)
module PP = struct
  let vm_uuid = Fmt.(using (fun vmr -> vmr.API.vM_uuid) string)
  let vm_name_label = Fmt.(using (fun vmr -> vmr.API.vM_name_label) string)
  let vm_record ppf vmr =
    Fmt.pf ppf "%a(%a)" vm_uuid vmr vm_name_label vmr

  let pif ppf pifr =
    Fmt.pf ppf "%s" pifr.API.pIF_uuid

  let ipset = Fmt.(using Ipset.elements (list Ipaddr.V4.pp_hum))
end

let call_rpc ~context f =
  f ~rpc:context.rpc ~session_id:context.session_id

let find_vm ~context ~name =
  call_rpc ~context VM.get_by_name_label ~label:name >>=
  Lwt_list.filter_map_p (fun vm_ref ->
      call_rpc ~context VM.get_record ~self:vm_ref >>= fun vmr ->
      if not vmr.API.vM_is_a_template then Lwt.return_some (vm_ref, vmr)
      else Lwt.return_none) >>= fun vms ->
  Logs.debug (fun m -> m "found VMs with name \"%s\":@,%a" name Fmt.(list (using snd PP.vm_record)) vms);
  Lwt.return vms

let find_templates ~context ~name =
  (* get_all_records_where doesn't seem to be able to filter on name_label/name-label *)
  call_rpc ~context VM.get_by_name_label ~label:name >>=
  Lwt_list.filter_map_p (fun vm_ref ->
      call_rpc ~context VM.get_record ~self:vm_ref >>= fun vmr ->
      if vmr.API.vM_is_a_template then Lwt.return_some (vm_ref, vmr)
      else Lwt.return_none) >>= fun vms ->
  Logs.debug (fun m -> m "found templates with name \"%s\":@,%a" name Fmt.(list (using snd PP.vm_record)) vms);
  Lwt.return vms

let update_map f map to_update =
  let open Astring in
  let map = String.Map.of_list map in
  let updated =
    List.fold_left (fun accum (k, v) -> String.Map.add k v accum) map to_update in
  if String.Map.equal String.equal map updated then begin
    Logs.debug (fun m -> m "map has not changed: %a" String.(Map.dump dump) map);
    Lwt.return_unit
  end
  else begin
    Logs.debug (fun m -> m "map updated to: %a" String.(Map.dump dump) updated);
    f ~value:(String.Map.bindings updated)
  end

let make_xenserver_template ~context (template_ref, template_rec) =
  update_map (call_rpc ~context VM.set_platform ~self:template_ref)
    template_rec.API.vM_platform [
    "nic_type", "e1000";
    "exp-nested-hvm", "1";
  ]

let with_session ~uname ~pwd ~machine f =
  let rpc = make (Printf.sprintf "https://%s" machine) in
  Logs.debug (fun m -> m "Logging in to %s with user %s" machine uname);
  Session.login_with_password ~rpc ~uname ~pwd ~version:"1.1" ~originator:"testarossa" >>= fun session_id ->
  Lwt.finalize (fun () -> f ~context:{uname; pwd; machine; rpc; session_id})
    (fun () ->
       Logs.debug (fun m -> m "Logging out of %s" machine);
       Session.logout ~rpc ~session_id
    )

let with_default_session f =
  let home =
    try Sys.getenv "HOME"
    with Not_found -> failwith "HOME is not set" in
  let pwf = Filename.concat home ".testarossa" in
  let machine = Filename.concat home ".testarossa_host" in
  Lwt_io.lines_of_file machine |> Lwt_stream.to_list >>= function
  | [machine] ->
    Log.debug (fun m -> m "Using physical XS host(pool) %s" machine);
    begin
      Lwt_io.lines_of_file pwf |> Lwt_stream.to_list >>= function
      | [ uname; pwd ] ->
        with_session ~uname ~pwd ~machine f
      | l ->
        Lwt.fail_with (Printf.sprintf "Must specify exactly 2 lines in %s (got %d)" pwf (List.length l))
    end
  | _ -> Lwt.fail_with (Printf.sprintf "Must specify a single host in %s" machine)

open Bos

let pp_process_status ppf = function
  | Unix.WEXITED c -> Fmt.pf ppf "exited with code %d" c
  | Unix.WSIGNALED n -> Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal n
  | Unix.WSTOPPED n -> Fmt.pf ppf "stopped by signal %a" Fmt.Dump.signal n

let lwt_run cmd =
  Logs.debug (fun m -> m "Running command @[%a@]" Cmd.pp cmd);
  let cmda = Cmd.to_list cmd |> Array.of_list in
  Lwt_process.with_process_in (cmda.(0), cmda) ~stdin:`Dev_null ~timeout:30. (fun proc ->
      let pid = proc#pid in
      let header = Printf.sprintf "%s[%d]" cmda.(0) pid in
      Lwt.catch (fun () ->
          Lwt_io.read_lines proc#stdout |> Lwt_stream.to_list >>= fun stdout ->
          Logs.debug (fun m -> m "stdout: %a" ~header Fmt.(list string) stdout);

          proc#close >>= fun status ->
          Logs.debug (fun m -> m "%a" pp_process_status status);
          if status <> WEXITED 0 then
            Lwt.fail_with (Fmt.strf "%a: %a" Cmd.pp cmd pp_process_status status)
          else
            Lwt.return stdout
        ) (fun e ->
          proc#terminate;
          Lwt.fail e))

let ssh ~context cmd =
  let target = Printf.sprintf "%s@%s" context.uname context.machine in
  let cmd = Cmd.(v "ssh" % target % "bash" % "-l" % "-c" % Filename.quote (Cmd.to_string cmd)) in
  lwt_run cmd

let get_arp_map ~context =
  let open Astring in
  ssh ~context Cmd.(v "arp" % "-n") >>= fun lines ->
  Lwt.return @@ List.fold_left (fun accum line ->
    match String.fields ~empty:false line with
    | ip :: _ :: mac :: _ ->
        (match Ipaddr.V4.of_string ip with
        | Some ipaddr ->
          String.Map.add mac ipaddr accum
        | None -> accum)
    | _ -> accum) String.Map.empty lines

let get_xs_local_ips ~context ip =
  (* FIXME: password auth... *)
  let open Astring in
  let target = Printf.sprintf "%s@%s" context.uname context.machine in
  let cmd = Cmd.(v "ssh" % "-A" % target % (Printf.sprintf "ssh -o StrictHostKeyChecking=false %s@%s bash -c '. /etc/xensource-inventory && xe pif-list params=IP host-uuid=$INSTALLATION_UUID'" context.uname (Ipaddr.V4.to_string ip))) in
  lwt_run cmd >>= fun lines ->
  let ips = String.concat ~sep:"\n" lines in
  String.fields ~empty:false ~is_sep:(function ',' -> true | _ -> false) ips |>
  List.fold_left (fun accum ip -> Ipset.add (Ipaddr.V4.of_string_exn ip) accum) Ipset.empty |>
  Lwt.return

let get_vm_ips ~context vms =
  let open Astring in
  get_arp_map ~context >>= fun arp_map ->
  vms |>
  Lwt_list.map_p (fun ((_, vmr) as vm) ->
      vmr.API.vM_VIFs |>
      Lwt_list.map_p (fun vif -> call_rpc ~context VIF.get_MAC ~self:vif) >>= fun macs ->
      Logs.debug (fun m -> m "VM %a has MACs: %a" PP.vm_record vmr Fmt.(list string) macs);
      let ips = List.fold_left (fun accum mac ->
          match String.Map.find mac arp_map with
          | Some ip -> Ipset.add ip accum
          | None -> accum) Ipset.empty macs in
      Logs.debug (fun m -> m "VM %a has IPs: %a" PP.vm_record vmr PP.ipset ips);
      Lwt_list.map_p (fun ip -> get_xs_local_ips ~context ip) (Ipset.elements ips) >>= fun _ ->
      (* this finds the 169.x addresses, but not necessarily the others.
         We have a XenServer running on the virtual hosts, so lets ask it for the full list of IPs
      *)
      Lwt.return (vm, ips))

let get_master ~context ip =
  Lwt.catch (fun () ->
      with_session ~uname:context.uname ~pwd:context.pwd ~machine:(Ipaddr.V4.to_string ip)
        (fun ~context ->
           Logs.debug (fun m -> m "Host %a is a master" Ipaddr.V4.pp_hum ip);
           Lwt.return ip
        ))
    (function
      | Api_errors.Server_error("HOST_IS_SLAVE", [master]) ->
        let master = Ipaddr.V4.of_string_exn master in
        Logs.debug (fun m -> m "Host %a is a slave of %a" Ipaddr.V4.pp_hum ip Ipaddr.V4.pp_hum master);
        Lwt.return master
      | e -> Lwt.fail e)

let choose_master masters =
  let pool_master =
    List.sort Ipaddr.V4.compare masters |>
    List.fold_left (fun (got_master, prev) ip ->
        let master =
          if Ipaddr.V4.compare prev ip = 0 then
            if got_master <> None then failwith "multiple pools found"
            else Some ip
          else got_master in
        master, ip) (None, Ipaddr.V4.any) |> fst in
  match masters, pool_master with
  | master :: _, None ->
    (* all individual hosts, just pick the 1st *)
    Some master
  | _, Some master -> Some master
  | [], _ -> None

let make_pool ~context ips =
  Lwt_list.map_p (get_master ~context) ips >>= fun masters ->
  match choose_master masters with
  | None ->
    Lwt.fail_with "No masters found in the pool"
  | Some master ->
    let slaves = List.filter (fun ip -> Ipaddr.V4.compare ip master <> 0) masters in
    Logs.debug (fun m -> m "We need to join %a to %a" Fmt.(Dump.list Ipaddr.V4.pp_hum) slaves Ipaddr.V4.pp_hum master);
    let master_address = Ipaddr.V4.to_string master in
    slaves |>
    Lwt_list.iter_p (fun ip ->
        with_session ~uname:context.uname ~pwd:context.pwd ~machine:(Ipaddr.V4.to_string ip) (fun ~context ->
            call_rpc ~context Pool.join ~master_address ~master_username:context.uname ~master_password:context.pwd)) >>= fun () ->
    Lwt.return master

let with_pool ~context master f =
  with_session ~uname:context.uname ~pwd:context.pwd ~machine:(Ipaddr.V4.to_string master) (fun ~context ->
      let rec wait_enabled () =
        Logs.debug (fun m -> m "Waiting for all hosts to be enabled in the pool");
        call_rpc ~context Host.get_all_records >>= fun hrecs ->
        if List.exists (fun (_,r) -> not r.API.host_enabled) hrecs then
          Lwt_unix.sleep 0.1 >>= wait_enabled
        else Lwt.return_unit
      in
      wait_enabled () >>= fun () ->
      Logs.debug (fun m -> m "All hosts enabled in the pool, master is: %a" Ipaddr.V4.pp_hum master);
      f ~context)

let get_management_pifs ~context =
  call_rpc ~context PIF.get_all >>=
  Lwt_list.filter_p (fun self -> call_rpc ~context PIF.get_management ~self)

let enable_clustering ~context =
  Logs.debug (fun m -> m "Checking for existing cluster on pool");
  call_rpc ~context Cluster.get_all >>= function
  | _ :: _ :: _ -> Lwt.fail_with "Too many clusters"
  | [cluster] -> Lwt.return cluster
  | [] ->
    get_management_pifs ~context >>= function
    | [] -> Lwt.fail_with "No management interface found"
    | pif :: _ as pifs ->
      Logs.debug (fun m -> m "Setting disallow unplug");
      Lwt_list.iter_p (fun self ->
          call_rpc ~context PIF.set_disallow_unplug ~self ~value:true) pifs >>= fun () ->

      call_rpc ~context PIF.get_network ~self:pif >>= fun network ->

      Logs.debug (fun m -> m "Creating cluster on pool");
      call_rpc ~context Cluster.pool_create ~network ~cluster_stack:"corosync" ~token_timeout:20.0 ~token_timeout_coefficient:1.0
