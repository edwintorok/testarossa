open Bos_setup
open Testarossa
open Lwt.Infix

let ip = OS.Arg.conv ~docv:"IP" (fun s ->
    R.trap_exn Ipaddr.V4.of_string_exn s |>
    R.error_exn_trap_to_msg) Ipaddr.V4.pp_hum

let iscsi = OS.Arg.(opt ~docv:"iscsi" ["iscsi"] ~absent:None (some ip))
let iqn = OS.Arg.(opt ~docv:"iqn" ["iqn"] ~absent:None (some string))
let scsiid = OS.Arg.(opt ~docv:"SCSIid" ["scsiid"] ~absent:None (some string))

let license_server = OS.Arg.(opt ["license-server-address"] ~absent:None (some string))
let license_server_port = OS.Arg.(opt ["license-server-port"] int ~absent:27000)
let edition = OS.Arg.(opt ["edition"] string ~absent:"enterprise-per-socket")

(* physical XS host containing the virtual XS hosts if any *)
let physical = OS.Arg.(opt ["physical"] ~absent:None (some string))

let rollback = OS.Arg.flag ["rollback"]

let uname = "root" (* TODO: with_default_session read from . file *)
let pwd = ""

let with_physical f =
  match physical with
  | Some host ->
     Context.debug (fun m -> m "Logging in to physical host %s" host) >>= fun () ->
     Context.with_login ~uname ~pwd host @@ fun phys ->
     Rollback.ensure_pool_snapshot phys >>= fun () ->
     f phys
  | None -> Lwt.return_unit

let sync t =
  let open Xen_api_lwt_unix in
  let open Context in
  step t "Sync pool database" (fun ctx ->
      rpc ctx Pool.sync_database)

let main () =
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ();
  let ips = OS.Arg.(parse ~pos:ip ()) in
  with_physical (fun phys ->
      Logs.debug (fun m -> m "rollback: %b" rollback);
      if rollback then
        Rollback.rollback_pool phys
      else
        Lwt.return_unit)

  >>= fun () ->

  Test_sr.make_pool ~uname ~pwd ips >>= fun t ->
  (* TODO: do before/after too *)
  sync t >>= fun () ->

  
  License.maybe_apply_license_pool t ?license_server ~license_server_port ~edition [Features.HA; Features.Corosync] >>= fun () ->

  Test_sr.enable_clustering t >>= fun _cluster ->
  (match iscsi, iqn with
  | Some iscsi, Some iqn ->
     Test_sr.get_gfs2_sr t ~iscsi ~iqn ?scsiid () >>= fun _gfs2 ->
     Lwt.return_unit
  | _ ->
     Lwt.return_unit) >>= fun () ->
  Allowed_ops.T1.execute t >>= fun () ->
  Allowed_ops.T2.execute t >>= fun () ->
  Allowed_ops.T3.execute t >>= fun () ->
  Allowed_ops.T4.execute t >>= fun () ->
  Allowed_ops.T5.execute t >>= fun () ->
  Allowed_ops.T6.execute t

(*  with_default_session (fun ~context ->
      find_templates ~context ~name:"XenServer" >>= fun lst ->
      Lwt_list.iter_p (make_xenserver_template ~context) lst >>= fun () ->
      find_vms ~context >>= fun vms ->
      Lwt_list.iter_p (make_xenserver_template ~context) vms >>= fun () ->
      find_vm ~context ~name:Sys.argv.(1) >>= fun vms ->

      Logs.debug (fun m -> m "IPs: %a" Fmt.(Dump.list Ipaddr.V4.pp_hum) ips);
      make_pool ~context ?license_server ~license_server_port ips >>= fun master ->
      with_pool ~context master (fun ~context ->
          enable_clustering ~context >>= fun cluster ->
          match iscsi, iqn with
          | Some iscsi, Some iqn ->
            get_gfs2_sr ~context ~iscsi ~iqn >>= fun gfs2 ->
            Logs.debug (fun m -> m "got SR");
            (*repeat 100 (fun () ->
                unplug_pbds ~context ~sr:gfs2 >>= fun () ->
                plug_pbds ~context ~sr:gfs2)*)
            repeat 10 (fun () ->
              pool_reboot ~context
            )
            (*do_ha ~context ~sr:gfs2 >>= fun () ->
              undo_ha ~context >>= fun () ->
                detach_sr ~context ~sr:gfs2 >>= fun () ->
                  cluster_host_allowed_operations ~context cluster >>= fun () >
              Lwt.return_unit*)
        )) *)

let () =
  Printexc.record_backtrace true;
  Logs.set_level ~all:true (Some Logs.Debug);
  Bos.OS.Cmd.(run_out Cmd.(v "tput" % "cols") |> to_string)
  |> R.map int_of_string
  |> Logs.on_error_msg ~use:(fun () -> 80)
  |> Format.set_margin;
  Lwt_main.run (main ())
