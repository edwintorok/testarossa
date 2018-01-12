open Bos_setup
open Testarossa
open Lwt.Infix

let ip = OS.Arg.conv ~docv:"IP" (fun s ->
             R.trap_exn Ipaddr.V4.of_string_exn s |>
             R.error_exn_trap_to_msg) Ipaddr.V4.pp_hum

let iscsi = OS.Arg.(opt ~docv:"iscsi" ["iscsi"] ~absent:None (some ip))
let iqn = OS.Arg.(opt ~docv:"iqn" ["iqn"] ~absent:None (some string))

let license_server = OS.Arg.(opt ["license-server-address"] ~absent:None (some string))
let license_server_port = OS.Arg.(opt ["license-server-port"] int ~absent:27000)

let main =
  let ips = OS.Arg.(parse ~pos:ip ()) in
  with_default_session (fun ~context ->
      find_templates ~context ~name:"XenServer" >>= fun lst ->
      Lwt_list.iter_p (make_xenserver_template ~context) lst >>= fun () ->
      find_vm ~context ~name:Sys.argv.(1) >>= fun vms ->

      Logs.debug (fun m -> m "IPs: %a" Fmt.(Dump.list Ipaddr.V4.pp_hum) ips);
      make_pool ~context ips >>= fun master ->
      with_pool ~context master (fun ~context ->
          enable_clustering ~context >>= fun cluster ->
          match iscsi, iqn with
          | Some iscsi, Some iqn ->
             get_gfs2_sr ~context ~iscsi ~iqn >>= fun gfs2 ->
             Logs.debug (fun m -> m "got SR");
             match license_server with
             | None -> Lwt.return_unit
             | Some license_server ->
                license_hosts ~context ~license_server ~license_server_port >>= fun () ->
                do_ha ~context ~sr:gfs2 >>= fun () ->
                Lwt.return_unit
          | _ ->
             cluster_host_allowed_operations ~context cluster
        ))

let () =
  Printexc.record_backtrace true;
  Logs.set_level ~all:true (Some Logs.Debug);
  Bos.OS.Cmd.(run_out Cmd.(v "tput" % "cols") |> to_string)
  |> R.map int_of_string
  |> Logs.on_error_msg ~use:(fun () -> 80)
  |> Format.set_margin;
  Lwt_main.run main