open Bos_setup
open Testarossa
open Lwt.Infix

let ctx = Lwt.new_key ()

(* grey *)
let pp_time = Fmt.(styled `Black string |> styled `Bold)

let msg_of_level = function
  | Logs.App -> Logs_fmt.app_style, ""
  | Debug -> Logs_fmt.debug_style, "debug"
  | Warning -> Logs_fmt.warn_style, "warn"
  | Info -> Logs_fmt.info_style, "info"
  | Error -> Logs_fmt.err_style, "error"

let pp_level ppf level =
  let style, msg = msg_of_level level in
  Fmt.pf ppf "%5a" Fmt.(styled style string) msg

let pp_header ppf (level, header) =
  let now = Debug.gettimestring () in
  let name = "" in
  let module_ = "" in
  let id, task =
    match Lwt.get ctx with
    | Some (id, task) -> id, task
    | None -> 0, ""
  in
  Fmt.pf ppf "[%a%a|%a|%d %s|%s|%s] " pp_time now pp_level level
    Fmt.(option string |> styled Logs_fmt.app_style) header
    id name
    task module_

let setup style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  Context.debug (fun m -> m ~header:"x" "Initialized")

open Cmdliner

let common =
  let docs = Manpage.s_common_options in
  Term.(const setup $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let init () =
  Logs.info (fun m -> m "started")

let () =
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let default_cmd =
    let doc = "a small system-level test framework using Xen-on-Xen" in
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common)),
    Term.info "testarossa" ~version:"v1.2" ~doc ~sdocs ~exits
  in
  Term.exit @@ Term.(eval_choice ~catch:true default_cmd [
    Cmd_init.init ~sdocs ~exits ~common;
    Cmd_lwt.prepare ~sdocs ~exits ~common;
    Cmd_init.list ~sdocs ~exits ~common;
    Cmd_lwt.run ~sdocs ~exits ~common
    ])
(*
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

      Context.debug (fun m -> m "IPs: %a" Fmt.(Dump.list Ipaddr.V4.pp_hum) ips);
      make_pool ~context ?license_server ~license_server_port ips >>= fun master ->
      with_pool ~context master (fun ~context ->
          enable_clustering ~context >>= fun cluster ->
          match iscsi, iqn with
          | Some iscsi, Some iqn ->
            get_gfs2_sr ~context ~iscsi ~iqn >>= fun gfs2 ->
            Context.debug (fun m -> m "got SR");
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
*)
