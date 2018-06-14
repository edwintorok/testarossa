open Testarossa
open Lwt.Infix

let ctx = Lwt.new_key ()

let pp_time = Fmt.(styled `Black string |> styled `Bold)

let msg_of_level = function
  | Logs.App -> (Logs_fmt.app_style, "")
  | Debug -> (Logs_fmt.debug_style, "debug")
  | Warning -> (Logs_fmt.warn_style, "warn")
  | Info -> (Logs_fmt.info_style, "info")
  | Error -> (Logs_fmt.err_style, "error")


let pp_level ppf level =
  let style, msg = msg_of_level level in
  Fmt.pf ppf "%5a" Fmt.(styled style string) msg


let pp_header ppf (level, header) =
  let now = Debug.gettimestring () in
  let name = "" in
  let module_ = "" in
  let id, task =
    match Lwt.get ctx with Some (id, task) -> (id, task) | None -> (0, "")
  in
  Fmt.pf ppf "[%a%a|%a|%d %s|%s|%s] " pp_time now pp_level level
    Fmt.(option string |> styled Logs_fmt.app_style)
    header id name task module_


let setup () =
  Printexc.record_backtrace true ;
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty () ;
  Logs.set_level ~all:true (Some Logs.Debug) ;
  Format.set_margin 100 ;
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ())


let bracket name f =
  Alcotest_lwt.test_case name `Slow (fun _ configfile ->
      Deployment.of_file configfile
      >>= fun config ->
      Deployment.ensure_vhosts_on config
      >>= fun () ->
      Lwt.finalize
        (fun () -> f config)
        (fun () -> Deployment.check_vhost_crashdumps config) )


let destroy_pools conf =
  Deployment.forall_virtual conf "destroy pool" Test_sr.destroy_pool

let clear_crashes conf =
  let open Xen_api_lwt_unix in
  Deployment.forall_virtual conf "delete crashdumps" (fun ctx ->
      Context.rpc ctx @@ Host_crashdump.get_all >>=
      Lwt_list.iter_p (fun self ->
          Context.rpc ctx @@ Host_crashdump.destroy ~self))

let clustering conf =
  Deployment.with_virtual_master conf "Enable clustering" (fun ctx ->
      Test_sr.enable_clustering ctx
      >>= fun _cluster ->
      Lwt.return_unit
  )

let reboot conf =
  let open Context in
  let open Xen_api_lwt_unix in
  Deployment.with_virtual_master conf "Reboot pool" (fun ctx ->
      rpc ctx Host.get_all >>= fun hosts ->
      Lwt_list.iter_p (fun host -> rpc ctx @@ Host.disable ~host) hosts
      >>= fun () ->
      (* TODO: slaves first then master *)
      Lwt_list.iter_p (fun host -> rpc ctx @@ Host.reboot ~host) hosts)

let suite = [
  "basic", [
      bracket "clear crashdumps" clear_crashes
    ; bracket "destroy pools" destroy_pools
    ; bracket "enable clustering" clustering
    ; bracket "reboot all hosts" reboot
  ]
]

let () =
  setup () ;
  let open Cmdliner in
  let config =
    let doc = "Load configuration from this file" in
    Arg.(value & opt file "config.json" & info ["config"] ~doc)
  in
  Alcotest.run_with_args "testarossa" config suite
