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
  let id, task = match Lwt.get ctx with Some (id, task) -> (id, task) | None -> (0, "") in
  Fmt.pf ppf "[%a%a|%a|%d %s|%s|%s] " pp_time now pp_level level
    Fmt.(option string |> styled Logs_fmt.app_style)
    header id name task module_

let setup () =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  Logs.set_level ~all:true (Some Logs.Debug);
  Format.set_margin 100;
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ())

let bracket configfile f =
  Testarossa.Deployment.of_file configfile >>= fun config ->
  Testarossa.Deployment.ensure_vhosts_on config >>= fun () ->
  Lwt.finalize f (fun () -> Testarossa.Deployment.check_vhost_crashdumps config)

let suite = [
]

let () =
  setup ();
  let open Cmdliner in
  let config =
    let doc = "Load configuration from this file" in
    Arg.(value & opt file "config.json" & info ["config"] ~doc) in
  Alcotest.run_with_args "testarossa" config suite
