open Cmdliner
open Testarossa
open Cmd_types
open Testarossa
open Lwt.Infix

let handle_rollback t ~rollback =
  match t.physical with
  | Some host ->
      Context.debug (fun m -> m "Logging in to physical host %s" host) ;
      Context.with_login ~uname:t.uname ~pwd:t.pwd host (fun phys ->
          if rollback then Rollback.rollback_pool phys else 
            Context.step phys "ensure pool has snapshot" Rollback.ensure_pool_snapshot)
  | None -> Lwt.return_unit


let sync t =
  Xen_api_lwt_unix.(Context.(step t "Sync pool database" (fun ctx -> rpc ctx Pool.sync_database)))


let do_clear_crashdumps t =
  let open Xen_api_lwt_unix in
  let open Context in
  step t "Clearing crashdumps" @@ fun ctx ->
  rpc ctx @@ Host_crashdump.get_all >>=
  Lwt_list.iter_p (fun self  ->
  rpc ctx @@ Host_crashdump.destroy ~self)

let do_rollback conf =
  handle_rollback conf ~rollback:true
  >>= fun () ->
  let master = List.hd conf.hosts |> Ipaddr.V4.to_string in
  Context.with_login ~uname:conf.uname ~pwd:conf.pwd master (fun t ->
      Context.step t "Wait for all hosts to be enabled" @@ fun ctx ->
      Test_sr.wait_enabled ctx ())

let do_prepare conf ~rollback ~clear_crashdumps =
  handle_rollback conf ~rollback
  >>= fun () -> Test_sr.make_pool ~uname:conf.uname ~pwd:conf.pwd conf conf.hosts
  >>= fun t -> Test_sr.optimize_vms t
  >>= fun () -> sync t
  >>= fun () ->
  (if clear_crashdumps then do_clear_crashdumps t
  else Lwt.return_unit)
  >>= fun () ->
  License.maybe_apply_license_pool t conf [Features.HA; Features.Corosync]
  >>= fun () ->
  Test_sr.enable_clustering t
  >>= fun _cluster ->
  match conf.iscsi with
  | Some iscsi ->
      Test_sr.get_gfs2_sr t ~iscsi ?iqn:conf.iqn ?scsiid:conf.scsiid () >>= fun _gfs2 -> Lwt.return_unit
  | _ -> Lwt.return_unit

let lwt_main config f =
  Lwt_main.run (
  Lwt_io.with_file ~mode:Lwt_io.input config (fun ch -> Lwt_io.read ch)
  >>= fun conf -> Jsonrpc.of_string conf |> Cmd_types.t_of_rpc |> f
  );
  let errs = Logs.err_count () in
  let warns = Logs.warn_count () in
  if errs > 0 || warns > 0 then (
    Logs.info (fun m -> m "Exiting with error due to errors(%d) and warnings(%d)" errs warns) ;
    exit 2 ) 

let config =
  let doc = "Load configuration from this file" in
  Arg.(required & opt (some path) None & info ["config"] ~doc)


let rollback =
  let doc = "Rollback pool" in
  Arg.(value & flag & info ["rollback"] ~doc)

let clear_crashdumps =
  let doc = "Clear crashdumps" in
  Arg.(value & flag & info ["clear-crashdumps"] ~doc)

let skip_serial =
  let doc = "Skip serial tests and go straight to the parallel ones" in
  Arg.(value & flag & info ["skip-serial"] ~doc)

let prepare ~common ~sdocs ~exits =
  let doc = "Prepare test environment (setup pool, snapshot pool)" in
  let main () config rollback clear_crashdumps =
    lwt_main config (do_prepare ~rollback ~clear_crashdumps) in
  ( Term.(const main $ common $ config $ rollback $ clear_crashdumps)
  , Term.info "prepare" ~doc ~sdocs ~exits )

let rollback ~common ~sdocs ~exits =
  let doc = "Rollback to snapshot" in
  let main () config = lwt_main config do_rollback in
  Term.(const main $ common $ config), Term.info "rollback" ~doc ~sdocs ~exits

let tests =
  let doc = "List of tests to run (default: all)" in
  Arg.(value & pos_all string [] & info [] ~docv:"TEST" ~doc)

let run_tests conf skip_serial tests =
  let available =
    Allowed_ops.tests
    |> List.map (fun (module M : Allowed_ops.S) -> (M.name, (module M : Allowed_ops.S)))
    |> Astring.String.Map.of_list
  in
  let tests =
    match tests with
    | [] -> Allowed_ops.tests
    | only -> List.map (fun name -> Astring.String.Map.get name available) only
  in
  Test_sr.make_pool ~uname:conf.uname ~pwd:conf.pwd conf conf.hosts
  >>= fun t ->
  Lwt_list.iter_s
    (fun (module M : Allowed_ops.S) ->
      Logs.info (fun m -> m "Executing %s test" M.name) ;
      M.execute ~skip_serial t )
    tests


let run ~common ~sdocs ~exits =
  let doc = "Run tests in a prepared environment" in
  let main () config skip_serial tests = lwt_main config (fun conf -> run_tests conf skip_serial tests) in
  (Term.(const main $ common $ config $ skip_serial $ tests), Term.info "run" ~doc ~sdocs ~exits)

let bonding ~common ~sdocs ~exits =
  let doc = "Run bonding tests" in
  let main () config = lwt_main config Cmd_bonding.run in
  Term.(const main $ common $ config), Term.info "bonding" ~doc ~sdocs ~exits

let deployment ~common ~sdocs ~exits =
  let doc = "Configure deployment, takes a xenrt deployment JSON as input" in
  let main () = Lwt_main.run (Testarossa.Deployment.run ()) in
  Term.(const main $ common), Term.info "deployment" ~doc ~sdocs ~exits
