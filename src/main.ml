open Bos_setup
open Testarossa
open Lwt.Infix

let main =          
  with_default_session (fun ~rpc ~session_id ->
      find_templates ~rpc ~session_id ~name:"XenServer" >>= fun lst ->
      Lwt_list.iter_p (make_xenserver_template ~rpc ~session_id) lst >>= fun () ->
      find_vm ~rpc ~session_id ~name:Sys.argv.(1) >>= fun vms ->
      ssh "root" "perfuk-01-11.xenrt.citrite.net" Cmd.(v "arp" % "-na") >>= fun _ ->
      Lwt.return_unit
      )

let () =
  Logs.set_level ~all:true (Some Logs.Debug);
  Lwt_main.run main
