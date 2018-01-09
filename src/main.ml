open Bos_setup
open Testarossa
open Lwt.Infix



let main =          
  with_default_session (fun ~rpc ~session_id ->
      find_templates ~rpc ~session_id ~name:"XenServer" >>= fun lst ->
      Lwt_list.iter_p (make_xenserver_template ~rpc ~session_id) lst
      )

let () =
  Logs.set_level ~all:true (Some Logs.Debug);
  Lwt_main.run main
