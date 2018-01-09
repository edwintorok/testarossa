open Xen_api_lwt_unix

let src = Logs.Src.create "testarossa" ~doc:"logs testarossa events"
module Log = (val Logs.src_log src : Logs.LOG)

type rpc = Rpc.call -> Rpc.response Lwt.t
type vm = API.ref_VM * API.vM_t
type vm_list = vm list                         

module PP = struct               
  let vm_uuid = Fmt.(using (fun vmr -> vmr.API.vM_uuid) string)
  let vm_name_label = Fmt.(using (fun vmr -> vmr.API.vM_name_label) string)
  let vm_record ppf vmr =
    Fmt.pf ppf "%a(%a)" vm_uuid vmr vm_name_label vmr
end           

let find_templates ~rpc ~session_id ~name =
  (* get_all_records_where doesn't seem to be able to filter on name_label/name-label *)
  VM.get_by_name_label ~rpc ~session_id ~label:name >>=
    Lwt_list.filter_map_p (fun vm_ref ->
        VM.get_record ~rpc ~session_id ~self:vm_ref >>= fun vmr ->
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

let make_xenserver_template ~rpc ~session_id (template_ref, template_rec) =
  update_map (VM.set_platform ~rpc ~session_id ~self:template_ref)
    template_rec.API.vM_platform [
      "nic_type", "e1000";
      "exp-nested-hvm", "1";
    ]

let with_session ~uname ~pwd ~machine f =
  let rpc = make (Printf.sprintf "https://%s" machine) in
  Logs.debug (fun m -> m "Logging in to %s with user %s" machine uname);
  Session.login_with_password ~rpc ~uname ~pwd ~version:"1.1" ~originator:"testarossa" >>= fun session_id ->
  Lwt.finalize (fun () -> f ~rpc ~session_id)
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
