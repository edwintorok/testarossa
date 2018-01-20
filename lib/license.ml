open Xen_api_lwt_unix
open Context

type t = {
  server : string;
  edition: string;
  port: int;
}

let edition_key = "edition"

let pp_feature = Fmt.using API.rpc_of_feature_t PP.rpc_t

let info f = lift Logs_lwt.info f

let ensure_pool info require_features =
  debug (fun m -> m "listing features") >>= fun () ->
  rpc Feature.get_all >>= fun features ->

  lift_lwt' Lwt_list.map_p (fun feature ->
    rpc (Feature.get_enabled ~self:feature) >>= fun is_enabled ->
    debug (fun m ->
      rpc (Feature.get_name_label ~self:feature) >>= fun name ->
      m "feature %s: %b" name is_enabled) >>= fun () ->
    return (feature, is_enabled)
  ) features >>= fun enabled ->

  rpc (Pool.get_license_state ~self:info.pool_ref) >>= fun state ->
  Logs.debug (fun m -> m "license state: %a" PP.dict state) >>= fun () ->
  List.mem_assoc edition_key state && List.assoc edition_key state


