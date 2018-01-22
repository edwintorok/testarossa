module PP : sig 
  val vm_uuid : API.vM_t Fmt.t
  val vm_name_label : API.vM_t Fmt.t
  val vm_record : API.vM_t Fmt.t
end

type rpc = Rpc.call -> Rpc.response Lwt.t
type vm = API.ref_VM * API.vM_t
type vm_list = vm list

type context

val with_session :
  uname:string ->
  pwd:string ->
  machine:string ->
  (context:context -> 'a Lwt.t) -> 'a Lwt.t
val with_default_session : (context:context -> 'a Lwt.t) -> 'a Lwt.t


val find_templates : context:context -> name:string -> vm_list Lwt.t
val find_vm : context:context -> name:string -> vm_list Lwt.t
val find_vms : context:context -> vm_list Lwt.t
val make_xenserver_template : context:context -> vm -> unit Lwt.t

val call_rpc : context:context -> (rpc:rpc -> session_id:API.ref_session -> 'a) -> 'a

val ssh: context:context -> Bos.Cmd.t -> string list Lwt.t

module Ipset: Set.S with type elt = Ipaddr.V4.t
val get_arp_map : context:context -> Ipaddr.V4.t Astring.String.Map.t Lwt.t

val get_vm_ips : context:context -> vm_list -> (vm * Ipset.t) list Lwt.t

val make_pool : context:context -> ?license_server:string -> ?license_server_port:int -> Ipaddr.V4.t list -> Ipaddr.V4.t Lwt.t

val with_pool: context:context -> Ipaddr.V4.t -> (context:context -> 'a Lwt.t) -> 'a Lwt.t

val enable_clustering : context:context -> API.ref_Cluster Lwt.t

val cluster_host_allowed_operations : context:context -> API.ref_Cluster -> unit Lwt.t

val create_gfs2_sr : context:context -> iscsi:Ipaddr.V4.t -> iqn:string -> API.ref_SR Lwt.t
val get_gfs2_sr : context:context -> iscsi:Ipaddr.V4.t -> iqn:string -> API.ref_SR Lwt.t
val do_ha : context:context -> sr:API.ref_SR -> unit Lwt.t
val undo_ha : context:context -> unit Lwt.t
val unplug_pbds : context:context -> sr:API.ref_SR -> unit Lwt.t
val plug_pbds : context:context -> sr:API.ref_SR -> unit Lwt.t
val detach_sr : context:context -> sr:API.ref_SR -> unit Lwt.t

val repeat : int -> (unit -> unit Lwt.t) -> unit Lwt.t

val pool_reboot: context:context -> unit Lwt.t
