open Xen_api_lwt_unix
type rpc = Rpc.call -> Rpc.response Lwt.t
type session_info =
  { master: string
  ; ctx_rpc : rpc
  ; session_id: API.ref_session
  ; master_ref: API.ref_host
  ; pool_ref: API.ref_pool }
type t

val login : ?max_expiration_retry:int -> ?timeout:float -> uname:string -> pwd:string -> string -> t Lwt.t

val rpc : t -> (session_info -> rpc:rpc -> session_id:API.ref_session -> 'a Lwt.t) -> 'a Lwt.t
val rpc' : session_info -> (rpc:rpc -> session_id:API.ref_session -> 'a Lwt.t) -> 'a Lwt.t

type 'a log = 'a Logs_lwt.log
val debug : 'a log
val info : 'a log
val warn : 'a log
val err : 'a log

type host = {name: string; ip: string}
val get_host_pp : session_info -> API.ref_host -> host Lwt.t

module PP : sig
  val dict : (string * string) list Fmt.t
  val rpc_t : Rpc.t Fmt.t
  val feature : API.feature_t Fmt.t
  val records : 'b Fmt.t -> ('a Ref.t * 'b) list Fmt.t
  val features : Features.feature list Fmt.t
  val host : host Fmt.t
end
