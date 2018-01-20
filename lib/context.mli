open Xen_api_lwt_unix
type rpc = Rpc.call -> Rpc.response Lwt.t
type session_info =
  { master: string
  ; session_id: API.ref_session
  ; master_ref: API.ref_host
  ; pool_ref: API.ref_pool }
type +'a t
val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val lift : ('a -> 'b Lwt.t) -> 'a -> 'b t
val rpc : (rpc:rpc -> session_id:API.ref_session -> 'a Lwt.t) -> 'a t
val lift_lwt : (('a -> 'b Lwt.t) -> 'c Lwt.t) -> ('a -> 'b t) -> 'c t
val lift_lwt' : (('a -> 'b Lwt.t) -> 'c -> 'd Lwt.t) -> ('a -> 'b t) -> 'c -> 'd t
val liftF : (session_info -> 'a -> 'b t) -> 'a -> 'b t

type 'a log = ('a, unit t) Logs.msgf -> unit t
val debug : 'a log
val info : 'a log
val warn : 'a log
val err : 'a log

module PP : sig
  val dict : (string * string) list Fmt.t
  val rpc_t : Rpc.t Fmt.t
end
