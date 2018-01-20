open Xen_api_lwt_unix

let src =
  Logs.Src.create "testarossa" ~doc:"logs testarossa events"


module Log = (val (Logs_lwt.src_log src : (module Logs_lwt.LOG)))

let version = "1.1"

let originator = "testarossa"

type rpc = Rpc.call -> Rpc.response Lwt.t

type session_info =
  { master: string
  ; session_id: API.ref_session
  ; master_ref: API.ref_host
  ; pool_ref: API.ref_pool }

type env =
  {rpc: rpc; session: session_info Singleton.t; max_expiration_retry: int}

let login ?(max_expiration_retry= 3) ?(timeout= 5.0) ~uname ~pwd ~master =
  let open Lwt.Infix in
  let rpc = make_json ~timeout ("https://" ^ master) in
  let login () =
    Log.debug (fun m -> m "Logging in to %s as %s" master uname)
    >>= fun () ->
    Session.login_with_password ~rpc ~uname ~pwd ~version ~originator
    >>= fun session_id ->
    Pool.get_all ~rpc ~session_id
    >>= function
      | [pool_ref] ->
          Pool.get_master ~rpc ~session_id ~self:pool_ref
          >>= fun master_ref ->
          Lwt.return {master; session_id; master_ref; pool_ref}
      | [] ->
          Log.err (fun m -> m "No pools found on %s" master)
          >>= fun () -> Lwt.fail_with "No pools found"
      | _ :: _ :: _ ->
          Log.err (fun m -> m "Too many pools on %s" master)
          >>= fun () -> Lwt.fail_with "Too many pools"
  in
  let r = {rpc; max_expiration_retry; session= Singleton.create login} in
  Singleton.get r.session
  >>= fun info ->
  Log.debug (fun m ->
      let session_id = info.session_id in
      Session.get_uuid ~rpc:r.rpc ~session_id ~self:session_id
      >>= fun uuid -> m "Got session id %s" uuid )
  >>= fun () -> Lwt.return r


type +'a t = env -> 'a Lwt.t

(* call [f ~rpc ~session_id] and retry by logging in again on [Api_errors.session_invalid] *)
let rpc f e =
  let open Lwt.Infix in
  let rec retry = function
    | n when n > e.max_expiration_retry ->
        Logs.debug (fun m -> m "Maximum number of retries exceeded") ;
        Lwt.fail_with "Maximum number of retries exceeded"
    | n ->
        Singleton.get e.session
        >>= fun session ->
        Lwt.catch
          (fun () -> f ~rpc:e.rpc ~session_id:session.session_id)
          (function
              | Api_errors.Server_error (code, _)
                when code = Api_errors.session_invalid ->
                  Log.debug (fun m -> m "Session is not valid (try #%d)!" n)
                  >>= fun () ->
                  Singleton.invalidate e.session session ;
                  retry (n + 1)
              | e ->
                  Lwt.fail e)
  in
  retry 0


let lift f x _ = f x

let return x = lift Lwt.return x

let ( >>= ) m f e = Lwt.bind (m e) (fun r -> f r e)
let liftF f x e =
  Lwt.bind (Singleton.get e.session) (fun r -> f r x e)


let lift_lwt f g e =
  let g' v = g v e in
  f g'

let lift_lwt' f g x e =
  let g' v = g v e in
  f g' x

let return_unit  : unit t = fun _ -> Lwt.return_unit

let check_level level =
  if level = Logs.Error then Logs.incr_err_count () else
    if level = Logs.Warning then Logs.incr_warn_count ()

type 'a log = ('a, unit t) Logs.msgf -> unit t
let msg level msgf = match Logs.Src.level src with
| None -> return_unit
| Some level' when level > level' ->
    check_level level; return_unit
| Some _ ->
    check_level level;
    let (ret, unblock) = Lwt.wait () in
    let k () : unit t = fun _ -> ret in
    let over () = Lwt.wakeup unblock () in
    Logs.report src level ~over k msgf

let debug msgf = msg Logs.Debug msgf
let info msgf = msg Logs.Info msgf
let warn msgf = msg Logs.Warning msgf
let err msgf = msg Logs.Error msgf

module PP = struct
  open Fmt
  let dict = Dump.(pair string string |> list)
  let rpc_t = Fmt.using Jsonrpc.to_string string
end
