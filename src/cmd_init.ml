open Rresult

type ipaddr = Ipaddr.t
let rpc_of_ipaddr t = Ipaddr.to_string t |> Rpc.rpc_of_string
let ipaddr_of_rpc rpc = Rpc.string_of_rpc rpc |> Ipaddr.of_string_exn

type t = {
  iscsi: ipaddr option;
  iqn : string option;
  license_server : string option;
  license_server_port : int;
  license_edition : string option;
  physical: string option;
  uname: string;
  pwd: string
} [@@deriving rpc]

let main () config =
  config |> rpc_of_t |> Jsonrpc.to_string |> prerr_endline

open Cmdliner

let ip =
  let parse s =
    R.trap_exn Ipaddr.of_string_exn s |>
    R.error_exn_trap_to_msg
  in
  let print = Ipaddr.pp_hum in
  Arg.conv ~docv:"IP address" (parse, print)

let iscsi =
  let doc = "Address of iSCSI target" in
  Arg.(value & opt (some ip) None & info ["iscsi"] ~doc)

let iqn =
  let doc = "iSCSI IQN" in
  let docv = "IQN" in
  Arg.(value & opt (some string) None & info ~docv ~doc ["iqn"])

let iqn =
  let doc = "SCSIid" in
  let docv = doc in
  Arg.(value & opt (some string) None & info ~doc ~docv ["scsiid"])

let license_server =
  let doc = "License server address" in
  let docv = "host" in
  Arg.(value & opt (some string) None & info ~doc ~docv ["license-server"])

let license_server_port =
  let doc = "License server port" in
  let docv = "port" in
  Arg.(value & opt int 27000 & info ~doc ~docv ["license-server-port"])

let license_edition =
  let doc = "License edition" in
  let docv = "edition" in
  Arg.(value & opt (some string) None & info ~doc ~docv ["license-edition"])

let physical =
  let doc = "Physical XenServer host containing virtual XenServer hosts" in
  Arg.(value & opt (some string) None & info ["physical"] ~doc)

let uname =
  let doc = "XenServer username" in
  Arg.(required & opt (some string) None & info ["username"] ~doc)

let pwd =
  let doc = "XenServer password" in
  Arg.(required & opt (some string) None & info ["password"] ~doc)

let build iscsi iqn license_server license_server_port license_edition physical uname pwd =
  {
    iscsi; iqn; license_server; license_server_port; license_edition; physical; uname; pwd
  }

let cmd ~common ~sdocs ~exits =
  let doc = "initialize a new test profile" in
  let build = Term.(const build $ iscsi $ iqn $ license_server $ license_server_port $ license_edition $ physical $ uname $ pwd) in
  Term.(const main $ common $ build),
  Term.info "init" ~doc ~sdocs ~exits
