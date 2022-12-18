(* A Capnp Server that accepts machines reporting their
   monitoring information *)
open Clarke

module Store = struct
  let add_raw_string ~machine ~msg =
    let v = Info.of_json msg in
    Eio.traceln "Machine: %s, Info: %a" machine Info.pp v
end

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server ~sw net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore =
    Capnp_rpc_net.Restorer.single service_id
      (Capnp_client.Reporter.local Store.add_raw_string)
  in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let run_server ~sw net =
  let uri = start_server ~sw net in
  Eio.traceln "Server address: %a" Uri.pp uri;
  Ok ()

open Cmdliner

let cmd setup_log ~net =
  let main () = Eio.Switch.run @@ fun sw -> run_server ~sw net in
  let info = Cmd.info "serve" in
  Cmd.v info Term.(const main $ setup_log)
