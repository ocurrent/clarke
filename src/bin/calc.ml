open Clarke
open Eio
open Cmdliner

let data_file_term =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A file containing JSON values" [ "data" ]

let msg_err = function Error (`Msg s) -> Error s | Ok _ as v -> v

let cmd setup_log fs =
  let main () file =
    Path.(with_open_in (fs / file)) @@ fun flow ->
    let reader = Buf_read.of_flow ~max_size:max_int flow in
    Result.map (Summary.pp Fmt.stdout) (Clarke.Summary.summary reader)
    |> msg_err
  in
  let info = Cmd.info "calc" in
  Cmd.v info Term.(const main $ setup_log $ data_file_term)
