module Api = Carbon_api.MakeRPC (Capnp_rpc_lwt)
open Capnp_rpc_lwt

type t = Api.Client.Reporter.t Capability.t

module Reporter = struct
  let local fn =
    let module Reporter = Api.Service.Reporter in
    Reporter.local
    @@ object
         inherit Reporter.service

         method report_impl params release_param_caps =
           let open Reporter.Report in
           let machine = Params.machine_get params in
           let msg = Params.msg_get params in
           release_param_caps ();
           fn ~machine ~msg;
           let response, results =
             Service.Response.create Results.init_pointer
           in
           Results.reply_set results "success";
           Service.return response
       end

  let report t machine msg =
    let open Api.Client.Reporter in
    let request, params =
      Capability.Request.create Report.Params.init_pointer
    in
    Report.Params.machine_set params machine;
    Report.Params.msg_set params msg;
    Capability.call_for_unit t Report.method_id request
end

let report ~machine t info =
  let msg = Info.to_json info in
  match Reporter.report t machine msg with
  | Ok () -> Ok ()
  | Error (`Capnp cap) -> Error (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp cap))
