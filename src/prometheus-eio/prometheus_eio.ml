(* Apache License Version 2.0: See the end of the file for the full license. *)
open Prometheus

module Metrics = struct
  let namespace = "prometheus"
  let subsystem = "logs"

  let inc_messages =
    let help = "Total number of messages logged" in
    let c =
      Counter.v_labels ~label_names:[ "level"; "src" ] ~help ~namespace
        ~subsystem "messages_total"
    in
    fun lvl src ->
      let lvl = Logs.level_to_string (Some lvl) in
      Counter.inc_one @@ Counter.labels c [ lvl; src ]
end

module Unix_runtime = struct
  let start_time = Unix.gettimeofday ()

  let simple_metric ~metric_type ~help name fn =
    let info =
      {
        MetricInfo.name = MetricName.v name;
        help;
        metric_type;
        label_names = [];
      }
    in
    let collect () = LabelSetMap.singleton [] [ Sample_set.sample (fn ()) ] in
    (info, collect)

  let process_start_time_seconds =
    simple_metric ~metric_type:Counter "process_start_time_seconds"
      (fun () -> start_time)
      ~help:"Start time of the process since unix epoch in seconds."

  let metrics = [ process_start_time_seconds ]
end

type config = int option

module Server = struct
  module Server = Cohttp_eio.Server

  let callback (req, _, _) =
    let open Http in
    let uri = Request.resource req in
    match (Request.meth req, uri) with
    | `GET, "/metrics" ->
        let data =
          Lwt_eio.Promise.await_lwt
          @@ Prometheus.CollectorRegistry.(collect default)
        in
        let body =
          Cohttp_eio.Body.Fixed
            (Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data)
        in
        let headers =
          Http.Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        (Http.Response.make ~status:`OK ~headers (), body)
    | _ ->
        ( Http.Response.make ~status:`Bad_request (),
          Cohttp_eio.Body.Fixed "Bad request" )
end

let run_server ~port env handler =
  let run_domain ssock handler =
    let on_error exn =
      Printf.fprintf stderr "Error handling connection: %s\n%!"
        (Printexc.to_string exn)
    in
    let handler = Cohttp_eio.Server.connection_handler handler env#clock in
    Eio.Switch.run (fun sw ->
        let rec loop () =
          Eio.Net.accept_fork ~sw ssock ~on_error handler;
          loop ()
        in
        loop ())
  in
  let run ~port env handler =
    Eio.Switch.run @@ fun sw ->
    let ssock =
      Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
        ~backlog:128
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    run_domain ssock handler
  in
  run ~port env handler

let serve env = function
  | None -> fun () -> ()
  | Some port ->
      let callback = Server.callback in
      fun () -> run_server ~port env callback

let listen_prometheus =
  let open! Cmdliner in
  let doc =
    Arg.info ~docs:"MONITORING OPTIONS" ~docv:"PORT"
      ~doc:"Port on which to provide Prometheus metrics over HTTP."
      [ "listen-prometheus" ]
  in
  Arg.(value @@ opt (some int) None doc)

let opts = listen_prometheus

let () =
  let add (info, collector) =
    CollectorRegistry.(register default) info collector
  in
  List.iter add Unix_runtime.metrics

module Logging = struct
  let inc_counter = Metrics.inc_messages

  let pp_timestamp f x =
    let open Unix in
    let tm = localtime x in
    Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let reporter formatter =
    let report src level ~over k msgf =
      let k _ =
        over ();
        k ()
      in
      let src = Logs.Src.name src in
      Metrics.inc_messages level src;
      msgf @@ fun ?header ?tags:_ fmt ->
      Fmt.kpf k formatter
        ("%a %a %a @[" ^^ fmt ^^ "@]@.")
        pp_timestamp (Unix.gettimeofday ())
        Fmt.(styled `Magenta string)
        (Printf.sprintf "%14s" src)
        Logs_fmt.pp_header (level, header)
    in
    { Logs.report }

  let set_level (src, level) =
    let rec aux = function
      | [] ->
          Logs.warn (fun f ->
              f "set_level: logger %S not registered; ignoring" src)
      | x :: _ when Logs.Src.name x = src -> Logs.Src.set_level x (Some level)
      | _ :: xs -> aux xs
    in
    aux (Logs.Src.list ())

  let init ?(default_level = Logs.Info) ?(levels = []) ?(formatter = Fmt.stderr)
      () =
    Fmt_tty.setup_std_outputs ();
    Logs.set_reporter (reporter formatter);
    Logs.set_level (Some default_level);
    List.iter set_level levels
end

(* Largely based on the other implementations of Prometheus servers:


                                 Apache License
                           Version 2.0, January 2004
                        https://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   Copyright 2016-2017 Docker, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       https://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.*)
