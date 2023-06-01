(lang dune 3.8)
(name clarke)

(generate_opam_files true)
(version 0.1.0)
(source (github ocurrent/clarke))
(authors "Patrick Ferris <patrick@sirreg.org>")
(maintainers "Patrick Ferris <patrick@sirreg.org>")
(documentation "https://ocaml.ci.dev/documentation")
(license "MIT")

(package
 (name clarke)
 (synopsis "A power monitoring tool")
 (depends
  (ocaml (>= 5.0))
  (eio_luv (>= 0.7))
  variorum
  ptime
  cmdliner
  http
  cohttp-eio
  tls-eio
  lwt_eio
  prometheus
  prometheus-app
  ISO3166
  carbon
  capnp-rpc-lwt
  capnp-rpc-unix
  (capnp (>= 3.5.0))
  (eio (>= 0.9))
  (ezjsonm (>= 1.3.0))
  (fmt (>= 0.9.0))
  (logs (>= 0.7.0))))
