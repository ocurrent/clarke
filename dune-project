(lang dune 3.7)
(name clarke)

(generate_opam_files true)
(version 0.1.0)
(source (github ocurrent/clarke))
(authors "Patrick Ferris <patrick@sirreg.org>" "Ben Andrew <benmandrew@gmail.com>" "Tim McGilchrist <timmcgil@gmail.com>")
(maintainers "Patrick Ferris <patrick@sirreg.org>" "Ben Andrew <benmandrew@gmail.com>" "Tim McGilchrist <timmcgil@gmail.com>")
(documentation "https://github.com/ocurrent/clarke#readme")
(license "MIT")

(package
 (name clarke)
 (synopsis "A power monitoring tool")
 (depends
  (ocaml (>= 5.0))
   variorum
   ptime
   cmdliner
   http
   cohttp-eio
   tls-eio
   lwt_eio
  (prometheus (>= 1.2))
   prometheus-app
   ISO3166
   carbon
   capnp-rpc-lwt
   capnp-rpc-unix
  (capnp (>= 3.5.0))
  (eio (>= 0.10))
  (eio_main (>= 0.10))
  (ezjsonm (>= 1.3.0))
  (fmt (>= 0.9.0))
  (logs (>= 0.7.0))))
