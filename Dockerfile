FROM ocaml/opam:debian-11-ocaml-5.0 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev libgmp-dev libtool autoconf automake cmake -y --no-install-recommends
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 571829bccdd249fd71f4015b772bfc689615f50c && opam update
COPY --chown=opam clarke.opam /src/
WORKDIR /src
RUN opam pin -yn . && opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune subst
RUN opam config exec -- dune build ./_build/install/default/bin/clarke

FROM ubuntu:22.04
RUN apt-get update && apt-get install docker.io libev4 curl gnupg2 git libsqlite3-dev ca-certificates netbase -y --no-install-recommends
WORKDIR /var/lib/clarke
ENTRYPOINT ["/usr/local/bin/clarke"]
ENV PROGRESS_NO_TRUNC=1
COPY --from=build /src/_build/install/default/bin/clarke /usr/local/bin/