FROM ocaml/opam:alpine-3.16-ocaml-5.0 AS build
RUN sudo apk add --update libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev libgmp-dev libtool autoconf automake cmake
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 571829bccdd249fd71f4015b772bfc689615f50c && opam update
COPY --chown=opam clarke.opam /src/
WORKDIR /src
RUN opam pin -yn . && opam install -y --deps-only .
ADD --chown=opam . .
RUN git apply static.patch
RUN opam exec -- dune subst
RUN opam config exec -- dune build ./_build/install/default/bin/clarke

FROM alpine-3.16
RUN apk add --update docker.io libev4 curl gnupg2 git libsqlite3-dev ca-certificates netbase
WORKDIR /var/lib/clarke
ENTRYPOINT ["/usr/local/bin/clarke"]
ENV PROGRESS_NO_TRUNC=1
COPY --from=build /src/_build/install/default/bin/clarke /usr/local/bin/