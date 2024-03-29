FROM ocaml/opam:alpine-3.16-ocaml-5.0 AS build
RUN sudo apk add --update libev-dev linux-headers capnproto-dev m4 pkgconf gmp-dev libtool autoconf automake cmake
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard a078652a295abca9601c6240d6d191fb0abc2b9c && opam update
COPY --chown=opam clarke.opam /src/
WORKDIR /src
RUN opam pin -yn . && opam install -y --deps-only .
ADD --chown=opam . .
RUN git apply static.patch
RUN opam exec -- dune subst
RUN opam config exec -- dune build ./_build/install/default/bin/clarke

FROM alpine:3.16
WORKDIR /var/lib/clarke
ENTRYPOINT ["/usr/local/bin/clarke"]
ENV PROGRESS_NO_TRUNC=1
COPY --from=build /src/_build/install/default/bin/clarke /usr/local/bin/
