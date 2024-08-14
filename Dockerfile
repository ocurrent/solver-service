FROM ocaml/opam:debian-12-ocaml-5.2 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev libzstd-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 13acffc3de9c22953d1e08bad3e56ee6e965eeed && opam update
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
COPY --chown=opam solver-service.opam solver-service-api.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build @install
RUN opam exec -- dune install --prefix=/usr/local --destdir=pkg --section=bin --relocatable solver-service

FROM debian:12
RUN apt-get update && apt-get install libev4 curl git libsqlite3-0 ca-certificates netbase -y --no-install-recommends
WORKDIR /var/lib/ocluster-worker
ENTRYPOINT ["/usr/local/bin/solver-service"]
COPY --from=build \
    /src/pkg/usr/local/bin/solver-service \
    /usr/local/bin/
