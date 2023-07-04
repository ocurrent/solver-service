FROM ocaml/opam:debian-12-ocaml-4.14@sha256:45b04e2a4c933c57549382045dfac12cb7e872cace0456f92f4b022066e48111 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 68755fe0f8ed7cc42b6403f77d0f7c01b4a6133b && opam update
COPY --chown=opam solver-service.opam solver-service-api.opam solver-worker.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build -p solver-service,solver-service-api,solver-worker @install
RUN opam config exec -- dune install --prefix=/usr/local --destdir=pkg --section=bin --relocatable solver-worker solver-service

FROM debian:12
RUN apt-get update && apt-get install libev4 curl git libsqlite3-dev ca-certificates netbase -y --no-install-recommends
WORKDIR /var/lib/ocluster-worker
ENTRYPOINT ["/usr/local/bin/solver-worker"]
ENV PROGRESS_NO_TRUNC=1
COPY --from=build \
    /src/pkg/usr/local/bin/solver-worker \
    /src/pkg/usr/local/bin/solver-service \
    /usr/local/bin/
