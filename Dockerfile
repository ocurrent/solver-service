FROM ocaml/opam:ubuntu-22.04-ocaml-4.14@sha256:6a39f49be26b7c085a4b1cca37920e04f5dcdc648dc96383ead337fb8db245b7 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 6830df559aeecd5f61800392c8f804795e8a47ec && opam update
COPY --chown=opam solver-service.opam solver-service-api.opam solver-worker.opam /src/

COPY --chown=opam \
	ocurrent/current_docker.opam \
	ocurrent/current_github.opam \
	ocurrent/current_gitlab.opam \
	ocurrent/current_git.opam \
	ocurrent/current.opam \
	ocurrent/current_rpc.opam \
	ocurrent/current_slack.opam \
	ocurrent/current_web.opam \
	/src/ocurrent/
COPY --chown=opam \
	ocluster/ocluster-api.opam \
	ocluster/current_ocluster.opam \
	/src/ocluster/
COPY --chown=opam \
	ocaml-dockerfile/dockerfile*.opam \
	/src/ocaml-dockerfile/
WORKDIR /src
RUN opam pin add -yn current_docker.dev "./ocurrent" && \
    opam pin add -yn current_github.dev "./ocurrent" && \
    opam pin add -yn current_gitlab.dev "./ocurrent" && \
    opam pin add -yn current_git.dev "./ocurrent" && \
    opam pin add -yn current.dev "./ocurrent" && \
    opam pin add -yn current_rpc.dev "./ocurrent" && \
    opam pin add -yn current_slack.dev "./ocurrent" && \
    opam pin add -yn current_web.dev "./ocurrent" && \
    opam pin add -yn current_ocluster.dev "./ocluster" && \
    opam pin add -yn dockerfile.dev "./ocaml-dockerfile" && \
    opam pin add -yn dockerfile-opam.dev "./ocaml-dockerfile" && \
    opam pin add -yn ocluster-api.dev "./ocluster"

RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build -p solver-service,solver-service-api,solver-worker @install
RUN opam config exec -- dune install --prefix=/usr/local --destdir=pkg --section=bin --relocatable solver-worker solver-service

FROM ubuntu:22.04
RUN apt-get update && apt-get install docker.io libev4 curl gnupg2 git libsqlite3-dev ca-certificates netbase -y --no-install-recommends
WORKDIR /var/lib/ocluster-worker
ENTRYPOINT ["/usr/local/bin/solver-worker"]
ENV PROGRESS_NO_TRUNC=1
COPY --from=build \
    /src/pkg/usr/local/bin/solver-worker \
    /src/pkg/usr/local/bin/solver-service \
    /usr/local/bin/
