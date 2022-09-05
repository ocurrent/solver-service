FROM ocaml/opam:debian-11-ocaml-4.14@sha256:70bd0da74f68550d667cc1a8ae80661e694cd9ac614273b23c45f3fb8a9f3ac9 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev libffi-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev graphviz -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && git reset --hard ed966e5e82880cba7c483d3a5c1c8d4328c4f0c1 && opam update
WORKDIR /src
COPY --chown=opam solver-service.opam solver-service-api.opam \
	solver-worker.opam /src/
RUN opam-2.1 install -y --deps-only .
ADD --chown=opam . .
RUN opam-2.1 exec -- dune build ./_build/install/default/bin/solver-worker

FROM debian:11
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase -y --no-install-recommends
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb [arch=amd64] https://download.docker.com/linux/debian buster stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce -y --no-install-recommends
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/solver-worker"]
ENV OCAMLRUNPARAM=a=2
# Enable experimental for docker manifest support
ENV DOCKER_CLI_EXPERIMENTAL=enabled
COPY --from=build /src/_build/install/default/bin/solver-worker /usr/local/bin/
