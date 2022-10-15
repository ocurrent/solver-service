FROM ocaml/opam:ubuntu-22.04-ocaml-4.14@sha256:1d783d4caa30a9e2913d05aa0e80f803af02f04e3124d38a148c92a3f9ac1bbc AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard ed966e5e82880cba7c483d3a5c1c8d4328c4f0c1 && opam update
COPY --chown=opam solver-service.opam solver-service-api.opam solver-worker.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune subst
RUN opam config exec -- dune build ./_build/install/default/bin/solver-service
RUN opam config exec -- dune build ./_build/install/default/bin/solver-worker

FROM ubuntu:22.04
RUN apt-get update && apt-get install docker.io libev4 curl gnupg2 git libsqlite3-dev ca-certificates netbase -y --no-install-recommends
WORKDIR /var/lib/ocluster-worker
ENTRYPOINT ["/usr/local/bin/solver-worker"]
ENV PROGRESS_NO_TRUNC=1
COPY --from=build /src/_build/install/default/bin/solver-worker /src/_build/install/default/bin/solver-service /usr/local/bin/
