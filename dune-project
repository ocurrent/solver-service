(lang dune 3.7)
(name solver-service)

(formatting (enabled_for dune))
(generate_opam_files true)
(source (github ocurrent/solver-service))
(authors "Thomas Leonard <talex5@gmail.com>" "Patrick Ferris <patrick@sirref.org>")
(maintainers "alpha@tarides.com" "Tim McGilchrist <timmcgil@gmail.com>")

(package
 (name solver-service)
 (synopsis "Choose package versions to test")
 (depends
  (ocaml (>= 5.0.0))
  ; Examples dependencies
  (current_web (and (>= 0.6.4) :with-test))
  (current_git (and (>= 0.6.4) :with-test))
  (current_github (and (>= 0.6.4) :with-test))
  (current_ocluster (and (>= 0.2.1) :with-test))
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))
  (yojson (>= 2.1.0))
  (lwt (>= 5.6.1))
  (eio (>= 0.12))
  (eio_main (>= 0.12))
  (lwt_eio (>= 0.5))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (ocaml-version (>= 3.6.1))
  (solver-service-api (= :version))
  (dune-build-info (>= 3.8.0))
  (opam-0install (>= 0.4.4))
  ; Lower bound on opam with performance fixes #6144 #6122
  (opam-core (>= 2.3.0~alpha1))
  (opam-state (>= 2.3.0~alpha1))
  (opam-repository (>= 2.3.0~alpha1))
  (opam-format (>= 2.3.0~alpha1))

  (git-unix (>= 3.12.0))
  (ocluster-api (>= 0.2.1))
  (prometheus-app (>= 1.2))
  (capnp-rpc-net (>= 1.2.3))
  (capnp-rpc-unix (>= 1.2.3)))
 (conflicts (carton (< 0.4.2))))

(package
 (name solver-service-api)
 (synopsis "Cap'n Proto API for the solver service")
 (depends
  (ocaml (>= 4.14.1))
  (capnp (>= 3.5.0))
  (capnp-rpc-lwt (>= 1.2.3))
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))))
