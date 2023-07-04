(lang dune 3.7)
(name solver-service)

(generate_opam_files true)
(source (github ocurrent/solver-service))
(authors "Thomas Leonard <talex5@gmail.com>" "Patrick Ferris <patrick@sirref.org>")
(maintainers "alpha@tarides.com" "Tim McGilchrist <timmcgil@gmail.com>")

(package
 (name solver-service)
 (synopsis "Choose package versions to test")
 (depends
  (ocaml (>= 4.14.1))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  ; Examples dependencies
  (current_web (and (>= 0.6.4) :with-test))
  (current_git (and (>= 0.6.4) :with-test))
  (current_github (and (>= 0.6.4) :with-test))
  (current_ocluster (and (>= 0.2.1) :with-test))
  (ocluster-api (and (>= 0.2.1) :with-test))
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))
  (yojson (>= 2.1.0))
  (lwt (>= 5.6.1))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (ocaml-version (>= 3.6.1))
  (solver-service-api (= :version))
  (dune-build-info (>= 3.8.0))
  conf-libev
  (opam-0install (>= 0.4.3))
  (git-unix (>= 3.12.0))
  (capnp-rpc-unix (>= 1.2.3)))
 (conflicts (carton (< 0.4.2))))

(package
 (name solver-service-api)
 (synopsis "Cap'n Proto API for the solver service")
 (depends
  (ocaml (>= 4.14.1))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (capnp (>= 3.5.0))
  (capnp-rpc-lwt (>= 1.2.3))
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))))

(package
 (name solver-worker)
 (synopsis "OCluster worker that can solve opam constraints")
 (depends
  (ocaml (>= 4.14.1))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (current_ocluster (and (>= 0.2.1) :with-test))
  (ocluster-api (>= 0.2.1))
  (prometheus-app (>= 1.2))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (dune-build-info (>= 3.7))
  (solver-service (= :version))))
