(lang dune 2.0)
(name solver-service)
(generate_opam_files true)
(source (github ocurrent/solver-service))
(authors "talex5@gmail.com" "patrick@sirref.org")
(maintainers "tim@tarides.com" "alpha@tarides.com")
(package
 (name solver-service)
 (synopsis "Choose package versions to test")
 (depends
  (alcotest-lwt :with-test)
  ; Examples dependencies
  (current_web :with-test)
  (current_github :with-test)
  (current_ocluster :with-test)
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))
  yojson
  lwt
  logs
  fmt
  ocaml-version
  solver-service-api
  conf-libev
  (opam-0install (>= "0.2"))
  (git-unix (>= 3.2.0))
  (capnp-rpc-unix (>= 1.2)))
 (conflicts (carton (< 0.4.2))))
(package
 (name solver-service-api)
 (synopsis "Cap'n Proto API for the solver service")
 (depends
  (alcotest-lwt :with-test)
  current_rpc
  (capnp (>= 3.4.0))
  capnp-rpc-lwt
  ppx_deriving
  ppx_deriving_yojson))
(package
 (name solver-worker)
 (synopsis "An OCluster worker that can solve opam constraints")
 (depends
  (ocaml (>= "4.14.0"))
  (alcotest-lwt (and (>= 1.5.0) :with-test))
  ocluster-api
  current
  (prometheus-app (>= 1.2))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (dune-build-info (>= 2.9.1))
  (solver-service (= :version))))
