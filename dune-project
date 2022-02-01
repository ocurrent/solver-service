(lang dune 2.0)
(name solver-service)
(generate_opam_files true)
(source (github patricoferris/solver-service))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(package
 (name solver-service)
 (synopsis "Choose package versions to test")
 (depends
  (alcotest-lwt :with-test)
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.1))
  yojson
  lwt
  logs
  fmt
  solver-service-api
  conf-libev
  (opam-0install (>= "0.2"))
  (git-unix (>= 3.2.0))
  (capnp-rpc-unix (>= 1.2)))
 (conflicts
  (carton (< 0.4.2))) ; workaround for mirage/ocaml-git#514
)
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
  (solver-service (= :version))))