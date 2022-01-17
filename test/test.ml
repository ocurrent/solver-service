let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "solver-service" [ ("service", Test_service.tests) ]
