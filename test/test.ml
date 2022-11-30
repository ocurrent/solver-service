let () =
  (* ignore user's git configuration *)
  Unix.putenv "GIT_AUTHOR_NAME" "test";
  Unix.putenv "GIT_COMMITTER_NAME" "test";
  Unix.putenv "EMAIL" "test@example.com";
  Lwt_main.run
  @@ Alcotest_lwt.run "solver-service" [ ("service", Test_service.tests) ]
