open Eio.Std
open Utils

let () = Printexc.record_backtrace true

let debian_12_ocaml_5 =
  { Solver_service_api.Worker.Vars.
    arch = "x86_64";
    os = "linux";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "12";
    ocaml_package = "ocaml-base-compiler";
    ocaml_version = "5.0";
    opam_version = "2.1.3";
    lower_bound = false;
  }

let platforms = [
  "debian-12-ocaml-5", debian_12_ocaml_5;
]

let test_simple t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let root_pkgs = ["app.dev", {| depends: [ "foo" ] |}] in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1.0", {| depends: [ "ocaml-base-compiler" ] |};
  ]
  in
  solve t "Select foo.1.0" ~platforms ~root_pkgs ~commits:[opam_repo, opam_packages];
  solve t "Foo 1.1 now available" ~platforms ~root_pkgs ~commits:[
    opam_repo, (("foo.1.1", "") :: opam_packages)
  ];
  (* Doesn't do another git-fetch, and doesn't use latest commit. *)
  solve t "Retry with previous commit" ~platforms ~root_pkgs ~commits:[opam_repo, opam_packages];
  ()

let test_overlay t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let overlay_repo = Opam_repo.create "overlay.git" in
  let root_pkgs = ["app.dev", {| depends: [ "foo" ] |}] in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1.0", {| depends: [ "ocaml-base-compiler" { >= "5.1" } ] |};
  ] in
  let overlay_packages = [
    "foo.1.0", {| depends: [ "ocaml-base-compiler" { = "5.0" } ] |};
  ] in
  solve t "Fails without overlay" ~platforms ~root_pkgs ~commits:[
    opam_repo, opam_packages;
  ];
  solve t "Selects 5.0 with overlay" ~platforms ~root_pkgs ~commits:[
    opam_repo, opam_packages;
    overlay_repo, overlay_packages;
  ];
  solve t "Overlay not needed" ~platforms ~root_pkgs:["app.dev", ""] ~commits:[
    opam_repo, opam_packages;
    overlay_repo, overlay_packages;
  ];
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1.0", "";
  ] in
  let overlay_packages = [
    "foo.1.1", {| depends: [ "ocaml-base-compiler" { = "6.0" } ] |};
  ] in
  solve t "Non-overlaid versions still visible" ~platforms ~root_pkgs ~commits:[
    opam_repo, opam_packages;
    overlay_repo, overlay_packages;
  ]

let test_lower_bound t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let root_pkgs = ["app.dev", {| depends: [ "foo" ] |}] in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1.0", "";
    "foo.1.1", "";
  ] in
  let platforms = platforms @ [
    "lower-bound", {debian_12_ocaml_5 with lower_bound = true}
  ] in
  solve t "Selects foo 1.0 for lower-bound" ~platforms ~root_pkgs ~commits:[opam_repo, opam_packages]

let test_no_solution t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let root_pkgs = ["app.dev", {| depends: [ "foo" ] |}] in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1.0", {| depends: [ "ocaml-base-compiler" { = "6.0" } ] |};
  ] in
  solve t "No solution" ~platforms ~root_pkgs ~commits:[opam_repo, opam_packages]

let test_errors t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let root_pkgs = ["app.dev", {| syntax error |}] in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1.0", {| depends: [ "ocaml-base-compiler" ] |};
  ] in
  solve t "Invalid opam root pkg" ~platforms ~root_pkgs ~commits:[opam_repo, opam_packages];
  let root_pkgs = ["app.dev", ""] in
  let opam_packages_invalid = [
    "ocaml-base-compiler.5.0", "syntax error";
  ] in
  solve t "Invalid package in repo" ~platforms ~root_pkgs ~commits:[opam_repo, opam_packages_invalid];
  Fmt.pr "@.## Invalid commit@.@.";
  let req =
    { Solver_service_api.Worker.Solve_request.
      opam_repository_commits = [ "opam-repo.git", "17e66310c5c95560291a2d6499e7154d30f95f06"];
      root_pkgs = [];
      pinned_pkgs = [];
      platforms;
    }
  in
  let response = Solver_service.Solver.solve t ~log:stderr_log req in
  Fmt.pr "@[<v2>results:@,%a@]@." pp_response response;
  Fmt.pr "@.## Invalid repository@.@.";
  let req =
    { Solver_service_api.Worker.Solve_request.
      opam_repository_commits = [ "foo.git", "17e66310c5c95560291a2d6499e7154d30f95f06"];
      root_pkgs = [];
      pinned_pkgs = [];
      platforms;
    }
  in
  let response = Solver_service.Solver.solve t ~log:stderr_log req in
  Fmt.pr "@[<v2>results:@,%a@]@." pp_response response

let test_double_fetch t =
  Fmt.pr "@.";
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let root_pkgs = ["app.dev", Utils.add_opam_header ""] in
  let test req = Solver_service.Solver.solve t ~log:stderr_log req in
  let good_commit =
    Opam_repo.commit opam_repo [
      "ocaml-base-compiler.5.0", {|synopsis: "Force fetch"|};
    ]
  in
  let test_bad () =
    test {
      Solver_service_api.Worker.Solve_request.
      opam_repository_commits = ["opam-repo.git", "17e66310c5c95560291a2d6499e7154d30f9aaaa"];
      root_pkgs;
      pinned_pkgs = [];
      platforms;
    }
  and test_good () =
    test {
      Solver_service_api.Worker.Solve_request.
      opam_repository_commits = [good_commit];
      root_pkgs;
      pinned_pkgs = [];
      platforms;
    }
  in
  let both a b =
    let a, b = Fiber.pair a b in
    Fmt.pr "@[<v2>a:@,%a@]@." Utils.pp_response a;
    Fmt.pr "@[<v2>b:@,%a@]@." Utils.pp_response b
  in
  Fmt.pr "## Concurrent fetches of invalid commit@.@.";
  both test_bad test_bad;
  Fmt.pr "@.## One bad, one good@.@.";
  both test_bad test_good

let test_multiple_roots t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let opam_packages = [
    "ocaml-base-compiler.4.14", "";
    "ocaml-base-compiler.5.0", "";
    "ocaml.4.14", {| depends: [ "ocaml-base-compiler" {= "4.14"} ] |};
    "ocaml.5.0", {| depends: [ "ocaml-base-compiler" {= "5.0"} ] |};
  ] in
  let root_pkgs = [
    "api.dev", {| depends: [ "ocaml" ] |};
    "impl.dev", {| depends: [ "ocaml" {>= "5.0"} ] |};
  ] in
  solve t "Only compatible packages"
    ~root_pkgs
    ~commits:[opam_repo, opam_packages]
    ~platforms:[
      "ocaml-4.14", { debian_12_ocaml_5 with ocaml_version = "4.14" };
      "ocaml-5.0", debian_12_ocaml_5;
    ]

let test_pinned t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "foo.1", {| depends: [ "ocaml-base-compiler" {= "4.14"} ] |};
  ] in
  let root_pkgs = [ "root.dev", {| depends: [ "foo" ] |} ] in
  let pinned_pkgs = [
    "foo.2", {| depends: [ "ocaml-base-compiler" {= "5.0"} ] |};
  ] in
  solve t "Solution using pinned packages"
    ~root_pkgs
    ~pinned_pkgs
    ~commits:[opam_repo, opam_packages]
    ~platforms

let test_cancel t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
  ] in
  let root_pkgs = [ "foo.dev", "" ] in
  let cancelled = Promise.create_resolved () in
  solve t "Solution using pinned packages"
    ~cancelled
    ~root_pkgs
    ~commits:[opam_repo, opam_packages]
    ~platforms

let test_available t =
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
  ] in
  let root_pkgs = [
    "foo.dev", "";
    "foo-linux.dev", {| available: os = "linux" |};
  ] in
  solve t "foo-linux is only selected on Linux platform, but foo is selected for both"
    ~root_pkgs
    ~commits:[opam_repo, opam_packages]
    ~platforms:[
      "linux", debian_12_ocaml_5;
      "mac", { debian_12_ocaml_5 with os = "macos" };
    ]

let test_solve_cache t =
  let solve = solve_cache in
  let opam_repo = Opam_repo.create "opam-repo.git" in
  let root_pkgs = ["app.dev", {| depends: [ "foo" ] |}] in
  let depends = {| depends: [ "ocaml-base-compiler" "bar" ] |} in
  let opam_packages = [
    "ocaml-base-compiler.5.0", "";
    "bar.1.0", {| depends: [ "baz" ] |};
    "baz.1.0", "";
    "foo.1.0",depends;
    "foobar.0.1", "";
  ]
  in
  let first_opam_packages = opam_packages in
  let recent_commits =
    solve t "Select foo.1.0" ~platforms ~root_pkgs ~previous_commits:[opam_repo,[]]
      ~commits:[opam_repo, opam_packages]
  in
  let opam_packages = ("foo.1.1",depends) :: opam_packages in
  let recent_commits =
  solve t "Foo 1.1 now available (A direct dependency, the result will contain the new commit)" ~previous_commits:recent_commits ~platforms ~root_pkgs ~commits:[
    opam_repo, opam_packages
  ]
  in
  let opam_packages = ("foo.1.1",depends)::opam_packages in
  let recent_commits =
    solve t "Foo 1.1 again (hit the cache, the commit won't change)" ~previous_commits:recent_commits ~platforms ~root_pkgs ~commits:[
      opam_repo, opam_packages
    ]
  in
  let opam_packages =
   ("baz.1.0",{|depends: [ "foobar" ]|})
    ::(List.remove_assoc "baz.1.0" opam_packages)
  in
  let recent_commits =
    solve t "Baz 1.0 is a transitive dep of Foo, the cache will be invalidated(the result will contain the new commit)" ~previous_commits:recent_commits ~platforms ~root_pkgs ~commits:[
      opam_repo, opam_packages
    ]
  in

  let opam_packages = ("oof.1.0","") :: opam_packages in
  let recent_commits =
  solve t "Oof 1.0 now available (hit the cache, the commit won't change in the result)" ~previous_commits:recent_commits ~platforms ~root_pkgs ~commits:[
    opam_repo, opam_packages
  ]
  in
  solve t
    "Oof 1.1 now available (will invalidate the cache because foo 1.1 will be removed, the result will contain the new commit)"
    ~previous_commits:recent_commits ~platforms ~root_pkgs ~commits:[
    opam_repo, ("oof.1.1","") :: first_opam_packages
  ] |> ignore;
  ()

let () =
  Eio_main.run @@ fun env ->
  let domain_mgr = env#domain_mgr in
  let process_mgr = env#process_mgr in
  let cache_dir = "cache" in
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun () ->
  Switch.run @@ fun sw ->
  let t = Solver_service.Solver.create ~sw ~domain_mgr ~process_mgr ~cache_dir ~n_workers:2 in
  [
    "Simple", test_simple;
    "Overlay", test_overlay;
    "Lower-bound", test_lower_bound;
    "No solution", test_no_solution;
    "Errors", test_errors;
    "Double fetch", test_double_fetch;
    "Multiple roots", test_multiple_roots;
    "Pinned", test_pinned;
    "Cancel", test_cancel;
    "Available", test_available;
    "Solve_cache", test_solve_cache;
  ]
  |> List.iter (fun (name, fn) ->
      Fmt.pr "@.# %s@." name;
      fn t
    );
  Fmt.epr "Tests successful!@."
