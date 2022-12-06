module Worker = Solver_service_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)
module Store = Git_unix.Store
open Lwt.Syntax

let env (vars : Worker.Vars.t) v =
  Opam_0install.Dir_context.std_env ~arch:vars.arch ~os:vars.os
    ~os_distribution:vars.os_distribution ~os_version:vars.os_version
    ~os_family:vars.os_family ~opam_version:vars.opam_version () v

let parse_opam (name, contents) =
  let pkg = OpamPackage.of_string name in
  let opam = OpamFile.OPAM.read_from_string contents in
  (OpamPackage.name pkg, (OpamPackage.version pkg, opam))

let solve ~packages ~pins ~root_pkgs (vars : Worker.Vars.t) =
  let ocaml_package = OpamPackage.Name.of_string vars.ocaml_package in
  let ocaml_version = OpamPackage.Version.of_string vars.ocaml_version in
  let context =
    Git_context.create () ~packages ~pins ~env:(env vars)
      ~constraints:
        (OpamPackage.Name.Map.singleton ocaml_package (`Eq, ocaml_version))
      ~test:(OpamPackage.Name.Set.of_list root_pkgs)
      ~with_beta_remote:
        Ocaml_version.(Releases.is_dev (of_string_exn vars.ocaml_version))
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context (ocaml_package :: root_pkgs) in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" (t1 -. t0);
  match r with
  | Ok sels ->
      let pkgs = Solver.packages_of_result sels in
      Ok (List.map OpamPackage.to_string pkgs)
  | Error diagnostics -> Error (Solver.diagnostics diagnostics)

let main commits =
  let packages =
    Lwt_main.run
      (* Read all the package from all the given opam-repository repos,
       * and collate them into a single Map. *)
      (Lwt_list.fold_left_s
         (fun acc commit ->
           let repo_url = commit.Remote_commit.repo in
           let hash = Store.Hash.of_hex commit.Remote_commit.hash in
           let* store = Opam_repository.open_store ~repo_url () in
           Git_context.read_packages ~acc store hash)
         OpamPackage.Name.Map.empty commits)
  in
  let rec aux () =
    match input_line stdin with
    | exception End_of_file -> ()
    | len ->
        let len = int_of_string len in
        let data = really_input_string stdin len in
        let request =
          Worker.Solve_request.of_yojson (Yojson.Safe.from_string data)
          |> Result.get_ok
        in
        let {
          Worker.Solve_request.opam_repository_commits;
          root_pkgs;
          pinned_pkgs;
          platforms;
        } =
          request
        in
        assert (
          List.for_all
            (fun (repo, hash) -> List.mem (Remote_commit.v ~repo ~hash) commits)
            opam_repository_commits);
        let root_pkgs = List.map parse_opam root_pkgs in
        let pinned_pkgs = List.map parse_opam pinned_pkgs in
        let pins = root_pkgs @ pinned_pkgs |> OpamPackage.Name.Map.of_list in
        let root_pkgs = List.map fst root_pkgs in
        platforms
        |> List.iter (fun (_id, platform) ->
               let msg =
                 match solve ~packages ~pins ~root_pkgs platform with
                 | Ok packages -> "+" ^ String.concat " " packages
                 | Error msg -> "-" ^ msg
               in
               Printf.printf "%d\n%s%!" (String.length msg) msg);
        aux ()
  in
  aux ()

let main commit =
  try main commit
  with ex ->
    Fmt.epr "solver bug: %a@." Fmt.exn ex;
    let msg =
      match ex with Failure msg -> msg | ex -> Printexc.to_string ex
    in
    let msg = "!" ^ msg in
    Printf.printf "0.0\n%d\n%s%!" (String.length msg) msg;
    raise ex
