open Lwt.Syntax
module Log = Solver_service_api.Solver.Log
module P = Solver_service.Process

let commit, set_commit = Lwt.wait ()
let clone_path, set_clone_path = Lwt.wait ()

let get_sha clone_path =
  let cmd =
    "git"
    :: "-C"
    :: clone_path
    :: "log"
    :: "-n"
    :: "1"
    :: [ "--format=format:%H" ]
  in
  let cmd = ("", Array.of_list cmd) in
  P.pread cmd

let setup_store path =
  match path with
  | Error _ -> failwith "failed to create in-memory git store"
  | Ok path -> (
      Lwt.wakeup set_clone_path path;
      let* () =
        P.exec ("git", [| "git"; "-C"; Fpath.to_string path; "init" |])
      in
      let* () =
        P.exec
          ( "git",
            [|
              "git";
              "-C";
              Fpath.to_string path;
              "commit";
              "-m";
              "'empty'";
              "--allow-empty";
            |] )
      in
      let* store = Git_unix.Store.v path in
      match store with
      | Error err -> Fmt.failwith "%a" Git_unix.Store.pp_error err
      | Ok store ->
          let+ hash = get_sha (Fpath.to_string path) in
          Lwt.wakeup set_commit hash;
          store)

let open_store () =
  let* clone_path = clone_path in
  let+ store = Git_unix.Store.v clone_path in
  match store with
  | Ok store -> store
  | Error err -> Fmt.failwith "%a" Git_unix.Store.pp_error err

let clone () = Lwt.return ()
let oldest_commit_with ~from:_ _pkgs = commit
let fetch () = Lwt.return ()
