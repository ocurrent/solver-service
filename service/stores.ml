open Eio.Std

module Store = Git_unix.Store
module Store_map = Map.Make(String)

let (let*!) = Result.bind

let git_lock = Eio.Mutex.create ()
(* Avoid running multiple git operations at a time.
   This could be per-store, but we normally only have one anyway. *)

type commit = string * string

type t = {
  cache_dir : string;
  stores_lock : Eio.Mutex.t;
  mutable stores : Store.t Store_map.t;
  process_mgr : Eio.Process.mgr;

  (* For now we only keep the most recent set of packages. *)
  mutable index_cache : (commit list * Packages.t) option;
}

let git_command ?cwd args =
  "git" ::
  match cwd with
  | Some dir -> "-C" :: dir :: args
  | None -> args

let run_git ?cwd t args =
  Eio.Process.run t.process_mgr (git_command ?cwd args)

let line_opt r =
  if Eio.Buf_read.at_end_of_input r then None
  else Some (Eio.Buf_read.line r)

let run_git_line ?cwd t args =
  Eio.Process.parse_out t.process_mgr line_opt (git_command ?cwd args)

module Git_clone = struct
  let replace_special =
    String.map @@ function
    | 'A'..'Z'
    | 'a'..'z'
    | '0'..'9'
    | '-' as c -> c
    | _ -> '_'

  let rec mkdir_p path =
    try Unix.mkdir path 0o700 with
    | Unix.Unix_error (EEXIST, _, _) -> ()
    | Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname path in
      mkdir_p parent;
      Unix.mkdir path 0o700

  let repo_url_to_clone_path t repo_url =
    let uri = Uri.of_string repo_url in
    let sane_host =
      match Uri.host uri with
      | Some host -> replace_special host
      | None -> "no_host"
    in
    let sane_path =
      Uri.(
        path uri
        |> pct_decode
        |> Filename.chop_extension
        |> replace_special)
    in
    Fpath.(v t.cache_dir / sane_host / sane_path)

  let clone t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url in
    let clone_parent = Fpath.parent clone_path |> Fpath.to_string in
    let clone_path_str = Fpath.to_string clone_path in
    match Unix.lstat clone_path_str with
    | Unix.{ st_kind = S_DIR; _ } -> Ok ()
    | _ -> Fmt.failwith "%S is not a directory!" clone_path_str
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p clone_parent;
      try
        run_git t ["clone"; "--bare"; repo_url; clone_path_str];
        Ok ()
      with Eio.Io _ as ex ->
        Fmt.error_msg "Error cloning %S: %a" repo_url Fmt.exn ex

  let open_store t repo_url =
    let*! () = clone t repo_url in
    let path = repo_url_to_clone_path t repo_url in
    match Lwt_eio.run_lwt (fun () -> Git_unix.Store.v ~dotgit:path path) with
    | Ok _ as x -> x
    | Error e ->
      Fmt.error_msg "Failed to open %a: %a" Fpath.pp path Store.pp_error e

  let oldest_commit_with t ~repo_url ~from paths =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_line t ~cwd:clone_path
    @@ "log"
       :: "-n" :: "1"
       :: "--format=format:%H"
       :: from
       :: "--"
       :: paths

  let oldest_commits_with t ~from pkgs =
    let paths =
      pkgs
      |> List.map (fun pkg ->
          let name = OpamPackage.name_to_string pkg in
          let version = OpamPackage.version_to_string pkg in
          Printf.sprintf "packages/%s/%s.%s" name name version)
    in
    from
    |> Fiber.List.filter_map (fun (repo_url, hash) ->
        oldest_commit_with t ~repo_url ~from:hash paths
        |> Option.map (fun commit -> (repo_url, commit))
      )

  let fetch t repo_url =
    try
      let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
      run_git t ~cwd:clone_path ["fetch"; "origin"];
      Ok ()
    with Eio.Io _ as ex ->
      Fmt.error_msg "Error fetching %S: %a" repo_url Fmt.exn ex
end

let oldest_commit = Eio.Semaphore.make 180
(* we are using at most 360 pipes at the same time and that's enough to keep the current
 * performance and prevent some jobs to fail because of file descriptors exceed the limit.*)

let get t repo_url =
  Eio.Mutex.use_ro t.stores_lock @@ fun () ->
  match Store_map.find_opt repo_url t.stores with
  | Some x -> Ok x
  | None ->
    let*! store = Git_clone.open_store t repo_url in
    t.stores <- Store_map.add repo_url store t.stores;
    Ok store

let mem store hash = Lwt_eio.run_lwt (fun () -> Store.mem store hash)

let update_opam_repository_to_commit t (repo_url, hash) =
  let*! store = get t repo_url in
  Eio.Mutex.use_ro t.stores_lock @@ fun () ->
  let hash = Store.Hash.of_hex hash in
  if mem store hash then Ok ()
  else (
    Fmt.pr "Need to update %s to get new commit %a@." repo_url Store.Hash.pp hash;
    Eio.Mutex.use_ro git_lock @@ fun () ->
    let*! () = Git_clone.fetch t repo_url in
    let old_store = store in
    let*! new_store = Git_clone.open_store t repo_url in
    t.stores <- Store_map.add repo_url new_store t.stores;
    Lwt_eio.run_lwt (fun () -> Git_unix.Store.close_pack_files old_store);
    if mem new_store hash then
      Ok ()
    else
      Error (`Msg "Still missing commit after update!")
  )

let create ~process_mgr ~cache_dir =
  {
    cache_dir;
    process_mgr = (process_mgr :> Eio.Process.mgr);
    stores_lock = Eio.Mutex.create ();
    stores = Store_map.empty;
    index_cache = None;
  }

let oldest_commits_with t ~from repo_packages =
  Eio.Semaphore.acquire oldest_commit;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release oldest_commit) @@ fun () ->
  Git_clone.oldest_commits_with t repo_packages ~from

(* We could do this in parallel, except that there might be duplicate repos in the list. *)
let rec fetch_commits t = function
  | [] -> Ok ()
  | x :: xs ->
    let*! () = update_opam_repository_to_commit t x in
    fetch_commits t xs

let packages t commits =
  Eio.Mutex.use_ro git_lock @@ fun () ->
  match t.index_cache with
  | Some (k, v) when k = commits -> Ok v
  | _ ->
    (* Read all the packages from all the given opam-repository repos,
       and collate them into a single Map. *)
    let rec overlay acc = function
      | [] -> Ok acc
      | (repo_url, hash) :: xs ->
        let*! store = get t repo_url in
        let hash = Store.Hash.of_hex hash in
        let acc = Packages.of_commit ~super:acc store hash in
        overlay acc xs
    in
    let*! v = overlay Packages.empty commits in
    t.index_cache <- Some (commits, v);
    Ok v
