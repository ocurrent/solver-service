open Eio.Std

module Store = Git_unix.Store
module Store_map = Map.Make(String)

let (let*!) = Result.bind

type commit = string * string

type t = {
  cache_dir : string;
  mutable stores : Safe_store.t Store_map.t;
  process_mgr : Eio.Process.mgr;

  (* For now we only keep the most recent set of packages. *)
  mutable index_cache : (commit list * Packages.t Promise.or_exn) option;
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
    | Unix.{ st_kind = S_DIR; _ } -> ()
    | _ -> Fmt.failwith "%S is not a directory!" clone_path_str
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p clone_parent;
      try
        run_git t ["clone"; "--bare"; repo_url; clone_path_str]
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "cloning %S" repo_url

  let open_store t repo_url =
    let path = repo_url_to_clone_path t repo_url in
    match Lwt_eio.run_lwt (fun () -> Git_unix.Store.v ~dotgit:path path) with
    | Ok x -> x
    | Error e ->
      Fmt.failwith "Failed to open %a: %a" Fpath.pp path Store.pp_error e

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
      run_git t ~cwd:clone_path ["fetch"; "origin"]
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "fetching %S" repo_url
end

let oldest_commit = Eio.Semaphore.make 180
(* we are using at most 360 pipes at the same time and that's enough to keep the current
 * performance and prevent some jobs to fail because of file descriptors exceed the limit.*)

let get t repo_url =
  match Store_map.find_opt repo_url t.stores with
  | Some x -> Ok x
  | None ->
    let store = Safe_store.create () in
    t.stores <- Store_map.add repo_url store t.stores;
    (* As a convenience, try opening/cloning the store now.
       This avoids doing a git-fetch on start-up. *)
    Switch.run (fun sw ->
        let request = Safe_store.get ~sw store in
        match Safe_store.upgrade request (fun () ->
            Git_clone.clone t repo_url;
            Git_clone.open_store t repo_url
          )
        with
        | Ok () -> Ok store
        | Error `Concurrent_upgrade -> Ok store (* Probably can't happen, but not a problem if it did *)
        | Error `Exn ex -> Fmt.error_msg "%a" Eio.Exn.pp ex
      )

let mem store hash = Lwt_eio.run_lwt (fun () -> Store.mem store hash)

let try_update_to_commit t commit =
  Switch.run @@ fun sw ->
  let (repo_url, hash) = commit in
  let*! store = get t repo_url in
  let request = Safe_store.get ~sw store in
  let hash = Store.Hash.of_hex hash in
  match Safe_store.get_store request with
  | Ok store when mem store hash -> Ok ()
  | _ ->
    let*! () =
      Safe_store.upgrade request (fun () ->
          Fmt.pr "Need to update %s to get new commit %a@." repo_url Store.Hash.pp hash;
          Git_clone.clone t repo_url;
          Git_clone.fetch t repo_url;
          Git_clone.open_store t repo_url
        )
    in
    let request = Safe_store.get ~sw store in
    match Safe_store.get_store request with
    | Ok store ->
      if mem store hash then Ok ()
      else Error (`Msg "Still missing commit after update!")
    | Error ex -> Error (`Exn ex)

let rec update_to_commit t commit =
  match try_update_to_commit t commit with
  | Ok () -> Ok ()
  | Error `Concurrent_upgrade ->
    (* Retry using the new generation. *)
    update_to_commit t commit
  | Error `Exn ex ->
    Fmt.error_msg "%a" Eio.Exn.pp ex
  | Error `Msg _ as e -> e

let create ~process_mgr ~cache_dir =
  {
    cache_dir;
    process_mgr = (process_mgr :> Eio.Process.mgr);
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
    let*! () = update_to_commit t x in
    fetch_commits t xs

let packages t commits =
  match t.index_cache with
  | Some (k, v) when k = commits -> Ok (Promise.await_exn v)
  | _ ->
    (* Read all the packages from all the given opam-repository repos,
       and collate them into a single Map. *)
    let rec overlay acc = function
      | [] -> acc
      | (repo_url, hash) :: xs ->
        match get t repo_url with
        | Error `Msg m -> failwith m
        | Ok store ->
          let hash = Store.Hash.of_hex hash in
          let acc = Packages.of_commit ~super:acc store hash in
          overlay acc xs
    in
    let p, r = Promise.create () in
    t.index_cache <- Some (commits, p);
    match overlay Packages.empty commits with
    | packages ->
      Promise.resolve_ok r packages;
      Ok packages
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      Promise.resolve_error r ex;
      t.index_cache <- None;
      Printexc.raise_with_backtrace ex bt
