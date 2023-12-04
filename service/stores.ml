open Eio.Std

module Store = Git_unix.Store
module Store_map = Map.Make(String)

let (let*!) = Result.bind

type commit = string * string

type t = {
  cache_dir : string;
  mutable stores : Safe_store.t Store_map.t;
  process_mgr : [`Generic] Eio.Process.mgr_ty r;

  (* For now we only keep the most recent set of packages. *)
  mutable index_cache : (commit list * Packages.t Promise.or_exn) option;
}

module Git_clone = struct

  include ( Git_clone.Make ( struct
    type cache = t
    let dir t = t.cache_dir
    let process_mgr t = t.process_mgr
  end))

  let clone = clone_bare
end

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
    Git_clone.oldest_commit_with t ~repo_url ~from:hash paths
    |> Option.map (fun commit -> (repo_url, commit))
  )

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
    process_mgr = (process_mgr :> [`Generic] Eio.Process.mgr_ty r);
    stores = Store_map.empty;
    index_cache = None;
  }

let oldest_commits_with t ~from repo_packages =
  Eio.Semaphore.acquire oldest_commit;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release oldest_commit) @@ fun () ->
  oldest_commits_with t repo_packages ~from

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
