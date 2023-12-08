open Eio.Std

module Store = Git_unix.Store

module type CacheType = sig
  type cache
  val dir: cache -> string
  val process_mgr : cache -> [`Generic] Eio.Process.mgr_ty r
end

module Make (Cache : CacheType) = struct

  type t = Cache.cache

  let git_command ?cwd args =
    "git" ::
    match cwd with
    | Some dir -> "-C" :: dir :: args
    | None -> args

  let [@warning "-27"] grep_command ?cwd args = "grep" :: args

  let [@warning "-27"] rm_command ?cwd args = "rm" :: args

  let run_git ?cwd t args =
    Eio.Process.run (Cache.process_mgr t) (git_command ?cwd args)

  let run_rm ?cwd t args =
    Eio.Process.run (Cache.process_mgr t) (rm_command ?cwd args)

  let line_opt r =
    if Eio.Buf_read.at_end_of_input r then None
    else Some (Eio.Buf_read.line r)

  let run_git_line ?cwd t args =
    Eio.Process.parse_out (Cache.process_mgr t) line_opt (git_command ?cwd args)

  let take_all_opt r =
    if Eio.Buf_read.at_end_of_input r then None
    else Some (Eio.Buf_read.take_all r)

  let run_take_all ?cwd ?stdin t args command =
    Eio.Process.parse_out ?stdin (Cache.process_mgr t) take_all_opt (command ?cwd args)

  let lines_opt r =
    if Eio.Buf_read.at_end_of_input r then None
    else (Eio.Buf_read.map List.of_seq Eio.Buf_read.lines) r |> Option.some

  let run_lines ?cwd ?stdin t args command =
    Eio.Process.parse_out ?stdin (Cache.process_mgr t) lines_opt (command ?cwd args)

  let run_git_lines ?cwd ?stdin t args = run_lines ?cwd ?stdin t args git_command

  let run_git_take_all ?cwd ?stdin t args = run_take_all ?cwd ?stdin t args git_command 

  let run_grep_lines ?cwd ?stdin t args = run_lines ?cwd ?stdin t args grep_command

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
    let cache_dir = Cache.dir t in
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
    Fpath.(v cache_dir / sane_host / sane_path)

  let remove t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url in
    let clone_path_str = Fpath.to_string clone_path in
    match Unix.lstat clone_path_str with
    | Unix.{ st_kind = S_DIR; _ } -> (
      try
        run_rm t ["-fr"; clone_path_str]
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "removing %S" clone_path_str)
    | _ -> ()

  let clone ~bare t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url in
    let clone_parent = Fpath.parent clone_path |> Fpath.to_string in
    let clone_path_str = Fpath.to_string clone_path in
    match Unix.lstat clone_path_str with
    | Unix.{ st_kind = S_DIR; _ } -> ()
    | _ -> Fmt.failwith "%S is not a directory!" clone_path_str
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p clone_parent;
      try
        if bare then
          run_git t ["clone"; "--bare"; repo_url; clone_path_str]
        else
          run_git t ["clone"; repo_url; clone_path_str]
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "cloning %S" repo_url

  let clone_bare t repo_url = clone ~bare:true t repo_url

  let clone t repo_url = clone ~bare:false t repo_url

  let fetch t repo_url =
    try
      let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
      run_git t ~cwd:clone_path ["fetch"; "origin"]
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "fetching %S" repo_url

  let pull t repo_url =
    try
      let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
      run_git t ~cwd:clone_path ["pull"; "origin"]
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "pulling %S" repo_url

  let all_commits_rev t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_lines t ~cwd:clone_path
    @@ "log"
       :: "--reverse"
       :: [ "--format=format:%H" ]
    |> Option.value ~default:[]

  let diff_pkgs t ~repo_url ~new_commit ~old_commit =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_take_all t ~cwd:clone_path
    @@ "diff"
       :: old_commit
       :: new_commit
       :: "--"
       :: [ "packages" ]
    |> function
    | None -> []
    | Some diff ->
      try
        run_grep_lines ~stdin:(Eio.Flow.string_source diff) t ["^... ./packages/.*/opam"]
        |> Option.value ~default:[]
        |> List.filter_map (fun path -> Astring.String.cut ~sep:"/" path |> Option.map snd)
      with _ -> [] (* grep could exits with status 1 if there's no match *)

  let oldest_commit_with t ~repo_url ~from paths =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_line t ~cwd:clone_path
    @@ "log"
       :: "-n" :: "1"
       :: "--format=format:%H"
       :: from
       :: "--"
       :: paths

  let open_store t repo_url =
    let path = repo_url_to_clone_path t repo_url in
    match Lwt_eio.run_lwt (fun () -> Git_unix.Store.v ~dotgit:path path) with
    | Ok x -> x
    | Error e ->
      Fmt.failwith "Failed to open %a: %a" Fpath.pp path Store.pp_error e
end
