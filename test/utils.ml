let verbose = false

let add_opam_header s =
  {|
    opam-version: "2.0"
    synopsis: "Test package"
  |} ^ s

module Opam_repo : sig
  type t

  val create : string -> t
  (** [create path] opens a Git repository at [path], creating it if needed. *)

  val commit : t -> (string * string) list -> string * string
  (** [commit t files] creates a commit with the given opam package files and returns
      the [(repo_url, hash]) pair for it. *)

  val pp : t Fmt.t
end = struct
  (* A fake opam-repository upstream for testing. *)

  module S = Git_unix.Store

  type t = {
    name : string;
    store : S.t;
  }

  let pp f t = Fmt.string f t.name

  let or_die = function
    | Ok x -> x
    | Error e -> Fmt.failwith "%a" Git_unix.Store.pp_error e

  let create path : t =
    let store = Lwt_eio.run_lwt (fun () -> Git_unix.Store.v (Fpath.v path)) |> or_die in
    { store; name = path }

  let test_user = { Git.User.name = "test"; email = "test@example.com"; date = (0L, None) }

  let write t value =
    Lwt_eio.run_lwt (fun () -> S.write t.store value)
    |> or_die
    |> fst      (* The second value is undocumented and ignored everywhere in the git_unix examples *)

  let set_branch t branch commit =
    Lwt_eio.run_lwt (fun () -> S.Ref.write t.store branch (Git.Reference.uid commit)) |> or_die

  let write_tree t items =
    let write_entry (name, value) =
      match value with
      | `Dir node -> { Git.Tree.name; perm = `Dir; node }
      | `File node -> { Git.Tree.name; perm = `Normal; node }
    in
    let items = List.map write_entry items in
    write t (S.Value.tree (S.Value.Tree.of_list items))

  (* Converts
     ["foo.1", _;
      "foo.2", _;
      "bar.1"; _;
     to
     [foo -> 1 -> _
             2 -> _
      bar -> 1 -> _] *)
  let group_packages =
    let add acc (name, contents) =
      let pkg = OpamPackage.of_string name in
      let add_version old =
        OpamPackage.Version.Map.add pkg.version contents old
      in
      OpamPackage.Name.Map.update pkg.name add_version OpamPackage.Version.Map.empty acc
    in
    List.fold_left add OpamPackage.Name.Map.empty

  let write_versions t ~name versions =
    let versions = OpamPackage.Version.Map.to_seq versions |> List.of_seq in
    let write_version (v, contents) =
      let contents = add_opam_header contents in
      let contents = write t (S.Value.blob (S.Value.Blob.of_string contents)) in
      let version_dir = write_tree t ["opam", `File contents] in
      let pkg = OpamPackage.create name v in
      (OpamPackage.to_string pkg, `Dir version_dir)
    in
    write_tree t (List.map write_version versions)

  let commit t pkgs =
    let pkgs =
      group_packages pkgs
      |> OpamPackage.Name.Map.to_seq
      |> List.of_seq
      |> List.map (fun (name, versions) ->
          (OpamPackage.Name.to_string name, `Dir (write_versions t ~name versions))
        )
    in
    let tree = write_tree t ["packages", `Dir (write_tree t pkgs)] in
    let commit = S.Value.Commit.make ~tree ~author:test_user ~committer:test_user (Some "Commit") in
    let commit = write t (S.Value.commit commit) in
    set_branch t Git.Reference.master commit;
    (t.name, S.Hash.to_hex commit)
end

let stderr_log =
  let module L = Solver_service_api.Raw.Service.Log in
  L.local @@ object
    inherit L.service

    method write_impl params release_param_caps =
      let open L.Write in
      release_param_caps ();
      if verbose then (
        let msg = Params.msg_get params in
        output_string stderr msg;
        flush stderr;
      );
      Capnp_rpc_lwt.Service.return_empty ()
  end

let pp_packages = Fmt.Dump.list (Fmt.using fst Fmt.string)
let pp_commits = Fmt.Dump.list (Fmt.Dump.pair Opam_repo.pp pp_packages)
let pp_platforms = Fmt.Dump.list (Fmt.using fst Fmt.string)

let pp_selection f { Solver_service_api.Worker.Selection.id; compat_pkgs; packages; commits; lower_bound } =
  Fmt.pf f "@[<v2>%s:@,compat_pkgs: %a@,packages: %a@,commits: %a@,lower_bound: %b@]"
    id
    Fmt.(Dump.list string) compat_pkgs
    Fmt.(Dump.list string) packages
    Fmt.(Dump.list (Dump.pair string string)) commits
    lower_bound

let pp_response f = function
  | Ok sels -> Fmt.Dump.list pp_selection f sels
  | Error `Cancelled -> Fmt.string f "Cancelled"
  | Error `Msg m -> Fmt.pf f "Error: %s" m

let solve ?cancelled ?(pinned_pkgs=[]) t label ~commits ~root_pkgs ~platforms =
  Fmt.pr "@.## %s ##@.@.commits: %a@.root_pkgs: %a@.platforms: %a@."
    label
    pp_commits commits
    pp_packages root_pkgs
    pp_platforms platforms;
  if pinned_pkgs <> [] then Fmt.pr "pinned: %a@." pp_packages pinned_pkgs;
  let root_pkgs = List.map (fun (pkg, opam) -> pkg, add_opam_header opam) root_pkgs in
  let pinned_pkgs = List.map (fun (pkg, opam) -> pkg, add_opam_header opam) pinned_pkgs in
  let opam_repository_commits = List.map (fun (repo, packages) -> Opam_repo.commit repo packages) commits in
  let req = { Solver_service_api.Worker.Solve_request.
              opam_repository_commits;
              root_pkgs;
              pinned_pkgs;
              platforms;
            }
  in
  let response = Solver_service.Solver.solve ?cancelled t ~log:stderr_log req in
  Fmt.pr "@[<v2>results:@,%a@]@." pp_response response
