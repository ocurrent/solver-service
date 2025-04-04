open Eio.Std

module Worker = Solver_service_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)

type request = {
  packages : Packages.t;
  root_pkgs : (OpamTypes.name * (OpamTypes.version * OpamFile.OPAM.t)) list;
  pinned_pkgs : (OpamTypes.name * (OpamTypes.version * OpamFile.OPAM.t)) list;
  vars : Solver_service_api.Worker.Vars.t;
  cancelled : unit Eio.Promise.t option;
}

type reply = ((OpamPackage.t list, string) result * float, [`Msg of string | `Cancelled]) result

let env (vars : Worker.Vars.t) v =
  match v with
  | "arch" -> Some (OpamTypes.S vars.arch)
  | "os" -> Some (OpamTypes.S vars.os)
  | "os-distribution" -> Some (OpamTypes.S vars.os_distribution)
  | "os-version" -> Some (OpamTypes.S vars.os_version)
  | "os-family" -> Some (OpamTypes.S vars.os_family)
  | "opam-version"  -> Some (OpamVariable.S vars.opam_version)
  | "sys-ocaml-version" -> None
  | "ocaml:native" -> Some (OpamTypes.B true)
  | "enable-ocaml-beta-repository" -> None      (* Fake variable? *)
  | _ ->
    (* Disabled, as not thread-safe! *)
    (* OpamConsole.warning "Unknown variable %S" v; *)
    None

let solve { packages; root_pkgs; pinned_pkgs; vars; cancelled } =
  match cancelled with
  | Some p when Promise.is_resolved p -> Error `Cancelled
  | _ ->
    try
      let pins = root_pkgs @ pinned_pkgs |> OpamPackage.Name.Map.of_list in
      let root_pkgs = List.map fst root_pkgs in
      let ocaml_package = OpamPackage.Name.of_string vars.ocaml_package in
      let ocaml_version = OpamPackage.Version.of_string vars.ocaml_version in
      let constraints = OpamPackage.Name.Map.singleton ocaml_package (`Eq, ocaml_version) in
      let with_beta_remote = Ocaml_version.(Releases.is_dev (of_string_exn vars.ocaml_version)) in
      let context =
        Git_context.create ()
          ~packages
          ~pins
          ~env:(env vars)
          ~constraints
          ~test:(OpamPackage.Name.Set.of_list root_pkgs)
          ~with_beta_remote
          ~lower_bound:vars.lower_bound
      in
      let t0 = Unix.gettimeofday () in
      let r = Solver.solve context (ocaml_package :: root_pkgs) in
      let t1 = Unix.gettimeofday () in
      let r =
        match r with
        | Ok sels -> Ok (Solver.packages_of_result sels)
        | Error diagnostics -> Error (Solver.diagnostics diagnostics)
      in
      Ok (r, (t1 -. t0))
    with Failure msg ->
      Error (`Msg msg)
