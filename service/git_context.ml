module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

type rejection = UserConstraint of OpamFormula.atom | Unavailable

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages : Packages.t;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map;
      (* User-provided constraints *)
  test : OpamPackage.Name.Set.t;
  with_beta_remote : bool;
  lower_bound : bool;
}

let ocaml_beta_pkg = OpamPackage.of_string "ocaml-beta.enabled"

(* From https://github.com/ocaml/ocaml-beta-repository/blob/master/packages/ocaml-beta/ocaml-beta.enabled/opam *)
let ocaml_beta_opam =
  OpamFile.OPAM.read_from_string
    {|
opam-version: "2.0"
maintainer: "platform@lists.ocaml.org"
bug-reports: "https://github.com/ocaml/ocaml/issues"
authors: [
  "Xavier Leroy"
  "Damien Doligez"
  "Alain Frisch"
  "Jacques Garrigue"
  "Didier Rémy"
  "Jérôme Vouillon"
]
homepage: "https://ocaml.org"
synopsis: "OCaml beta releases enabled"
description: "Virtual package enabling the installation of OCaml beta releases."
flags: avoid-version
|}

let user_restrictions t name = OpamPackage.Name.Map.find_opt name t.constraints
let dev = OpamPackage.Version.of_string "dev"

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else
    match OpamVariable.Full.to_string v with
    | "version" -> Some (OpamTypes.S (OpamPackage.version_to_string pkg))
    | x -> t.env x

let filter_deps t pkg f =
  let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  f
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev ~dev_setup:false
       ~default:false

let filter_available t pkg opam =
  let available = OpamFile.OPAM.available opam in
  match OpamFilter.eval ~default:(B false) (env t pkg) available with
  | B true -> Ok opam
  | B false -> Error Unavailable
  | _ ->
      OpamConsole.error "Available expression not a boolean: %s"
        (OpamFilter.to_string available);
      Error Unavailable

let version_compare lower_bound (v1, opam1) (v2, opam2) =
  let avoid1 =
    List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam1)
  in
  let avoid2 =
    List.mem OpamTypes.Pkgflag_AvoidVersion (OpamFile.OPAM.flags opam2)
  in
  if avoid1 = avoid2 then
    if lower_bound then OpamPackage.Version.compare v2 v1
    else OpamPackage.Version.compare v1 v2
  else if avoid1 then -1
  else 1

let candidates t name =
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (version, opam) ->
      let pkg = OpamPackage.create name version in
      [ (version, filter_available t pkg opam) ]
  | None -> (
      let versions = Packages.get_versions t.packages name in
      let versions =
        if
          t.with_beta_remote
          && OpamPackage.Name.compare name (OpamPackage.name ocaml_beta_pkg)
             = 0
        then
          OpamPackage.Version.Map.add
            (OpamPackage.version ocaml_beta_pkg)
            ocaml_beta_opam versions
        else versions
      in
      let user_constraints = user_restrictions t name in
      OpamPackage.Version.Map.bindings versions
      |> List.fast_sort (version_compare t.lower_bound)
      |> List.rev_map (fun (v, opam) ->
          match user_constraints with
          | Some test
            when not
                (OpamFormula.check_version_formula
                   (OpamFormula.Atom test) v) ->
            (v, Error (UserConstraint (name, Some test)))
          | _ ->
            let pkg = OpamPackage.create name v in
            (v, filter_available t pkg opam)))

let pp_rejection f = function
  | UserConstraint x ->
    Fmt.pf f "Rejected by user-specified constraint %s"
      (OpamFormula.string_of_atom x)
  | Unavailable -> Fmt.string f "Availability condition not satisfied"

let create ?(test = OpamPackage.Name.Set.empty)
    ?(pins = OpamPackage.Name.Map.empty) ?(lower_bound = false) ~constraints
    ~env ~packages ~with_beta_remote () =
  { env; packages; pins; constraints; test; with_beta_remote; lower_bound }
