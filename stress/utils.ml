open Lwt.Syntax

let opam_template arch =
  let arch = Option.value ~default:"%{arch}%" arch in
  Fmt.str
    {|
  {
    "arch": "%s",
    "os": "%%{os}%%",
    "os_family": "%%{os-family}%%",
    "os_distribution": "%%{os-distribution}%%",
    "os_version": "%%{os-version}%%",
    "opam_version": "%%{opam-version}%%"
  }
|}
    arch

let get_vars ~ocaml_package_name ~ocaml_version ?arch () =
  let+ vars =
    Solver_service.Process.pread
      ("", [| "opam"; "config"; "expand"; opam_template arch |])
  in
  let json =
    match Yojson.Safe.from_string vars with
    | `Assoc items ->
        `Assoc
          (("ocaml_package", `String ocaml_package_name)
          :: ("ocaml_version", `String ocaml_version)
          :: items)
    | json ->
        Fmt.failwith "Unexpected JSON: %a"
          Yojson.Safe.(pretty_print ~std:true)
          json
  in
  Result.get_ok @@ Solver_service_api.Worker.Vars.of_yojson json

let get_opam_file pv =
  Solver_service.Process.pread ("", [| "opam"; "show"; "--raw"; pv |])

let get_opam_packages () =
  let open Lwt.Infix in
  Solver_service.Process.pread
    ("", [| "opam"; "list"; "--short"; "--color"; "never" |])
  >|= (fun s -> String.split_on_char '\n' s)
  >|= List.map (fun p -> String.trim p)
