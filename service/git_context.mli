include Opam_0install.S.CONTEXT

val read_packages :
  ?acc:OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t ->
  Git_unix.Store.t ->
  Git_unix.Store.Hash.t ->
  OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t Lwt.t
(** [read_packages store commit] is an index of the opam files in [store] at
    [commit]. *)

val create :
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?prefer_oldest:bool ->
  constraints:OpamFormula.version_constraint OpamPackage.Name.Map.t ->
  env:(string -> OpamVariable.variable_contents option) ->
  packages:OpamFile.OPAM.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t ->
  with_beta_remote:bool ->
  unit ->
  t
