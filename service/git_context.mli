include Opam_0install.S.CONTEXT

val create :
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?lower_bound:bool ->
  constraints:OpamFormula.version_constraint OpamPackage.Name.Map.t ->
  env:(string -> OpamVariable.variable_contents option) ->
  packages:Packages.t ->
  with_beta_remote:bool ->
  unit ->
  t
