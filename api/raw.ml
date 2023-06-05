(** Raw Solver API functions and types for CapnP RPC.

    Instantiates the generated schema code to use CapnP RPC LWT implementation.
    See [schema.capnp] for the Cap'n Proto schema file.
*)

include Schema.MakeRPC (Capnp_rpc_lwt)
