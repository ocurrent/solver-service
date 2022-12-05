let src = Logs.Src.create "solver-worker" ~doc:"solver worker agent"

include (val Logs.src_log src : Logs.LOG)
