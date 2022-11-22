let src =
  Logs.Src.create "clarke" ~doc:"Clarke power and carbon monitoring system"

include (val Logs.src_log src : Logs.LOG)
