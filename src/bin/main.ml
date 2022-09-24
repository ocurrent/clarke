type measurer_spec = [
  | `Const of float
  | `Variorum
  | `Ipmi
]

let measurer ~clock = function
  | `Const f -> Clarke.Models.const ~clock f
  | `Variorum -> Clarke.Variorum.make ~clock ()
  | `Ipmi -> Clarke.Ipmi.make ~clock ()