open Clarke

type measurer_spec = [ `Const of float | `Variorum | `Ipmi ]

let measurer ~clock = function
  | `Const f -> Models.const ~clock f
  | `Variorum -> S.Measurer ((module Clarke.Variorum), { clock })

let run sink (S.Measurer ((module M), t) : S.measurer) period =
  while true do
    M.collect t |> Outputs.Flow.send sink;
    Eio.Flow.copy_string "\n" sink;
    Eio_unix.sleep period
  done

let () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  run env#stdout (measurer ~clock `Variorum) 2.
