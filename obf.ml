let main () =
  match Reader.read_string "++++ ++++ [> ++++ ++++ < -] > ." with
  | Result.Ok program -> program |> Executor.run |> ignore
  | Result.Error msg -> print_endline msg

let () = main ()
