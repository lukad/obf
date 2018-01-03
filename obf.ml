let main () =
  match Reader.read_string "++++++++[>++++++++<-]>." with
  | Result.Ok v -> print_endline (Ast.show_program v)
  | Result.Error msg -> print_endline msg
;;

main()
