let main () =
  let prog = [
      Ast.Add(8);
      Ast.Loop([
            Ast.Move(1);
            Ast.Add(8);
            Ast.Move(-1);
            Ast.Add(-1);
        ])
    ]
  in
  print_endline (Ast.show_program prog)
;;

main()
