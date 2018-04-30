type emit = Llvm | Ast | None [@@deriving show]

let parse_emit = function
  | "llvm" -> Llvm
  | "ast" -> Ast
  | s -> raise (Invalid_argument s)

let show_emit = function
  | Llvm -> "llvm"
  | Ast -> "ast"
  | None -> "none"

type t = {
    emit : emit [@print show_emit] [@parse parse_emit];
  } [@@deriving show, argparse {
    positional = ["program", "Brainfuck program"]
  }];;

let default = {
    emit = None;
  }

let read_file f =
  let input = open_in f in
  let length = in_channel_length input in
  let s = Bytes.create length in
  really_input input s 0 length;
  close_in input;
  Bytes.to_string s

let parse_program file =
  match read_file file |> Reader.read_string with
  | Result.Ok program ->
     Some program
  | Result.Error msg ->
     print_endline msg;
     None

let main () =
  let cfg, rest = argparse default "obf" Sys.argv in

  let program =
    match parse_program rest.(0) with
    | Some program -> program |> Optimizer.optimize
    | None -> exit 1
  in

  match cfg.emit with
  | None -> Executor.run program
  | Llvm -> Executor.generate program |> Executor.show_module |> print_endline
  | Ast -> Ast.show_program program |> print_endline

let () = main ()
