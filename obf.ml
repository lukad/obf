type t = {
    emit_llvm : bool [@set_true]
  } [@@deriving show, argparse {
    positional = ["program", "Brainfuck program"]
  }]

let default = {
    emit_llvm = false;
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

  ignore cfg;

  let program =
    match parse_program rest.(0) with
    | Some program -> program
    | None -> exit 1
  in

  match cfg.emit_llvm with
  | false -> Executor.run program
  | true -> Executor.generate program |> Executor.show_module |> print_endline

let () = main ()
