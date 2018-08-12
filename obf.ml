open Llvm_target

type emit = Llvm | Ast | Asm | Object | None [@@deriving show]

let parse_emit = function
  | "llvm" -> Llvm
  | "ast" -> Ast
  | "asm" -> Asm
  | "obj" -> Object
  | s -> raise (Invalid_argument s)

let _show_emit = function
  | Llvm -> "llvm"
  | Ast -> "ast"
  | Asm -> "asm"
  | Object -> "obj"
  | None -> "none"

type t = {
    emit : emit [@print _show_emit] [@parse parse_emit] [@short "-e"]
    (** Emit generated code to stdout: llvm, ast, asm, obj **);
  } [@@deriving show, argparse {
    positional = ["program", "Brainfuck program"]
  }]

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

let emit_machine_code program codegen_type =
  Llvm_all_backends.initialize ();
  let default_triple = Target.default_triple () in
  let target = Target.by_triple default_triple in
  let target_machine = TargetMachine.create ~triple:default_triple target in
  let llmodule = Executor.generate program in
  let buffer = TargetMachine.emit_to_memory_buffer llmodule codegen_type target_machine in
  Llvm.MemoryBuffer.as_string buffer

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
  | Asm -> emit_machine_code program CodeGenFileType.AssemblyFile |> print_endline
  | Object -> emit_machine_code program CodeGenFileType.ObjectFile |> print_endline

let () = main ()
