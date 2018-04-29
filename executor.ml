module ExecutionEngine = Llvm_executionengine
module ScalarOpts = Llvm_scalar_opts
module PassManager = Llvm.PassManager
module DataLayout = Llvm_target.DataLayout

open Ctypes
open Foreign

let generate program =
  Llvm.enable_pretty_stacktrace ();
  Llvm_all_backends.initialize ();
  let bfmod = program |> Optimizer.optimize |> Codegen.gen in

  let fpm = PassManager.create_function bfmod in
  PassManager.initialize fpm |> ignore;

  ScalarOpts.add_instruction_combination fpm |> ignore;
  ScalarOpts.add_licm fpm |> ignore;
  ScalarOpts.add_ind_var_simplification fpm |> ignore;
  ScalarOpts.add_loop_deletion fpm |> ignore;

  let main = match Llvm.lookup_function "main" bfmod with
    | Some(main) -> main
    | None -> raise (Failure "kek")
  in

  for i = 0 to 3 do
    ignore i;
    ScalarOpts.add_gvn fpm |> ignore;
    ScalarOpts.add_sccp fpm |> ignore;
    ScalarOpts.add_cfg_simplification fpm |> ignore;
    ScalarOpts.add_constant_propagation fpm |> ignore;
    ScalarOpts.add_aggressive_dce fpm |> ignore;
    ScalarOpts.add_cfg_simplification fpm |> ignore;
    ScalarOpts.add_dead_store_elimination fpm |> ignore;
  done;

  PassManager.run_function main fpm |> ignore;
  Llvm_analysis.assert_valid_module bfmod;
  bfmod

let run program =
  ExecutionEngine.initialize () |> ignore;
  let bfmod = generate program in
  let engine = ExecutionEngine.create bfmod in
  let main_address =
    let main_type = funptr (void @-> returning void) in
    ExecutionEngine.get_function_address "main" main_type engine
  in
  main_address ()
