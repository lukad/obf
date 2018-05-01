module PassManager = Llvm.PassManager

let context = Llvm.global_context ()

let tape_size = 30000

let int_t size = Llvm.integer_type context size
let int size = Llvm.const_int (int_t size)

let cell_type = Llvm.integer_type context 8
let index_type = Llvm.integer_type context 32

let cell_value = Llvm.const_int cell_type
let index_value = Llvm.const_int index_type
let void = Llvm.void_type context

let read jit_module =
  let read_type = Llvm.function_type (int_t 32) [||] in
  Llvm.declare_function "getchar" read_type jit_module

let write jit_module =
  let write_type = Llvm.function_type void [|int_t 32|] in
  Llvm.declare_function "putchar" write_type jit_module

let calloc jit_module =
  let calloc_type = Llvm.function_type (Llvm.pointer_type (int_t 8)) [|int_t 64; int_t 64|] in
  Llvm.declare_function "calloc" calloc_type jit_module

let free jit_module =
  let free_type = Llvm.function_type void [|Llvm.pointer_type (int_t 8)|] in
  Llvm.declare_function "free" free_type jit_module

let gen program =
  ignore program;

  let jit_module = Llvm.create_module context "obf" in

  let read = read jit_module in
  let write = write jit_module in
  let calloc = calloc jit_module in
  let free = free jit_module in

  let main_type = Llvm.function_type (Llvm.void_type context) [||] in
  let main = Llvm.declare_function "main" main_type jit_module in

  let block = ref (Llvm.append_block context "" main) in
  let builder = ref (Llvm.builder_at_end context !block) in

  let tape = Llvm.build_call calloc [|int 64 tape_size; int 64 1|] "tape" !builder in
  let head = ref tape in

  let rec gen_ins = function
    | Ast.Move i ->
       head := Llvm.build_gep !head [|cell_value i|] "head" !builder
    | Ast.Add i ->
       let head_value = Llvm.build_load !head "head_value" !builder in
       let result = Llvm.build_add head_value (cell_value i) "add_result" !builder in
       Llvm.build_store result !head !builder |> ignore
    | Ast.Set i ->
       Llvm.build_store (cell_value i) !head !builder |> ignore
    | Ast.Read ->
       let input = Llvm.build_call read [||] "input" !builder in
       let truncated = Llvm.build_trunc input cell_type "truncated" !builder in
       Llvm.build_store truncated !head !builder |> ignore
    | Ast.Write ->
       let head_value = Llvm.build_load !head "output" !builder in
       let extended = Llvm.build_sext head_value (int_t 32) "extended" !builder in
       Llvm.build_call write [|extended|] "" !builder |> ignore
    | Ast.Loop(body) ->
       let body_block = Llvm.append_block context "body" main in
       let after_block = Llvm.append_block context "after" main in

       let before_value = !head in
       let before_block = Llvm.insertion_block !builder in

       let head_value = Llvm.build_load !head "head_value" !builder in
       let condition = Llvm.build_is_not_null head_value "" !builder in
       Llvm.build_cond_br condition body_block after_block !builder |> ignore;

       let body_builder = Llvm.builder_at_end context body_block in
       builder := body_builder;

       let incoming = [(before_value, before_block)] in
       let phi = Llvm.build_phi incoming "body_phi" body_builder in

       head := phi;

       List.iter gen_ins body |> ignore;

       let end_block = Llvm.insertion_block !builder in
       let end_value = !head in

       let head_value = Llvm.build_load !head "head_value" !builder in
       let condition = Llvm.build_is_not_null head_value "" !builder in
       Llvm.build_cond_br condition body_block after_block !builder |> ignore;
       Llvm.add_incoming (end_value, end_block) phi |> ignore;

       builder := Llvm.builder_at_end context after_block;

       let incoming =
         [
           (before_value, before_block);
           (end_value, end_block)
         ] in
       let after_phi = Llvm.build_phi incoming "after_phi" !builder in

       head := after_phi;

       ignore ()
  in

  List.iter gen_ins program;

  Llvm.build_call free [|tape|] "" !builder |> ignore;
  Llvm.build_ret_void !builder |> ignore;

  jit_module
