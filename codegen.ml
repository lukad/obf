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
  ignore free;
  ignore calloc;
  ignore read;
  ignore write;

  let main_type = Llvm.function_type (Llvm.void_type context) [||] in
  let main = Llvm.declare_function "main" main_type jit_module in

  let block = ref (Llvm.append_block context "" main) in
  let builder = ref (Llvm.builder_at_end context !block) in

  let ptr = Llvm.build_alloca (Llvm.pointer_type cell_type) "ptr" !builder in
  let data_ptr = Llvm.build_call calloc [|int 64 tape_size; int 64 1|] "data_ptr" !builder in

  Llvm.build_store data_ptr ptr !builder |> ignore;

  let rec gen_ins = function
    | Ast.Move i ->
       let index = Llvm.build_load ptr "index" !builder in
       let new_index = Llvm.build_gep index [|index_value i|] "new_index" !builder in
       Llvm.build_store new_index ptr !builder |> ignore

    | Ast.Add i ->
       let index = Llvm.build_load ptr "index" !builder in
       let value = Llvm.build_load index "value" !builder in
       let result = Llvm.build_add value (cell_value i) "add_result" !builder in
       Llvm.build_store result index !builder |> ignore

    | Ast.Read ->
       let input = Llvm.build_call read [||] "" !builder in
       let truncated = Llvm.build_trunc input cell_type "" !builder in
       let index = Llvm.build_load ptr "" !builder in
       Llvm.build_store truncated index !builder |> ignore

    | Ast.Write ->
       let index = Llvm.build_load ptr "" !builder in
       let value = Llvm.build_load index "" !builder in
       let extended = Llvm.build_sext value (int_t 32) "" !builder in
       Llvm.build_call write [|extended|] "" !builder |> ignore

    | Ast.Loop(body) ->
       let condition_block = Llvm.append_block context "condition" main in
       let body_block = Llvm.append_block context "body" main in
       let after_block = Llvm.append_block context "after" main in

       Llvm.build_br condition_block !builder |> ignore;

       builder := Llvm.builder_at_end context condition_block;

       let index = Llvm.build_load ptr "" !builder in
       let value = Llvm.build_load index "" !builder in

       let condition = Llvm.build_is_not_null value "" !builder in
       Llvm.build_cond_br condition body_block after_block !builder |> ignore;

       builder := Llvm.builder_at_end context body_block;
       List.iter gen_ins body |> ignore;

       Llvm.build_br condition_block !builder |> ignore;

       builder := Llvm.builder_at_end context after_block;
  in

  List.iter gen_ins program;
  Llvm.build_ret_void !builder |> ignore;

  jit_module
