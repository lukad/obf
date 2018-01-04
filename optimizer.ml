open Ast

let rec opt = function
  | [] -> []

  | Add 0  :: rest -> opt rest
  | Move 0 :: rest -> opt rest

  | Add a  :: Add b  :: rest -> Add (a + b) :: opt rest
  | Move a :: Move b :: rest -> Add (a + b) :: opt rest

  | Loop []   :: rest ->                    opt rest
  | Loop body :: rest -> Loop (opt body) :: opt rest

  | ins :: rest -> ins :: (opt rest)

let optimize prog =
  let rec optimize a b = if a = b then a else optimize b (opt b) in
  optimize prog (opt prog)
