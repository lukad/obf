open Ast

let rec opt = function
  | [] -> []

  | Add 0  :: rest -> opt rest
  | Move 0 :: rest -> opt rest

  | Add a  :: Add b  :: rest -> Add (a + b)  :: opt rest
  | Move a :: Move b :: rest -> Move (a + b) :: opt rest

  | Loop [(Add -1)]         :: rest -> Set 0           :: opt rest
  | Loop body :: Loop _body :: rest -> Loop (opt body) :: opt rest
  | Loop body               :: rest -> Loop (opt body) :: opt rest

  | Set a :: Add b  :: rest -> Set (a + b) :: opt rest
  | Set _ :: Set a  :: rest -> Set a       :: opt rest
  | Set 0 :: Loop _ :: rest -> Set 0       :: opt rest
  | Add _ :: Set a  :: rest -> Set a       :: opt rest

  | ins :: rest -> ins :: (opt rest)

let optimize prog =
  let rec optimize a b =
    if a = b then
      a
    else
      optimize b (opt b)
  in
  optimize prog (opt prog)
