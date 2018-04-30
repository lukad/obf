type program = instruction list [@@deriving show]

and instruction =
  | Move of int
  | Add of int
  | Set of int
  | Loop of program
  | Read
  | Write
  [@@deriving show];;
