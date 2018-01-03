type program = ast list [@@deriving show]

and ast =
  | Move of int
  | Add of int
  | Loop of program
  | Read
  | Write
  [@@deriving show];;
