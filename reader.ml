open Angstrom

let is_comment c = not (List.mem c ['+'; '-'; '>'; '<'; ','; '.'; '['; ']'])
let comment = many (satisfy is_comment)

let chars c f = many1 (char c) >>= f

let plus = chars '+' (fun s -> return (Ast.Add (List.length s)))
let minus = chars '-' (fun s -> return (Ast.Add ~-(List.length s)))
let left = chars '>' (fun s -> return (Ast.Move (List.length s)))
let right = chars '<' (fun s -> return (Ast.Move ~-(List.length s)))
let read = char ',' *> return Ast.Read
let write = char '.' *> return Ast.Write

let simple = (plus <|> minus <|> left <|> right <|> read <|> write) <* comment
let loop program = (char '[' *> program <* char ']') <* comment
                   >>= fun body -> return (Ast.Loop body)

let program = fix (fun program -> comment *> many (simple <|> loop program)) <* end_of_input

let read_string = parse_string program
