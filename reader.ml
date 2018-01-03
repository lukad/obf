open Angstrom

let is_whitespace c = not (List.mem c ['+'; '-'; '>'; '<'; ','; '.'; '['; ']'])
let ws = many (satisfy is_whitespace)

let chars c f = many1 (char c) >>= f

let plus = chars '+' (fun s -> return (Ast.Add (List.length s)))
let minus = chars '-' (fun s -> return (Ast.Add ~-(List.length s)))
let left = chars '>' (fun s -> return (Ast.Move (List.length s)))
let right = chars '<' (fun s -> return (Ast.Move ~-(List.length s)))
let read = char ',' *> return Ast.Read
let write = char '.' *> return Ast.Write

let simple = (plus <|> minus <|> left <|> right <|> read <|> write) <* ws
let loop program = (char '[' *> program <* char ']') <* ws
                   >>= fun body -> return (Ast.Loop body)

let program = fix (fun program -> ws *> many (simple <|> loop program)) <* end_of_input

let read_string = parse_string program
