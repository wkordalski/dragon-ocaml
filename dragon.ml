
type node = Node.node
type char_t = Node.char_t
type str_t = Node.str_t

let lex = Lexer.lex
let print = Printer.print

let translate_node_to_token n =
  match n with
  | Node.Identifier(s) -> Parser.Identifier(s)
  | _ -> assert false

let translate_obj_to_node n =
  match n with
  | Parser.Obj_Identifier(s) -> Node.Identifier(s)
  | _ -> assert false

let parse ch = (* parses Node.node Stream.t to Node.node *)
  () (* TODO *)
