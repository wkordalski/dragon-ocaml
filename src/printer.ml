
let print node =
  match node with
  | Node.Newline -> "[Newline]"
  | Node.Indent -> "[Indent]"
  | Node.Dedent -> "[Dedent]"
  | Node.Identifier(id) -> "[Identifier: "^id^"]"
  | Node.Number(vl) -> "[Number: "^vl^"]"
  | _ -> "[Undefined]"
