type identifier = [
| `Identifier of Node.str_t
]

type expression = [
| identifier
| `GetMemberOperator of (expression * identifier)
]


let rec parse_expression l = failwith "Not implemented"

(*
 * Postfix expression
 * | primary expression '.' identifier
 *)

and parse_postfix_expression l =
  let rec helper (left:expression) (tokens:Token.t list) : (expression * Token.t list) =
    match tokens with
    | Token.Operator(".") :: t ->
    begin
        match t with
        | Token.Identifier(s) :: t -> helper (`GetMemberOperator(left, `Identifier(s))) t
        | _ -> raise Node.ParserMatchFailed
    end
    | _ -> (left, tokens)
  in
  let leftmost, l = parse_primary_expression l
  in helper leftmost l

and parse_primary_expression (l:Token.t list) : (expression * Token.t list) =
  match l with
  (* Rule :: identifier *)
  | Token.Identifier(s) :: t -> (`Identifier(s), t)
  (* Rule :: (expression) *)
  | Token.ParenRoundLeft :: t ->
  begin
      let expr, t = parse_expression t in
      match t with
      | Token.ParenRoundRight :: t -> (expr, t)
      | _ -> raise Node.ParserMatchFailed
  end
  | _ -> raise Node.ParserMatchFailed
