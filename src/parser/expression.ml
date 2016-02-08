type identifier = [
| `Identifier of Node.str_t
]

type expression = [
| identifier
| `GetMemberOperator of (expression * identifier)
| `PostfixIncreaseOperator of (expression)
| `PostfixDecreaseOperator of (expression)
| `PrefixIncreaseOperator of (expression)
| `PrefixDecreaseOperator of (expression)
]


let rec parse_expression l = parse_prefix_expression l

(*
 * Prefix expression
 * | '++' prefix expression
 * | '--' prefix expression
 *)

and parse_prefix_expression l =
  match l with
  | Token.Operator("++") :: t ->
      let rhs, t = parse_prefix_expression t in
      (`PrefixIncreaseOperator(rhs), t)
  | Token.Operator("--") :: t ->
      let rhs, t = parse_prefix_expression t in
      (`PrefixDecreaseOperator(rhs), t)
  | _ -> parse_postfix_expression l

(*
 * Postfix expression
 * | postfix expression '.' identifier
 * | postfix expression '++'
 * | postfix expression '--'
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
    | Token.Operator("++") :: t -> helper (`PostfixIncreaseOperator(left)) t
    | Token.Operator("--") :: t -> helper (`PostfixDecreaseOperator(left)) t
    | _ -> (left, tokens)
  in
  let leftmost, l = parse_primary_expression l
  in helper leftmost l


  (*
   * Primary expression
   * | identifier
   * | '(' expression ')'
   *)

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
