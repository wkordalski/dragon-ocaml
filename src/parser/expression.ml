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
| `PowerOperator of (expression * expression)
| `UnaryPlusOperator of (expression)
| `UnaryMinusOperator of (expression)
| `UnaryComplementOperator of (expression)
| `MultiplyOperator of (expression * expression)
| `DivideOperator of (expression * expression)
| `ModuloOperator of (expression * expression)
| `PlusOperator of (expression * expression)
| `MinusOperator of (expression * expression)
| `CatOperator of (expression * expression)
]


let rec parse_expression l = parse_add_expression l

(*
 * Add expression
 * | add expression '*' multiply expression
 * | add expression '/' multiply expression
 * | add expression '%' multiply expression
 * | multiply expression
 *)
and parse_add_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("+") :: t ->
        let rhs, t = parse_mul_expression t in
        helper (`PlusOperator(lhs, rhs)) t
    | Token.Operator("-") :: t ->
        let rhs, t = parse_mul_expression t in
        helper (`MinusOperator(lhs, rhs)) t
    | Token.Operator("~") :: t ->
        let rhs, t = parse_mul_expression t in
        helper (`CatOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_mul_expression l in
  helper lhs t

(*
 * Multiply expression
 * | multiply expression '*' unary expression
 * | multiply expression '/' unary expression
 * | multiply expression '%' unary expression
 * | unary expression
 *)
and parse_mul_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("*") :: t ->
        let rhs, t = parse_unary_expression t in
        helper (`MultiplyOperator(lhs, rhs)) t
    | Token.Operator("/") :: t ->
        let rhs, t = parse_unary_expression t in
        helper (`DivideOperator(lhs, rhs)) t
    | Token.Operator("%") :: t ->
        let rhs, t = parse_unary_expression t in
        helper (`ModuloOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_unary_expression l in
  helper lhs t

(*
 * Unary expression
 * | '+' unary expression
 * | '-' unary expression
 * | '~' unary expression
 * | power expression
 *)
and parse_unary_expression l =
  match l with
  | Token.Operator("+") :: t ->
      let rhs, t = parse_unary_expression t in
      (`UnaryPlusOperator(rhs), t)
  | Token.Operator("-") :: t ->
      let rhs, t = parse_unary_expression t in
      (`UnaryMinusOperator(rhs), t)
  | Token.Operator("~") :: t ->
      let rhs, t = parse_unary_expression t in
      (`UnaryComplementOperator(rhs), t)
  | _ -> parse_power_expression l

(*
 * Power expression
 * | prefix expression '**' power expression
 * | prefix expression
 *)

and parse_power_expression l =
  let lhs, t = parse_prefix_expression l in
  match t with
  | Token.Operator("**") :: t ->
      let rhs, t = parse_power_expression t in
      (`PowerOperator(lhs, rhs), t)
  | _ -> (lhs, t)

(*
 * Prefix expression
 * | '++' prefix expression
 * | '--' prefix expression
 * | postfix expression
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
