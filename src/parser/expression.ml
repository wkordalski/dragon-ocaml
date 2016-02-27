
type expression = [
| Node.identifier
| `Default
| `GetMemberOperator of (expression * Node.identifier)
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
| `BitAndOperator of (expression * expression)
| `BitXorOperator of (expression * expression)
| `BitOrOperator of (expression * expression)
| `LeftShiftOperator of (expression * expression)
| `RightShiftOperator of (expression * expression)
| `LeftUnsignedShiftOperator of (expression * expression)
| `RightUnsignedShiftOperator of (expression * expression)
| `LessCompareOperator of (expression * expression)
| `GreaterCompareOperator of (expression * expression)
| `LessEqualCompareOperator of (expression * expression)
| `GreaterEqualCompareOperator of (expression * expression)
| `EqualOperator of (expression * expression)
| `NotEqualOperator of (expression * expression)
| `IdentityOperator of (expression * expression)
| `NotIdentityOperator of (expression * expression)
| `IsOperator of (expression * expression)
| `NotIsOperator of (expression * expression)
| `InOperator of (expression * expression)
| `NotInOperator of (expression * expression)
| `AndOperator of (expression * expression)
| `XorOperator of (expression * expression)
| `OrOperator of (expression * expression)
]


let rec parse_expression l = parse_symlogic_or_expression l
and parse_simple_expression l = parse_symlogic_or_expression l (* Must not have assignment and comma operator *)

(* TODO:
 * Where to place 'if ... then ... else ...' and 'try ... except ... then ...'
 * 'as' operator, '..' (range) operator, '->' (lambda) operator
 * compound for expression
*)

(* Logical keyword ops: 'not', then 'and' and 'or' *)
(* Assignment ops *)
(* Tuple comma operator *)
(* Logical symbolic ops *)
and parse_symlogic_or_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("||") :: t ->
        let rhs, t = parse_symlogic_xor_expression t in
        helper (`OrOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_symlogic_xor_expression l in
  helper lhs t


and parse_symlogic_xor_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("^^") :: t ->
        let rhs, t = parse_symlogic_and_expression t in
        helper (`XorOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_symlogic_and_expression l in
  helper lhs t


and parse_symlogic_and_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("&&") :: t ->
        let rhs, t = parse_equality_expression t in
        helper (`AndOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_equality_expression l in
  helper lhs t

(* Equality ops, identity and membership ops *)
and parse_equality_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("==") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`EqualOperator(lhs, rhs)) t
    | Token.Operator("!=") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`NotEqualOperator(lhs, rhs)) t
    | Token.Operator("===") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`IdentityOperator(lhs, rhs)) t
    | Token.Operator("!==") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`NotIdentityOperator(lhs, rhs)) t
    | Token.Keyword("is") :: Token.Keyword("not") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`NotIsOperator(lhs, rhs)) t
    | Token.Keyword("is") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`IsOperator(lhs, rhs)) t
    | Token.Operator("!") :: Token.Keyword("is") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`NotIsOperator(lhs, rhs)) t
    | Token.Keyword("not") :: Token.Keyword("in") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`NotInOperator(lhs, rhs)) t
    | Token.Keyword("in") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`InOperator(lhs, rhs)) t
    | Token.Operator("!") :: Token.Keyword("in") :: t ->
        let rhs, t = parse_compare_expression t in
        helper (`NotInOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_compare_expression l in
  helper lhs t

(* Comparison ops *)

and parse_compare_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("<") :: t ->
        let rhs, t = parse_bitor_expression t in
        helper (`LessCompareOperator(lhs, rhs)) t
    | Token.Operator(">") :: t ->
        let rhs, t = parse_bitor_expression t in
        helper (`GreaterCompareOperator(lhs, rhs)) t
    | Token.Operator("<=") :: t ->
        let rhs, t = parse_bitor_expression t in
        helper (`LessEqualCompareOperator(lhs, rhs)) t
    | Token.Operator(">=") :: t ->
        let rhs, t = parse_bitor_expression t in
        helper (`GreaterEqualCompareOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_bitor_expression l in
  helper lhs t

and parse_bitor_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("|") :: t ->
        let rhs, t = parse_bitxor_expression t in
        helper (`BitOrOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_bitxor_expression l in
  helper lhs t

and parse_bitxor_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("^") :: t ->
        let rhs, t = parse_bitand_expression t in
        helper (`BitXorOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_bitand_expression l in
  helper lhs t

and parse_bitand_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator("&") :: t ->
        let rhs, t = parse_shift_expression t in
        helper (`BitAndOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_shift_expression l in
  helper lhs t

and parse_shift_expression l =
  let rec helper lhs tokens =
    match tokens with
    | Token.Operator(">>") :: t ->
        let rhs, t = parse_add_expression t in
        helper (`RightShiftOperator(lhs, rhs)) t
    | Token.Operator("<<") :: t ->
        let rhs, t = parse_add_expression t in
        helper (`LeftShiftOperator(lhs, rhs)) t
    | Token.Operator(">>>") :: t ->
        let rhs, t = parse_add_expression t in
        helper (`RightUnsignedShiftOperator(lhs, rhs)) t
    | Token.Operator("<<<") :: t ->
        let rhs, t = parse_add_expression t in
        helper (`LeftUnsignedShiftOperator(lhs, rhs)) t
    | _ -> (lhs, tokens)
  in
  let lhs, t = parse_add_expression l in
  helper lhs t

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
