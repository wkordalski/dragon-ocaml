open TestUtils

let parse f s =
  let tks = Dragon.Lexer.lex (Stream.of_string s) in
  let tokens = stream_to_list tks in
  let expr, _ = f tokens in
  expr

let parse_expression s =
  parse (Dragon.Parser.Expression.parse_expression) s

let get_member_operator_test () =
  let expr = parse_expression "a.b.c" in
  match expr with
  | `GetMemberOperator(`GetMemberOperator(`Identifier("a"), `Identifier("b")), `Identifier("c")) -> true
  | _ -> false

let increase_operator_test () =
  let expr = parse_expression "x++" in
  match expr with
  | `PostfixIncreaseOperator(`Identifier("x")) -> true
  | _ -> false

let run () =
    header "Parser tests";
    test "get member operator" get_member_operator_test;
    test "postfix increase operator" increase_operator_test;
