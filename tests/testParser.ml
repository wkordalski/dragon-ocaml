open TestUtils

let get_member_operator_test () =
  let tks = Dragon.Lexer.lex (Stream.of_string "a.b.c\n") in
  let tokens = stream_to_list tks in
  let expr, rest = Dragon.Parser.Expression.parse_postfix_expression tokens in
  match expr with
  | `GetMemberOperator(`GetMemberOperator(`Identifier("a"), `Identifier("b")), `Identifier("c")) -> true
  | _ -> false


  let run () =
    header "Parser tests";
    test "get member operator" get_member_operator_test;
