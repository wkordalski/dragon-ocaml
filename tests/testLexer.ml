open TestUtils

let simple_test () =
  let data = Dragon.Lexer.lex (Stream.of_string "var k\n") in
  compare_streams data (Stream.of_list [ Dragon.Token.Keyword ("var"); Dragon.Token.Identifier("k"); Dragon.Token.Newline; Dragon.Token.Newline; Dragon.Token.End ])

let dot_operator_test () =
  let data = Dragon.Lexer.lex (Stream.of_string "k.x\n") in
  compare_streams data (Stream.of_list
    [ Dragon.Token.Identifier ("k"); Dragon.Token.Operator("."); Dragon.Token.Identifier("x");
    Dragon.Token.Newline; Dragon.Token.Newline; Dragon.Token.End ])


let run () =
  header "Lexer tests";
  test "simple" simple_test;
  test "dot operator" dot_operator_test;
