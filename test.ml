(*
module Pars = Parser.Make(Token)(Node)(Drgparser.FileGrammar)
(*;;*)
(*Pars.print ()*)
;;
let str = Dragon.lex (Stream.of_string "var k\n\nvar l\n") in
(*Stream.iter Dragon.print_node str*)

let rts = Pars.parse_stream str Drgparser.node_to_token in
if List.length rts <> 0 then print_endline "Some results" else print_endline "Parsing error"
*)
(*
open OUnit2;;

let () =
  run_test_tt_main Parser_test.test_suites
*)

TestLexer.run();
TestParser.run()
