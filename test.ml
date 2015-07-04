
let lx = Dragon.lex (Stream.of_string "   hello `witaj`  word++ /* 1+a*b Ędward Ącki czy jak mu \n tam...*/ ++# + +3\n     my ```sweat\nand\ngreat``` hero\n my lady 10.5e-12.333'444'888\n") in
Stream.iter (fun e -> Dragon.print_node e) lx

(*
let lx = Dragon.lex (Stream.of_string "some + comments /# test /* where /# nested comments #/# /# are /# so\n /# usefull #/ like #/#/# that #/ code\n") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
(*
let lx = Dragon.lex (Stream.of_string "  some (code\nnewlined\n    but no)\n   indent {right\nnow} and ({[so\n]on\n}here)\nis going\n") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
(*
let lx = Dragon.lex (Stream.of_string "some \'text is \\n here 1.234e-5 to \\\\\' parse") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
(*
let lx = Dragon.lex (Stream.of_string "some \'\'\'text\nmultiline\n is'here' \\n here 1.234e-5 to \\\\\'\'\' parse") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
(*
let lx = Dragon.lex (Stream.of_string "some \"augumented [code] string [help.me(\"\\[some [make.free(\"power\")] string\\]\")]\" literal") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
(*
let lx = Dragon.lex (Stream.of_string "some \"\"\"text\nmultiline\n is\"here\" [some.function(\"call\")] \n\\n here 1.234e-5 to \\\"\"\"\" parse") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
(*
let lx = Dragon.lex (Stream.of_string "some\\\n  code\\\n  \n \n       \nto\n  parse") in
Stream.iter (fun e -> Dragon.print_node e) lx
*)
;;
(*

module SNode =
struct
  type t =
    | Number of int
    | OpPlus
    | OpTimes
    | Ending

  let print v =
    match v with
    | Number(x) -> print_int x
    | OpPlus -> print_string "+"
    | OpTimes -> print_string "*"
    | Ending -> print_string "<$>"
end

module GramMod =
struct
  let n = Token.Terminal(1)
  let p = Token.Terminal(2)
  let t = Token.Terminal(3)

  let s = Token.Nonterminal(4)
  let m = Token.Nonterminal(5)

  module R = Rule.Make(Token)(SNode)

  type tok = Token.t
  type sem = SNode.t
  type rul = R.t

  let start = s
  let rules tok =
    if Token.equal tok s then [R.make s [m] (fun [v]->v); R.make s [s;p;m] (fun [SNode.Number(t);_;SNode.Number(v)]->SNode.Number(t+v))] else
    if Token.equal tok m then [R.make m [n] (fun [v]->v); R.make m [m;t;n] (fun [SNode.Number(t);_;SNode.Number(v)]->SNode.Number(t*v))] else
    []
  let tokens = [n;p;t;s;m]

  let cfunc node =
    match node with
    | SNode.Number _ -> n
    | SNode.OpPlus -> p
    | SNode.OpTimes -> t
    | SNode.Ending -> Token.ending
end

module Pars = Parser.Make(Token)(SNode)(GramMod)
;;
let file = [SNode.Number(3); SNode.OpPlus; SNode.Number(5); SNode.OpTimes; SNode.Number(2);SNode.OpPlus; SNode.Number(2);SNode.Ending] in
let s = Stream.of_list file in
let r = Pars.parse_stream s GramMod.cfunc in
List.iter SNode.print r
;;
*)
