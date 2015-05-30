(*
let lx = Dragon.lex (Stream.of_string "   hello  word\n my lady 10.5e-12.333'444'888\n") in
List.iter (fun (a,b) -> print_string (a^":"^b^"\n")) (Dragon.parse lx)

;;
*)
let x = Parser.Terminal(1)
and a = Parser.Terminal(2)
and q = Parser.Terminal(7)
and s = Parser.Nonterminal(3)
and n = Parser.Nonterminal(4)
and e = Parser.Nonterminal(5)
and v = Parser.Nonterminal(6)
in
let f l = Node.Identifier("dummy") in
let rules = [Parser.Rule(s,[n],f); Parser.Rule(n, [v;q;e],f); Parser.Rule(n,[e],f);
             Parser.Rule(e,[v],f); Parser.Rule(v,[x],f); Parser.Rule(v,[a;e], f)] in
Parser.parse rules s
;;
