(*
let lx = Dragon.lex (Stream.of_string "   hello  word\n my lady 10.5e-12.333'444'888\n") in
List.iter (fun (a,b) -> print_string (a^":"^b^"\n")) (Dragon.parse lx)

;;
*)
let x = Token.Terminal(1)
and a = Token.Terminal(2)
and q = Token.Terminal(7)
and s = Token.Nonterminal(3)
and n = Token.Nonterminal(4)
and e = Token.Nonterminal(5)
and v = Token.Nonterminal(6)
in
let f (l:Node.t list) : Node.t = Node.Identifier("dummy") in
let rules = [(s,[n],f); (n, [v;q;e],f); (n,[e],f);
             (e,[v],f); (v,[x],f); (v,[a;e],f)] in
Parser.parse (List.map (fun (t,l,f) -> Parser.TRule.make t l f) rules) s
;;
