(*
let lx = Dragon.lex (Stream.of_string "   hello  word\n my lady 10.5e-12.333'444'888\n") in
List.iter (fun (a,b) -> print_string (a^":"^b^"\n")) (Dragon.parse lx)

;;
*)
module GramMod =
struct
  let x = Token.Terminal(1)
  let a = Token.Terminal(2)
  let q = Token.Terminal(7)
  let s = Token.Nonterminal(3)
  let n = Token.Nonterminal(4)
  let e = Token.Nonterminal(5)
  let v = Token.Nonterminal(6)
  let f (l:Node.t list) : Node.t = Node.Identifier("dummy")

  module R = Rule.Make(Token)(Node)

  type tok = Token.t
  type sem = Node.t
  type rul = R.t
  
  let start = s
  let rules t =
    if Token.equal t s then [R.make s [n] f] else
    if Token.equal t n then [R.make n [v;q;e] f; R.make n [e] f] else
    if Token.equal t e then [R.make e [v] f] else
    if Token.equal t v then [R.make v [x] f; R.make v [a;e] f] else
    []
  let tokens = [x;a;q;s;n;e;v]
end

module Pars = Parser.Make(Token)(Node)(GramMod)