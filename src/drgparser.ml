open Node

let id = Token.Terminal(1)
let kwd_namespace = Token.Terminal(2)
let indent = Token.Terminal(3)
let dedent = Token.Terminal(4)
let newline = Token.Terminal(5)
let op_dot = Token.Terminal(8)
let kwd_var = Token.Terminal(9)


let namespace_decl = Token.Nonterminal(6)
let id_dot_list = Token.Nonterminal(7)
let variable_decl = Token.Nonterminal(10)
let decl_list = Token.Nonterminal(11)
let decl = Token.Nonterminal(12)
let program = Token.Nonterminal(13)

let node_to_token (n : Node.t) : Token.t =
  match n with
  | Identifier _ -> id
  | Indent -> indent
  | Dedent -> dedent
  | Newline -> newline
  | End -> Token.End
  | Keyword("namespace") -> kwd_namespace
  | Keyword("var") -> kwd_var
  | Operator(".") -> op_dot
  | _ -> failwith "Unknown node"

module TM = Map.Make(Token)
module R = Rule.Make(Token)(Node)

let rmap =
  let add a t p f =
    let cr = try TM.find t a with Not_found -> [] in
    TM.add t ((R.make t p f)::cr) a
  in
  let rmap = TM.empty in

  let rmap = add rmap id_dot_list [id] (function [e] -> IdDotList([e])|_->assert false) in
  let rmap = add rmap id_dot_list [id;op_dot;id_dot_list] (function [e;_;IdDotList(l)]->IdDotList(e::l)|_->assert false) in

  let rmap = add rmap decl_list [decl] (function [e] -> DeclarationList([e])|_->assert false) in
  let rmap = add rmap decl_list [newline] (function [_] -> DeclarationList([])|_->assert false) in
  let rmap = add rmap decl_list [decl;decl_list] (function [e;DeclarationList(l)]->DeclarationList(e::l)|_->assert false) in
  let rmap = add rmap decl_list [newline;decl_list] (function [_;e]->e|_->assert false) in

  let rmap = add rmap variable_decl [kwd_var;id;newline] (function [_;e;_]->VariableDeclaration(e,Unset,Unset)|_->assert false) in
  let rmap = add rmap decl [variable_decl] (function [e]->e|_->assert false) in


  let rmap = add rmap namespace_decl [kwd_namespace;id_dot_list;newline;indent;decl_list;dedent]
    (function [_;IdDotList(k);_;_;DeclarationList(l);_] -> NamespaceDeclaration(k,l)|_->assert false) in
  let rmap = add rmap decl [namespace_decl] (function [e]->e|_->assert false) in

  (* TEMPORARY *)
  let rmap = add rmap program [decl_list] (function [e]->e|_->assert false) in

  rmap

let rtks =
[
  id;
  kwd_namespace;
  indent;
  dedent;
  newline;
  op_dot;
  kwd_var;


  namespace_decl;
  id_dot_list;
  variable_decl;
  decl_list;
  decl;
  program;
]

module FileGrammar =
struct
  type tok = Token.t
  type sem = Node.t
  type rul = R.t

  let start = program
  let tokens = rtks
  let rules t = try TM.find t rmap with Not_found -> []
end
