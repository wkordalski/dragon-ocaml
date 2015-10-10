open Node

let id = Token.Terminal(1)
let kwd_namespace = Token.Terminal(2)
let indent = Token.Terminal(3)
let dedent = Token.Terminal(4)
let newline = Token.Terminal(5)
let op_dot = Token.Terminal(8)
let kwd_var = Token.Terminal(9)
let op_comma = Token.Terminal(18)
let op_equal = Token.Terminal(19)
let op_colon = Token.Terminal(20)

let namespace_decl = Token.Nonterminal(6)
let id_dot_list = Token.Nonterminal(7)
let variable_decl = Token.Nonterminal(10)
let decl_list = Token.Nonterminal(11)
let decl = Token.Nonterminal(12)
let program = Token.Nonterminal(13)
let expr = Token.Nonterminal(14)
let pattern = Token.Nonterminal(15)
let single_var_decl = Token.Nonterminal(16)
let var_decls = Token.Nonterminal(17)

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
  | Operator(",") -> op_comma
  | Operator("=") -> op_equal
  | Operator(":") -> op_colon
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

  let rmap = add rmap single_var_decl [id;op_equal;expr] (function [p;_;v] -> SingleVariableDeclaration(p,Unset,v)|_->assert false) in
  let rmap = add rmap single_var_decl [id;op_colon;expr] (function [p;_;t] -> SingleVariableDeclaration(p,t,Unset)|_->assert false) in
  let rmap = add rmap single_var_decl [id;op_colon;expr;op_equal;expr]
    (function [p;_;t;_;v] -> SingleVariableDeclaration(p,t,v)|_->assert false) in

  let rmap = add rmap var_decls [single_var_decl] (function [SingleVariableDeclaration(e)] -> VariablesDeclaration([e])|_->assert false) in
  let rmap = add rmap var_decls [single_var_decl;op_comma;var_decls]
    (function [SingleVariableDeclaration(e);_;VariablesDeclaration(l)]-> VariablesDeclaration(e::l)|_->assert false) in

  let rmap = add rmap variable_decl [kwd_var;var_decls;newline] (function [_;e;_]->e|_->assert false) in
  let rmap = add rmap decl [variable_decl] (function [e]->e|_->assert false) in

  let rmap = add rmap pattern [id] (function [e] -> IdPattern(e)|_->assert false) in

  let rmap = add rmap namespace_decl [kwd_namespace;id_dot_list;newline;indent;decl_list;dedent]
    (function [_;IdDotList(k);_;_;DeclarationList(l);_] -> NamespaceDeclaration(k,l)|_->assert false) in
  let rmap = add rmap decl [namespace_decl] (function [e]->e|_->assert false) in

  (* TEMPORARY *)
  let rmap = add rmap expr [id] (function [_] -> Indent|_->assert false) in
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
  op_comma;
  op_equal;
  op_colon;

  namespace_decl;
  id_dot_list;
  variable_decl;
  decl_list;
  decl;
  program;
  expr;
  pattern;
  single_var_decl;
  var_decls;
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
