
open Token
(*
let rec skip_newlines_and_comments tokens =
	match tokens with
	| Newline::t
	| LineComment(_)::t
	| BlockComment(_)::t
	| NestedComment(_)::t -> skip_newlines_and_comments t
	| _ -> tokens

let rec skip_comments tokens =
	match tokens with
	| LineComment(_)::t
	| BlockComment(_)::t
	| NestedComment(_)::t -> skip_comments t
	| _ -> tokens



let parse_var_definition tokens =
	let tokens = skip_comments tokens in
	match tokens with
	| Identifier(id) ->
			let tokens = skip_comments tokens in
			match tokens with
			| Operator(':') ->

			() (*parse type or init value*)
	| _ -> assert(false)	(*should throw compilation error*)
(*
let parse_def_definition tokens = ()
let parse_pro_definition tokens = ()
let parse_get_definition tokens = ()
let parse_set_definition tokens = ()
*)

let parse_definition tokens = (* tokens, definition *)
	let tokens = skip_newlines_and_comments tokens in
	match tokens with
	| Keyword('var')::t -> parse_var_definition t
	(*
	| Keyword('def')::t -> parse_def_definition t
	| Keyword('pro')::t -> parse_pro_definition t
	| Keyword('get')::t -> parse_get_definition t
	| Keyword('set')::t -> parse_set_definition t
	*)
	| _ -> assert(false)

let parse_file l = ()
*)

let bye s = print_string (s)
