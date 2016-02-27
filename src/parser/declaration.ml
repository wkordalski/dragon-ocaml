type decorator = [
| `Decorator of Expression.expression
]
type argument_declaration = [
|`ArgumentDeclaration of (Node.str_t * Expression.expression * Expression.expression)
]
type declaration = [
| `FunctionDeclaration of (Node.str_t * decorator list
													* argument_declaration list * Expression.expression
													* Node.str_t * Statement.statement list)
]


let parse_declaration l =
	let rec parse_decorators l decorators =
		let expr, t = Expression.parse_simple_expression l in
		let decorators = `Decorator(expr) :: decorators in
		match t with
		| Token.Newline :: t -> (List.rev decorators, t)
		| Token.Operator(",") :: t ->parse_decorators t decorators
		| _ -> raise Node.ParserMatchFailed
	in
	let parse_arg_decl l =
		match l with
		| Token.Identifier(id) :: Token.Operator(":") :: t ->
		begin
				let type_expr, t = Expression.parse_simple_expression t in
				match t with
				| Token.Operator("=") :: t ->
						let val_expr, t = Expression.parse_simple_expression t in
						(`ArgumentDeclaration(id, type_expr, val_expr), t)
				| _ -> (`ArgumentDeclaration(id, type_expr, `Default), t)
		end
		| Token.Identifier(id) :: Token.Operator("=") :: t ->
				let val_expr, t = Expression.parse_simple_expression t in
				(`ArgumentDeclaration(id, `Default, val_expr), t)
		| _ -> raise Node.ParserMatchFailed
	in
	let parse_args_decl_list l =
		let rec helper l args = (* Gets arg lists with at least one argument *)
			let arg, t = parse_arg_decl l in
			match t with
			| Token.ParenRoundRight :: t -> (List.rev args), t
			| Token.Operator(",") :: t -> helper t (arg :: args)
			| _ -> raise Node.ParserMatchFailed
		in
		match l with
		| Token.ParenRoundLeft :: Token.ParenRoundRight :: t -> [], t
		| Token.ParenRoundLeft :: t -> helper t []
		| _ -> raise Node.ParserMatchFailed
	in
	let parse_function_body l =
		match l with
		| Token.Indent :: t ->
				let docstring, t = match t with s :: Token.Newline :: t when Token.is_string s -> (Token.get_string s), t | _ -> "", t in
				(* parse statements *)
				let rec helper l stmts =
					match l with
					| Token.Dedent :: t -> (List.rev stmts), t
					| _ ->
							let stmt, t = Statement.parse_statement l in
							helper t (stmt :: stmts)
				in
				let stmts, t = helper t [] in
				(docstring, stmts, t)
		| _ -> raise Node.ParserMatchFailed
	in
	let rec helper l decorators =
		match l with
		| Token.Operator("@") :: t ->
				let decs, t = parse_decorators t [] in
				helper t (decorators @ decs)
		| Token.Keyword("def") :: Token.Identifier(id) :: t ->
		begin
				let args, t = parse_args_decl_list t in
				match t with
				| Token.Operator("->") :: t ->
				begin
						let ret_type, t = Expression.parse_simple_expression t in
						match t with
						| Token.Operator("=") :: t ->
								let ret_val, t = Expression.parse_expression t in
								`FunctionDeclaration(id, decorators, args, ret_type, "", [`ReturnStatement(ret_val)]), t
						| Token.Newline :: t ->
								let docstring, stmts, t = parse_function_body t in
								`FunctionDeclaration(id, decorators, args, ret_type, docstring, stmts), t
						| _ -> raise Node.ParserMatchFailed
				end
				|  _ -> raise Node.ParserMatchFailed
		end
		| _ -> raise Node.ParserMatchFailed
	in
	helper l []
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
