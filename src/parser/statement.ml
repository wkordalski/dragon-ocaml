(* Statement ends with Newline!!! *)

type statement = [
| `ExpressionStatement of Expression.expression
| `ReturnStatement of Expression.expression
| `BlockStatement of statement list
]


let rec parse_statement l =
  match l with
  | _ ->
  begin
      try
        let stmts, t = parse_grouped_statements l in
        match t with
        | Token.Newline :: t -> `BlockStatement(stmts), t
        | _ -> raise Node.ParserMatchFailed
      with Node.ParserMatchFailed ->
        raise Node.ParserMatchFailed
  end

and parse_grouped_statements l =
  let rec helper l stmts =
    match l with
    | Token.Operator(";") :: t ->
    begin
        try
          let stmt, t = parse_simple_statement t in
          helper t (stmt :: stmts)
        with Node.ParserMatchFailed -> (List.rev stmts), t
    end
    | _ -> (List.rev stmts), l
  in
  let stmt, t = parse_simple_statement l in
  helper t [stmt]

and parse_simple_statement l =
  match l with
  | Token.Keyword("return") :: t ->
      let expr, t = Expression.parse_expression t in
      (`ReturnStatement(expr), t)
  | _ ->
      let expr, t = Expression.parse_expression l in
      (`ExpressionStatement(expr), t)
