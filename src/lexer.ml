let is_digit c = (c >= '0' && c <= '9')
let is_vletter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_letter c =  (is_vletter c) || (c = '_')
let is_hexdigit c = is_digit c || is_vletter c
let is_alpha c = is_digit c || is_letter c
let is_newline c = (c = '\n')
let is_space c = (c = ' ') || (c = '\t')

let space_width c =
  match c with
  | ' '  -> 1
  | '\t' -> 2
  |  _   -> failwith "Not a space character"

let skip_indentation ch =
  let rec helper acc =
    match Stream.peek ch with
    | None -> 0
    | Some(cc)->
        if is_newline cc then (Stream.junk ch; helper 0) else
        if is_space cc then (Stream.junk ch; helper (acc + space_width cc))
        else acc
  in helper 0

let rec fix_indentation indent indents buffer = (* adds/removes indentation from parents and add tokens to buffer *)
  let h = List.hd (!indents) in
  if h < indent then
  (
    indents :=  indent::(!indents);
    Queue.push Node.Indent buffer
  )
  else
  if h > indent then
  (
    indents := List.tl (!indents);
    Queue.push Node.Dedent buffer;
    fix_indentation indent indents buffer
  )

let rec skip_spaces ch =
  match Stream.peek ch with
  | None -> ()
  | Some(cc) ->
      if is_space cc then (Stream.junk ch; skip_spaces ch)
      else ()

let read_identifier ch =
  (* TODO *)
  let rec helper acc =
    match Stream.peek ch with
    | None -> acc
    | Some(c) when is_alpha c -> (Stream.junk ch; helper (acc^(String.make 1 c)))
    | Some(_) -> acc
  in
  match Stream.peek ch with
  | None -> assert false
  | Some(c) ->
  (
    Stream.junk ch;
    match helper (String.make 1 c) with
    | "var" -> Some(Node.KeywordVar)
    | s -> Some(Node.Identifier(s))
  )

let read_number ch =
  let rec read_based_number acc =
    match Stream.peek ch with
    | None -> acc
    | Some(c) when is_hexdigit c || c ='\'' ->
    (
      Stream.junk ch;
      read_based_number (acc^(String.make 1 c))
    )
    | Some(_) -> acc
  and read_decimal_number acc allow_dot =
    match Stream.peek ch with
    | None -> acc
    | Some(c) when is_digit c || c = '\'' ->
    (
      Stream.junk ch;
      read_decimal_number (acc^(String.make 1 c)) allow_dot
    )
    | Some(c) when c = '.' && allow_dot ->
    (
      Stream.junk ch;
      read_decimal_number (acc^".") false
    )
    | Some(_) -> acc
  in
  let read_exponential_number acc =
    let acc = read_decimal_number acc true
    in
    match Stream.peek ch with
    | None -> acc
    | Some(c) when c = 'e' || c = 'E' ->
    (
      let acc = acc ^ (String.make 1 c) in
      Stream.junk ch;
      match Stream.peek ch with
      | None -> failwith "Lexing error: number parsing error"
      | Some(c) when c = '+' || c = '-' ->
      (
        Stream.junk ch;
        read_decimal_number (acc^(String.make 1 c)) true
      )
      | Some(c) when is_digit c ->
      (
        read_decimal_number acc true
      )
      | Some(_) -> failwith "Lexing error: number parsing error"
    )
    | Some(_) -> acc
  in
  match Stream.peek ch with
  | None -> assert false
  | Some('0')->
  (
    Stream.junk ch;
    match Stream.peek ch with
    | None -> Some(Node.Number ("0"))
    | Some(c) when is_vletter c ->
    (
      Stream.junk ch;
      Some(Node.Number (read_based_number("0"^(String.make 1 c))))
    )
    | Some(c) when is_digit c || c = '\'' || c = '.' ->
    (
      Some(Node.Number (read_exponential_number "0"))
    )
    | Some(_) -> Some(Node.Number("0"))
  )
  | Some(c) when is_digit c ->
  (
    Some(Node.Number (read_exponential_number ""))
  )
  | Some(_) -> assert false

let mk_lexer ch =
  (* Some usefull state variables *)
  let buffer = Queue.create ()
  and is_newline_tag = ref true
  and indents = ref [0]
  and parens = ref []
  and do_last_newline = ref true in
  let rec next_token x =
    (* Something in buffer *)
    if not (Queue.is_empty buffer) then Some(Queue.pop buffer) else
    (* Newline so emit inden/dedent *)
    if !is_newline_tag then
    (
      let spaces = skip_indentation ch in
      if spaces <> List.hd (!indents) then
      (
        fix_indentation spaces indents buffer;
        is_newline_tag := false;
        Some(Queue.pop buffer)
      )
      else
      (
        is_newline_tag := false;
        next_token x
      )
    )
    else
    (
      (* Skip spaces *)
      skip_spaces ch;
      (* Is it newline? *)
      match Stream.peek ch with
      | None ->
      (
        if !do_last_newline then (do_last_newline := false; Some(Node.Newline)) else
        match !indents with
        | h::t when h > 0 -> (indents := t; Some(Node.Dedent))
        | _ -> None
      )
      | Some('\n') ->
      (
        Stream.junk ch;
        is_newline_tag := true;
        Some(Node.Newline)
      )
      | Some(c) when is_letter c -> read_identifier ch
      | Some(c) when is_digit c  -> read_number ch
      | _ -> failwith "Lexing error: unknown starting token"
    )
  in next_token


let lex ch = (* stream of nodes *)
  (* Use Stream.next to get next character *)
  (* Then lex it to get tokens *)
  Stream.from (mk_lexer ch)
