let test n f = print_string("  [ " ^ (if f () then "OK" else "WA") ^ " ]  " ^ n ^ "\n")
let header s = print_string (s^" ---\n")

let rec compare_streams p q =
  let a = Stream.peek p and b = Stream.peek q in
  match a, b with
  | (None, None) -> true
  | (None, _) -> false
  | (_, None) -> false
  | (Some(x), Some(y)) -> let _ = Stream.junk p, Stream.junk q in Dragon.Token.equal x y && compare_streams p q

let stream_to_list s =
  let rec helper acc =
    match Stream.peek s with
    | Some(e) -> let _ = Stream.junk s in helper (e :: acc)
    | None -> List.rev acc
  in helper []
