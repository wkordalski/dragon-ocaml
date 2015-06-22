
type 'a t = { value : 'a; mutable parent : 'a t option }

let make v = { value = v; parent = None }
let get {value=v} = v

let rec find n =
  let { value = v; parent = p } = n in
  match p with
  | None -> n
  | Some(q) -> let m = find q in (n.parent <- Some(m); m)

let union n m = n.parent <- Some(find m)
