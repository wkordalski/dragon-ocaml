(*
let lx = Dragon.lex (Stream.of_string "   hello  word\n my lady 10.5e-12.333'444'888\n") in
List.iter (fun (a,b) -> print_string (a^":"^b^"\n")) (Dragon.parse lx)

;;
*)
let someparse = Parser.parse
;;

print_string "\n"
;;
