Stream.iter (fun n -> print_string (Dragon.print n))
            (Dragon.lex (Stream.of_string "   hello  word\n my lady\n"))
;;


print_string "\n"
;;
