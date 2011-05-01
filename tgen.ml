open Markov;;

let main () =
  (*match MCB.MarkovDict.lookup (MCB.make_dict (MCB.read "constitution.txt")) ("the","President") with
    | None -> Pervasives.print_int 0
    | Some l -> Pervasives.print_int (List.length l)*)

Pervasives.print_string (MCB.babble ("the","President") (MCB.make_dict (MCB.read "constitution.txt")))
;;

main();;
