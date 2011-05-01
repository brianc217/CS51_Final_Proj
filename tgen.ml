open Markov;;
open Combination;;

let main () =
  (*match MCB.MarkovDict.lookup (MCB.make_dict (MCB.read "constitution.txt")) ("the","President") with
    | None -> Pervasives.print_string "Zero"
    | Some l -> print_string (List.fold_left (fun hd agg -> hd^ " " ^agg) "" l)
  *)
  let list = MCB.read "constitution.txt" in
  let tokens = MCB.key_array list in
  let markovd = MCB.make_dict list in
(*print_string (List.fold_left (fun hd agg -> hd^ " " ^agg) "" list);;*)
  let posd = PoS.make_dict (PoS.read "pos.txt") in

    Pervasives.print_string (PoS.babble tokens markovd posd);;

main();;
