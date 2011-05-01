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
  let posd = PoS.make_dict (PoS.read "newpos.txt") in
  (*let poslist = PoS.randomsentence () in*)
  let print_list l = print_string ((List.fold_left (fun hd agg -> hd^ " " ^agg) "" l)^ "\n") in
    (*print_list poslist;*)
    match (PoS.PosDict.lookup posd "in") with 
      | None -> print_string "Zero"
      | Some l -> print_list l ;
    print_string ((PoS.babble tokens markovd posd ) ^ "\n");
    print_string ((PoS.babble tokens markovd posd ) ^ "\n");
print_string ((PoS.babble tokens markovd posd ) ^ "\n");
print_string ((PoS.babble tokens markovd posd ) ^ "\n");
print_string ((PoS.babble tokens markovd posd ) ^ "\n");
print_string ((PoS.babble tokens markovd posd ) ^ "\n");
print_string ((PoS.babble tokens markovd posd ) ^ "\n");
print_string ((PoS.babble tokens markovd posd ) ^ "\n");;

main();;
