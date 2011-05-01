open Dict;;

(*  Dictionary mapping two word tuples to a list of possible words that can
 *  follow those two words
 *)

module MCB = 
  struct
    module MarkovDict = Make(
      struct 
	type key = (string * string)
	type value = string list
	let compare = (fun x y -> let (a,b) = x in
		       let (c,d) = y in
			 String.compare (a^b) (c^d))
	let string_of_key = (fun s -> let (a,b) = s in a^ " " ^b)
	let string_of_value = (fun l ->
				 let string_of_list l =
				   let rec helper l s =
				     match l with
				       | [] -> s
				       | hd::tl -> helper tl s^hd in
				     helper l "" in 
				   string_of_list l)
      end)
    let read (file:string) : string list =
      let channel = open_in file in   
      let rec helper (word:string) (list:string list) : string list =
	let char = try Some(input_char channel) with End_of_file -> None in
	  match char with
	    | Some c -> 
		begin
		  match c with 
		    | ' ' | '\n' | '\r' -> 
			(begin 
			   match word with
			     | "" -> helper "" list
			     | word -> helper "" (list@[word])
			 end)
		    | '.' -> helper "" (list@[word]@["."])
		    | c -> helper (word^ String.make 1 c) list 
		end
            | None -> list in
	helper "" [] ;;

    let key_array (lst:string list) : MarkovDict.key array = 
      let rec helper (lst:string list) (tklst: MarkovDict.key list) :
	  MarkovDict.key list = 
	match lst with
	  | h1::h2::h3::tl -> if (List.mem (h1,h2) tklst)
	    then 
	      helper (h2::h3::tl) tklst
	    else
	      helper (h2::h3::tl) ((h1,h2)::tklst)
	  | h1::h2::tl -> if List.mem (h1,h2) tklst then tklst
	    else (h1,h2)::tklst
	  | [] -> tklst
      in Array.of_list (helper lst []) ;;

    let good_insert d k v = 
      match MarkovDict.lookup d k with
	| Some l -> MarkovDict.insert d k (l@v)
	| None -> MarkovDict.insert d k v ;;

    let make_dict (list:string list) : MarkovDict.dict = 
      let dict = MarkovDict.empty in
      let rec helper list dict =
	match list with
          | hd1::hd2::hd3::tl -> helper (hd2::hd3::tl) 
	    (good_insert dict (hd1,hd2) [hd3])
          | hd1::hd2::tl -> good_insert dict (hd1,hd2) tl
	  | [] -> dict in
	helper list dict ;;

    exception Not_in_dict;;

    let rec babble (k:MarkovDict.key) (d:MarkovDict.dict) : string = 
      let (a,b) = k in
      let rec helper (k:MarkovDict.key) (d:MarkovDict.dict) (s:string) 
	  :string =
	let (a,b) = k in
	let randomelement l = 
          List.nth l (Random.int (List.length l)) in
	let values = match (MarkovDict.lookup d k) with
          |Some l -> l
          |None -> raise Not_in_dict in 
	let next = randomelement values in 
          if (next = ".") then (s ^ ".") 
          else (helper (b,next) d (s ^ " " ^ next)) in
	a ^ " " ^ b ^ (helper k d "") ;;

end



