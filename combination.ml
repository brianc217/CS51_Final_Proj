open Dict;;
open Markov;;


(*  Dictionary mapping words to a list of possible parts of speech
 *)

module PoS = 
  struct
    module PosDict = Make(
      struct 
	type key = string
	type value = string list
	let compare = (fun x y -> String.compare x y)
	let string_of_key = (fun s -> s)
	let string_of_value = (fun l ->
				 let string_of_list l =
				   let rec helper l s =
				     match l with
				       | [] -> s
				       | hd::tl -> helper tl s^hd in
				     helper l "" in 
				   string_of_list l)
      end)
    
    let read filename = 
      let lines = ref [] in
      let chan = open_in filename in
	try
	  while true; do
	    lines := input_line chan :: !lines
	  done; []
	with End_of_file ->
	  close_in chan;
	  List.rev !lines ;; 

    let good_insert d k v = 
      match PosDict.lookup d k with
	| Some l -> PosDict.insert d k (l@v)
	| None -> PosDict.insert d k v ;;

    let make_dict l =
    let rec helper l d =
      match l with
	| a::b::t ->(
	    match (String.uppercase b) with
	      | "APPGE" -> helper t (good_insert d a 
				       ["possesive pronoun, pre-nominal"])
	      | "AT" | "AT1" -> helper t (good_insert d a ["article"])
	      | "BCL" -> helper t (good_insert d a ["before-clause marker"])
	      | "CC" | "CCB" | "CS" | "CSA" | "CSN" | "CST" | "CSW" -> 
		  helper t (good_insert d a ["conjunction"])
	      | "DA" | "DA1" | "DA2" | "DAR" | "DAT" | "DB" | "DB2" | "DD" | 
		    "DD1" | "DD2" | "DDQ" | "DDQGE" | "DDQV" -> 
		  helper t (good_insert d a ["determiner"])
	      | "EX" -> helper t (good_insert d a ["existential there"])
	      | "FO" -> helper t (good_insert d a ["formula"])
	      | "FU" | "FW" -> helper t d
	      | "GE" -> helper t (good_insert d a ["germanic genitive marker"])
	      | "IF" | "II" | "IO" | "IW" -> helper t 
		  (good_insert d a ["preposition"])
	      | "JJ" | "JJR" | "JJT" | "MC" | "MC1" | "MCMC" | "MD" | ""-> 
		  helper t (good_insert d a ["adjective"])
	      | "MC2"|"MCGE"|"MF"|"ND1"|"NN"|"NN1"|"NN2"|"NNA"|"NNB"|"NNL1"|
		    "NNL2"|"NNO"|"NNO2"|"NNT1"|"NNT2"|"NNU"|"NNU1"|"NNU2"|"NP"|
			"NP1"|"NP2"|"NPD1"|"NPD2"|"NPM1"|"NPM2"
			  -> helper t (good_insert d a ["noun"])
	      | "PN"|"PN1"|"PNQO"|"PNQS"|"PNQV"|"PNX1"|"PPGE"|"PPH1"|"PPHO1"|
		    "PPHO2"|"PPHS1"|"PPHS2"|"PPIO1"|"PPIO2"|"PPIS1"|"PPIS2"|
			"PPX"|"PPX2"|"PPY" -> helper t (good_insert d a ["pronoun"])
	      | "RA"|"REX"|"RG"|"RGQ"|"RGQV"|"RGR"|"RGT"|"RL"|"RP"|"RPK"|"RR"|
		    "RRQ"|"RRQV"|"RRR"|"RRT"|"RT "-> helper t 
		  (good_insert d a ["adverb"])
	      | "TO" -> helper t (good_insert d a ["infinitive marker"])
	      | "UH" -> helper t (good_insert d a ["interjection"])
	      | "VB0"|"VBDR"|"VBDZ"|"VBG"|"VBI"|"VBM"|"VBN"|"VBR"|"VBZ"
		    -> helper t (good_insert d a ["be"])
	      | "VD0"|"VDD"|"VDG"|"VDI"|"VDN"|"VDZ"|"VH0"|"VHD"|"VHG"|"VHI"|
		    "VHN"|"VHZ"|"VV0"|"VVD"|"VVG"|"VVGK"|"VVI"|"VVN"|"VVNK"|
			"VVZ"|"VM"|"VMK" -> helper t (good_insert d a ["verb"])
	      | "XX" -> helper t (good_insert d a ["not"])
	      | "ZZ1"|"ZZ2" -> helper t (good_insert d a ["letter"])
	      | anything_else -> helper t d)
	| [] -> d
	| _::[] -> raise (Failure "error in part of speech file")
    in helper l PosDict.empty ;;
    
    (* A helper function that will flatten a list *)
    let rec flatten l : 'a list =
      match l with
	| h::t -> h @ (flatten t)
	| [] -> [] ;;

    (* A helper function that picks a random element from a list *)
    let randomelement (l:'a list) : 'a =
      List.nth l (Random.int (List.length l)) ;;

    let randomsentence() =
      let v () = randomelement([["adverb"; "verb"]; ["verb"]]) in
      let n () = randomelement([["adjective"; "noun"]; ["noun"]]) in
      let np () = randomelement([["determiner"] @ n(); n()]) in
      let prepphrase () = randomelement([["preposition"] @ n()]) in
      let vp () = randomelement([v() @ np(); ["be"]@["adjective"]; v()]) in
      flatten(randomelement([[np() @ vp()]; [np() @ vp() @ prepphrase()]]));;

    exception Not_in_dict;;

    let babble (tokens) (markov:MCB.MarkovDict.dict) (dict:PosDict.dict) =
      let poslist = randomsentence() in

      let deopt a = 
	match a with
	  | None -> []
	  | Some l -> l in
  
      let rec find_token tokens dict poslist =
	let token = Array.get tokens (Random.int (Array.length tokens)) in
	let (a,b) = token in
	  if (List.mem (List.nth poslist 0) (deopt (PosDict.lookup dict a))) && 
	    (List.mem (List.nth poslist 1) (deopt (PosDict.lookup dict b))) 
	  then token
	  else find_token tokens dict poslist in
          
      let helper key markov dict poslist sent =
	let (a,b) = key in
    
	let rec helper2 key markov dict poslist sent int =
	  let (a,b) = key in
	  if int >= List.length poslist then sent else
	  let values = match (MCB.MarkovDict.lookup markov key) with
	    | None -> raise Not_in_dict
	    | Some l -> l in
	
	  let rec iterate list = 
	    match list with
	      | hd::tl -> if (List.mem (List.nth poslist int) 
			(deopt (PosDict.lookup dict hd))) then 
		  hd else iterate tl
	      | [] -> List.nth values (Random.int (List.length values)) in

	  let next = iterate values in
	    if (next = ".") then (sent ^ ".")
	    else (helper2 (b,next) markov dict poslist (sent^" "^next) 
		    (int+1)) in
	  a ^ " " ^ b ^ (helper2 key markov dict poslist sent 2) in
    
      let token = (find_token tokens dict poslist) in
	helper token markov dict poslist ""  ;;    


    let posdict = make_dict (read "newpos.txt");;
    let const_tokens = MCB.key_array MCB.const;;
    let alice_tokens = MCB.key_array MCB.alice;;

end



