open MCB;;

module type DICT = 
sig
  type key   
  type value 
  type dict
  val empty : dict 
  val insert : dict -> key -> value -> dict
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool
end

module PoS : DICT = 
struct
  module M = Map.Make(String)
  type key = string
  type value = string list
  type dict = value M.t
  let empty = M.empty
  let lookup d k = try Some (M.find k d) with Not_found -> None
  let insert d k v  = 
    match (lookup d k) with
      | Some l -> M.add k (l@v) d  
      | None -> M.add k v d
  let member d k = M.mem k d
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
  let make_dict l =
    let rec helper l d =
      match l with
	| a::b::t ->(
	    match (String.uppercase b) with
	      | "APPGE" -> helper t (insert d a 
				       ["possesive pronoun, pre-nominal"])
	      | "AT" | "AT1" -> helper t (insert d a ["article"])
	      | "BCL" -> helper t (insert d a ["before-clause marker"])
	      | "CC" | "CCB" | "CS" | "CSA" | "CSN" | "CST" | "CSW" -> 
		  helper t (insert d a ["conjunction"])
	      | "DA" | "DA1" | "DA2" | "DAR" | "DAT" | "DB" | "DB2" | "DD" | 
		    "DD1" | "DD2" | "DDQ" | "DDQGE" | "DDQV" -> 
		  helper t (insert d a ["determiner"])
	      | "EX" -> helper t (insert d a ["existential there"])
	      | "FO" -> helper t (insert d a ["formula"])
	      | "FU" | "FW" -> helper t d
	      | "GE" -> helper t (insert d a ["germanic genitive marker"])
	      | "IF" | "II" | "IO" | "IW" -> helper t 
		  (insert d a ["preposition"])
	      | "JJ" | "JJR" | "JJT" | "MC" | "MC1" | "MCMC" | "MD" | ""-> 
		  helper t (insert d a ["adjective"])
	      | "MC2"|"MCGE"|"MF"|"ND1"|"NN"|"NN1"|"NN2"|"NNA"|"NNB"|"NNL1"|
		    "NNL2"|"NNO"|"NNO2"|"NNT1"|"NNT2"|"NNU"|"NNU1"|"NNU2"|"NP"|
			"NP1"|"NP2"|"NPD1"|"NPD2"|"NPM1"|"NPM2"
			  -> helper t (insert d a ["noun"])
	      | "PN"|"PN1"|"PNQO"|"PNQS"|"PNQV"|"PNX1"|"PPGE"|"PPH1"|"PPHO1"|
		    "PPHO2"|"PPHS1"|"PPHS2"|"PPIO1"|"PPIO2"|"PPIS1"|"PPIS2"|
			"PPX"|"PPX2"|"PPY" -> helper t (insert d a ["pronoun"])
	      | "RA"|"REX"|"RG"|"RGQ"|"RGQV"|"RGR"|"RGT"|"RL"|"RP"|"RPK"|"RR"|
		    "RRQ"|"RRQV"|"RRR"|"RRT"|"RT "-> helper t 
		  (insert d a ["adverb"])
	      | "TO" -> helper t (insert d a ["infinitive marker"])
	      | "UH" -> helper t (insert d a ["interjection"])
	      | "VB0"|"VBDR"|"VBDZ"|"VBG"|"VBI"|"VBM"|"VBN"|"VBR"|"VBZ"
		    -> helper t (insert d a ["be"])
	      | "VD0"|"VDD"|"VDG"|"VDI"|"VDN"|"VDZ"|"VH0"|"VHD"|"VHG"|"VHI"|
		    "VHN"|"VHZ"|"VV0"|"VVD"|"VVG"|"VVGK"|"VVI"|"VVN"|"VVNK"|
			"VVZ"|"VM"|"VMK" -> helper t (insert d a ["verb"])
	      | "XX" -> helper t (insert d a ["not"])
	      | "ZZ1"|"ZZ2" -> helper t (insert d a ["letter"])
	      | anything_else -> helper t d)
	| [] -> d
	| _::[] -> raise (Failure "error in part of speech file")
    in helper l empty
end

let randomsentence() =
  let v () = randomelement([["adv"; "verb"]; ["verb"]]) in
  let n () = randomelement([["adj"; "noun"]; ["noun"]]) in
  let np () = randomelement([["sub"];["det"] @ n()]) in
  let prepphrase () = randomelement([["prep"] @ n()]) in
  let vp () = randomelement([v() @ np(); ["be"]@["adj"]; v()]) in
      flatten(randomelement([[np() @ vp()]; [np() @ vp() @ prepphrase()]]));;

let poslist = randomsentence();;


let babble (tokens:(string*string)array) (markov) (dict) =
  let poslist = randomsentence() in
  
  let rec find_token tokens dict poslist =
    let token = Array.get tokens(Random.int(Array.length tokens)) in
    let (a,b) = token in
      if ((PoS.lookup dict a) = (List.nth poslist 0) && 
	  (PoS.lookup dict b) = (List.nth poslist 1)) then token
      else find_token tokens dict poslist in
          
  let helper key markov dict poslist sent =
    let (a,b) = (find_token tokens dict poslist) in
    let rec helper2 key markov dict poslist sent int =
      let (a,b) = key in
      let values = match (MCB.lookup dict key) with
	| None -> [""]
	| Some l -> l in
      let rec iterate list = 
	match list with
	  | hd::tl -> if (PoS.lookup dict hd) = (List.nth poslist int) then 
	      hd else iterate tl
	  | [] -> "." in
      let next = iterate values in
      if (next = ".") then (sent ^ ".")
      else (helper2 (b,next) markov dict poslist (sent^" "^next) (int+1)) in
    a ^ " " ^ b ^ (helper2 key markov dict poslist sent 0) in
    
    let token = (find_token tokens dict poslist) in
    helper token markov dict poslist ""
;;
      
    
