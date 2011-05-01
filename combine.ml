(* Combines MCB and CFG to babble more accurately *)
(*open Mcb;;*)
(* Signature for Markov Chain Babbler *)
module type DICT = 
sig
  type word
  type key   
  type value 
  type dict
  val empty : dict 
  val insert : dict -> key -> value -> dict
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool
  val read : string -> string list
  val make_dict: string list -> dict
  (*val babble: 'a array -> dict -> dict -> string*)
  
end

(* Map implementation of MCB *)
module MCB : (DICT with type key = (string * string) 
with type value = (string list)) =  
struct

  module M = Map.Make(
    struct
      type t = (string * string)
      let compare x y = let x = match x with
	  | (a,b) -> a^b in
        let y = match y with
	  | (a,b) -> a^b in
	String.compare x y
    end)
  type word = string;;
  type key = (string*string);; 
  type value = (string list) ;;
  type dict = value M.t ;;
  let empty = M.empty ;;

  let lookup (d:dict) (k:key) : value option =
    try
      Some (M.find k d)
    with Not_found ->
      None;;

  let insert (d:dict) (k:key) (v:value) : dict = 
    match (lookup d k) with
      | Some l -> M.add k (l@v) d  
      | None -> M.add k v d;;
  
  let member (d) (k:key) : bool = M.mem k d ;;
  (* bugs: double space, punctuation, newline *)
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

  let key_array (lst:string list) : (string*string) array = 
    let rec helper (lst:string list) (tklst: (string*string) list) :
	(string*string) list = 
      match lst with
	| h1::h2::h3::tl -> if List.mem (h1,h2) tklst then
	    helper (h2::h3::tl) tklst
	  else
	    helper (h2::tl) ((h1,h2)::tklst)
	| h1::h2::tl -> if List.mem (h1,h2) tklst then tklst
	  else (h1,h2)::tklst
	| [] -> []
    in Array.of_list (helper lst [])
;;
  (*let key_array (lst: (string*string) list) : (string*string) array =
    Array.of_list lst*)

  let make_dict (list:string list) : dict = 
    let dict = empty in
    let rec helper list dict =
      match list with
        | hd1::hd2::hd3::tl -> helper (hd2::hd3::tl) 
	    (insert dict (hd1,hd2) [hd3])
        | hd1::hd2::tl -> insert dict (hd1,hd2) tl
	| [] -> dict in
      helper list dict
;;

exception Not_in_dict;;
end
  let rec babble k d dict (*(k:MCB.key) (d:MCB.dict)*) : string = 
    let (a,b) = k in
    let rec helper k d s (*(k:MCB.key) (d:MCB.dict) (s:string)*) : string =
      let (a,b) = k in
      let randomelement l = 
        List.nth l (Random.int(List.length l)) in
      let values = match (lookup d k) with
        |Some l -> l
        |None -> raise Not_in_dict in 
      let next = randomelement values in 
        if (next = ".") then (s ^ ".") 
        else (helper (b,next) d (s ^ " " ^ next)) in
    a ^ " " ^ b ^ (helper k d "")
      
;;


module PoS : DICT = 
struct
  module M = Map.Make(
    struct
      type t = string (*MCB.word*)
      let compare x y = String.compare x y
    end)
  type word = string (*MCB.word*)
  type key = string (*MCB.word*)
  type value = string list
  type dict = value M.t
  let empty = M.empty
  let lookup d k = try Some (M.find k d) with Not_found -> None
  let insert (d:dict) (k:key) (v:value)  = 
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


(* A helper function that will flatten a list *)
let rec flatten l : 'a list =
  match l with
    | h::t -> h @ (flatten t)
    | [] -> []

(* A helper function that picks a random element from a list *)
let randomelement (l:'a list) : 'a =
  List.nth l (Random.int (List.length l))

let randomsentence() =
  let v () = randomelement([["adv"; "verb"]; ["verb"]]) in
  let n () = randomelement([["adj"; "noun"]; ["noun"]]) in
  let np () = randomelement([["sub"];["det"] @ n()]) in
  let prepphrase () = randomelement([["prep"] @ n()]) in
  let vp () = randomelement([v() @ np(); ["be"]@["adj"]; v()]) in
      flatten(randomelement([[np() @ vp()]; [np() @ vp() @ prepphrase()]]));;

exception Not_in_dict;;
end;;

let babble (tokens:(M.key*M.key)array) (markov:MCB.dict) (dict:dict) =
  let poslist = randomsentence() in

  let deopt a = 
    match a with
      | None -> []
      | Some l -> l in
  
  let rec find_token tokens dict poslist =
    let token = Array.get tokens (Random.int (Array.length tokens)) in
    let (a,b) = token in
      if (List.mem (List.nth poslist 0) (deopt (lookup dict a))) && 
	 (List.mem (List.nth poslist 1) (deopt (lookup dict b))) 
      then token
      else find_token tokens dict poslist in
          
  let helper key markov dict poslist sent =
    let (a,b) = (find_token tokens dict poslist) in
    
    let rec helper2 key markov dict poslist sent int =
      let (a,b) = key in
      
      let values = match (MCB.lookup markov key) with
	| None -> raise Not_in_dict
	| Some l -> l in
	
      let rec iterate list = 
	match list with
	  | hd::tl -> if (List.mem (List.nth poslist int) 
			    (deopt (lookup dict hd))) then 
	      hd else iterate tl
	  | [] -> List.nth list (Random.int (List.length list)) in

      let next = iterate values in
      if (next = ".") then (sent ^ ".")
      else (helper2 (b,next) markov dict poslist (sent^" "^next) (int+1)) in
    a ^ " " ^ b ^ (helper2 key markov dict poslist sent 0) in
    
    let token = (find_token tokens dict poslist) in
    helper token markov dict poslist ""

;;
      
    
