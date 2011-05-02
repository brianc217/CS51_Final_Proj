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
  (*val babble: key -> dict -> string*)
  
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
  type word = string
  type key = (string * string);; 
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

  let key_list (lst:string list) : (string*string) list = 
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
    in helper lst []
;;
  let key_array (lst: (string*string) list) : (string*string) array =
    Array.of_list lst
;;
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

  exception Not_in_dict

  let rec babble k d (*(k:MCB.key) (d:MCB.dict)*) : string = 
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

end

