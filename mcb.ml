(* Signature for Markov Chain Babbler *)
module type MCB = 
sig
  type key   
  type value 
  type dict
  val empty : dict 
  val insert : dict -> key -> value -> dict
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool
  val read : string -> string list
  val normalize : dict -> dict
  val make_dict: string list -> dict
  val babble: key -> dict -> string
end

(*
(* List implementation of MCB *)
module MCB_imp : (MCB with type key = (token * token)
with type value = (token * float) list) = 
struct
  type token = string ;;
  type key = (token * token);;
  type value = (token * float) list;;
  type dict = (key * value) list;;
  let empty = [] ;;
  
let lookup (d:dict) (k:key) : value option = 
  try (Some(List.find ((=) k) d)) with Not_found -> None ;;
let member (d:dict) (k:key) : bool = List.mem k d ;;

(* Reads in text files and stores each word in a list of words. Bugs:Returns
   reversed list and doesn't include last word *)
let read (file:string) : token list =
    let channel = open_in file in   
    let rec helper (word:token) (list:token list) : token list =
      let char = try Some(input_char channel) with End_of_file -> None in
      match char with
	| Some c -> 
	    begin
	    match c with 
	      | ' ' -> helper "" (word::list)
	      | c -> helper (word^ String.make 1 c) list 
	    end
        | None -> list in
    helper "" [] 
;;

let normalize (d:dict) : dict = []
;;

  let rec make_dict (t:token list) (d:dict) : dict =
    match t with
      | [] -> d
      | h1::h2::h3::tl ->  
	  let v = 
	    match (lookup d (h1,h2)) with
	    | None -> make_dict (h2::h3::tl) (insert d (h1,h2) [(h3,1)])
	    | Some a -> a
	  in
	  let rec findnewvalue (v:value) = 
	    match v with
	     | [] -> (h3,1)::value
	     | (word,prob)::tl -> if word = h3 then
		(word,prob + 1)::tl
	       else 
		findnewvalue tl
	  in let newvalue = findnewvalue v in
	  make_dict (h2::h3::tl) (insert d (h1,h2) newvalue)
;;

  let babble (k:key) (d:dict) : string = "" ;;


end 
*)

(* Map implementation of MCB *)
module Map_MCB : (MCB with type key = (string * string) 
with type value = (string * float) list) =  
struct

  module M = Map.Make(
    struct
      type t = (string * string)
      let compare x y = let x = match x with
	  | (a,b) -> a in
        let y = match y with
	  | (a,b) -> a in
	String.compare x y
    end) 
  type key = (string * string) 
  type value = (string * float)list 
  type dict = (string*float)list M.t 
  let empty = M.empty 
  let insert (d:dict) (k:key) (v:value) : dict = M.empty
  let lookup (d:dict) (k:key) : value option =
    try
      Some (M.find k d)
    with Not_found ->
      None
  
  let member (d:dict) (k:key) : bool = M.mem k d 
  
  let read (file:string) : string list =
    let channel = open_in file in   
    let rec helper (word:string) (list:string list) : string list =
      let char = try Some(input_char channel) with End_of_file -> None in
      match char with
	| Some c -> 
	    begin
	    match c with 
	      | ' ' -> helper "" (word::list)
	      | c -> helper (word^ String.make 1 c) list 
	    end
        | None -> list in
    helper "" [] 

  let normalize (d:dict) : dict = M.empty
  let make_dict (list:string list) : dict = M.empty
  let babble (k:key) (d:dict) : string = ""
end