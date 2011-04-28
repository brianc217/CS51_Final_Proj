open Grammar;;
open MCB;;

let pslist = randomsentence();;

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
    let rec helper (l:string list) (d:dict) =
      match l with
	| a::b::t ->( match b with
			| "APPGE" -> helper t (insert d a [b])
			| "AT" | "AT1" -> helper t (insert d a [b])
			| "BCL" -> helper t (insert d a [b])
			| "CC" | "CCB" | "" -> helper t (insert d a [b]))
	| [] -> d
	| _::[] -> raise (Failure "error in part of speech file")
    in helper l empty
end