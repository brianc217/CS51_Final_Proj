module type DICT = 
sig
  type key   
  type value 
  type dict
  val empty : dict 
  val insert : dict -> key -> value -> dict
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed.  Return None if the input dict is empty *)
  val string_of_key: key -> string
  val string_of_value : value -> string
end

(* Arguments to the AssocListDict functor *)
module type DICT_ARG = 
sig
  type key
  type value
  val compare : key -> key -> int
  val string_of_key: key -> string
  val string_of_value : value -> string
end

module MapDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) = 
struct
  (* Note: the Map module in ocaml expects a comparison function
   * that returns -1, 0, and 1 for less than, equal, or greater than.
   *)

module M = Map.Make(
struct
  type t = D.key
  let compare x y = D.compare x y
end)

type key = D.key;;
type value = D.value;;
type dict = D.value M.t;;
let empty = M.empty;;
let lookup (d :dict) (k :key) : value option = 
  try Some(M.find k d) with Not_found -> None;;
let insert (d:dict) k v = M.add k v d ;;

let member d k = M.mem k d;;
let string_of_key = D.string_of_key;;
let string_of_value = D.string_of_value;;
 
end

module Make (D:DICT_ARG) : 
  (DICT with type key = D.key with type value = D.value) = 
  MapDict (D)





