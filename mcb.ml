module type MCB = 
sig
  type token
  type key   
  type value 
  type dict
  val empty : dict 
  val insert : dict -> key -> value -> dict
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool
  val read : 'a -> token list
  val normalize : dict -> dict
  val make_dict: token list -> dict
  val babble: key -> dict -> string
end

module MCB_imp : MCB
struct
  type token = string;;
  type key = (token*token);;
  type value = (token*float) list;;
  type dict = (key*value) list;;
  let empty = [];;
  let rec insert (d:dict) (k:key) (v:value) : dict = 
    match d with
      | [] -> [(k,v)]
      | _ -> 
end

module 
struct
  module M = Map.make(
    struct
      type t =
    end)
  type token = string
  type key = (string*string)
  type value = (token*token)
  type dict = 
  let empty = M.empty
  let insert (d:dict) (k:key) (v:value) : dict = M.add k v d
  let lookup (d:dict) (k:key) : value option =
    try
      Some (M.find k d)
    with Not_found ->
      None
  let member (d:dict) (k:key) : bool = 
    M.mem k d
  let read (file: 'a) : token list =


  let normalize (d:dict) : dict =


  let make_dict (t:token list) : dict =


  let babble (k:key) (d:dict) : string =


end