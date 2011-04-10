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