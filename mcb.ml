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
  
  let normalize (d:dict) : dict =

  let rec make_dict (t:token list) (d:dict) : dict =
    match t with
      | [] -> d
      | h1::h2::h3::tl -> if (member d (h1,h2)) then 
	  let v = 
	    match (lookup d (h1,h2)) with
	    | None -> _
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
	else
	  make_dict (h2::h3::tl) (insert d (h1,h2) [(h3,1)])

  let babble (k:key) (d:dict) : string =

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

(* Reads in text files and stores each word in a list of words. Doesn't
   work though. There are technical issues *) 
  let read (file: string) : string list =
    let channel = open_in file in
    let list = [] in
    let word = "" in
    let rec helper (lst: string list) =
      try ( 
        let x = Char.code (input_char channel) in
        if (x = 32) then
	  (word::lst)
	else  
	  ((word ^ (String.make 1 (Char.chr x))); (helper lst)) 
	) 
      with
        | End_of_file -> lst in
    helper list
;;  

  let normalize (d:dict) : dict =


  let make_dict (t:token list) : dict =


  let babble (k:key) (d:dict) : string =


end
