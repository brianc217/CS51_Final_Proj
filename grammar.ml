module Grammar = 
struct

(* Implementing a Set *)
module Words = Set.Make(String)

(* Function that reads each line of a file into a list *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
    try
      while true; do
	lines := input_line chan :: !lines
      done; []
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;

(* Function that takes an 'a list and turns it into a Words set*)
let rec listToSet l s =
    match l with
      | h::t -> listToSet t (Words.add h s)
      | [] -> s

(* Load all the different parts of speech *)
let nouns = listToSet (read_file "noun.txt") Words.empty
let adjs = listToSet (read_file "adj.txt") Words.empty
let advs = listToSet (read_file "adv.txt") Words.empty
let verbs = listToSet (read_file "verb.txt") Words.empty
let dets = listToSet (read_file "det.txt") Words.empty
let subs = listToSet (read_file "subject.txt") Words.empty
let be = listToSet (read_file "be.txt") Words.empty
let preps = listToSet (read_file "prep.txt") Words.empty

(* A helper function that will flatten a list *)
let rec flatten l : 'a list =
  match l with
    | h::t -> h @ (flatten t)
    | [] -> []

(* A helper function that picks a random element from a list *)
let randomelement l =
  List.nth l (Random.int (List.length l))

(* Returns a random sentence as a list of parts of speech. *)
let randomsentence () =
  let v () = randomelement([[advs; verbs]; [verbs]]) in
  let n () = randomelement([[adjs; nouns]; [nouns]]) in
  let np () = randomelement([[subs];[dets] @ n()]) in
  let prepphrase () = randomelement([[preps] @ n()]) in
  let vp () = randomelement([v() @ np(); [be]@[adjs]; v()]) in
      flatten(randomelement([[np() @ vp()]; [np() @ vp() @ prepphrase()]]))

(*
  randomsentence () returns a list of parts of speech (a Words.t list).
  You can use Words.mem to check to see if a string is in a certain part
  of speech: 
  val mem : elt -> t -> bool
  val mem : string -> Words.t -> bool
*)
end
