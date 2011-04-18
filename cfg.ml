
(* list of adjectives *)
(* list of adverbs *)
(* list of verbs *)
(* list of nouns *)
(* list of dets *) 
(* list of proper nouns *)
(* list of be verbs *)
(* all of the above lists will be read in from a text file *)

(* concat takes a list of strings and turns it into one string *)
let rec concat (l:string list) : string =
  match l with
    | h::t -> h ^ " " ^ (concat t)
    | [] -> ""

(* this first random element function works with string list *)
let randomelement1 l =
  let rec helper l n =
    match l with
      | h::t -> if n = 0 then h else helper t (n-1)
      | [] -> raise (Failure "random number error")
  in
  let n = Random.int (List.length l) in
    helper l n;;

(* this second random element function works with the lists of string lists*)
let randomelement2 l =
  let rec helper l n =
    match l with
      | h::t -> if n = 0 then h else helper t (n-1)
      | [] -> raise (Failure "random number error")
  in
  let n = Random.int (List.length l) in
    helper l n;;

(* 
   in the end, adj () and all the other functions will be random elements of 
   the list of words that will be defined at the top of the file. also, more
   possible definitions of sentences will be added.
*)
let randomsentence () =
    let adj () = randomelement1(["smart"; "brown"; "smelly"; "wonderful"]) in
    let adv () = randomelement1(["slowly"; "angrily"]) in
    let verb () = randomelement1(["runs"; "eats"; "eats"; "bites"; "breaks"]) in
    let noun () = randomelement1(["house"; "computer"; "phone"; "dog"; "stick"])
    in
    let det () = randomelement1(["a"; "the"; "that"; "this"; "your"]) in
    let pn () = randomelement1(["the President"; "Mario"; "Travis"; "Meng Xiao"; "Brian"; "Heath"]) in
    let be () = randomelement1(["is"]) in
    let v () = concat(randomelement2([[adv(); verb()]; [verb()]])) in
    let n () = concat(randomelement2([[adj(); noun()]; [noun()]])) in
    let np () = concat(randomelement2([[pn()];[det(); n()]])) in
    let vp () = concat(randomelement2([[v(); np()]; [be(); adj()]; [v()]])) in
    let sentence () = concat(randomelement2([[np();vp()]])) in
      sentence ()
;;
(* Run randomsentence() to get a random sentence! *)
