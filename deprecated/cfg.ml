module CFG =
struct
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
  
  (* Load all the words *)
  let nouns = Array.of_list (read_file "noun.txt")
  let adjs = Array.of_list (read_file "adj.txt")
  let advs = Array.of_list (read_file "adv.txt")
  let verbs = Array.of_list (read_file "verb.txt")
  let dets = Array.of_list (read_file "det.txt") 
  let subs = Array.of_list (read_file "subject.txt")
  let be = Array.of_list (read_file "be.txt")
    
  (* concat takes a list of strings and turns it into one string *)
  let rec concat (l:string list) : string =
    match l with
      | h::[] -> h
      | h::t -> h ^ " " ^ (concat t)
      | [] -> ""
	  
	  
  (* Returns a random element on a list *)
  let randomelement l =
    List.nth l (Random.int (List.length l))
      
  (* Returns a string of a random sentence! *)
  let randomsentence () =
    let adj () = Array.get adjs (Random.int (Array.length adjs)) in
    let adv () = Array.get advs (Random.int (Array.length advs)) in
    let verb () = Array.get verbs (Random.int (Array.length verbs)) in
    let noun () = Array.get nouns (Random.int (Array.length nouns)) in
    let det () = Array.get dets (Random.int (Array.length dets)) in
    let sub () = Array.get subs (Random.int (Array.length subs)) in
    let be () = Array.get be (Random.int (Array.length be)) in
    let v () = concat(randomelement([[adv(); verb()]; [verb()]])) in
    let n () = concat(randomelement([[adj(); noun()]; [noun()]])) in
    let np () = concat(randomelement([[sub()];[det(); n()]])) in
    let vp () = concat(randomelement([[v(); np()]; [be(); adj()]; [v()]])) in
    let punctuation () = randomelement["!"; "."; "?"; ","] in
      concat(randomelement([[np(); vp(); punctuation ()]]))
  ;;
end
  (* Run randomsentence() to get a random sentence! *)
  
