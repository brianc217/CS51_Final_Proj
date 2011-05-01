open Markov;;
open Combination;;
open Cfg;;

let rec cfg () =
  let instructions = "\nPress 'r' for a random sentence, and 'm' to go back to the main menu.\n" 
  in
    print_string instructions;
     let input = Pervasives.read_line () in
      match input with 
	| "r" | "R" -> print_string(CFG.randomsentence()); cfg ()
	| "m" | "M" -> ()
	| anything_else -> (print_string "That's not an option. Please try again!\n"); cfg ();;

let rec mcb () = 
  print_string "What would you like to babble over?\n 1. The Constitution\n 2. Alice in Wonderland\n";
  let input = Pervasives.read_line () in
  let dict = 
    match input with 
      | "1" -> MCB.const_dict
      | "2" -> MCB.alice_dict
      | anything_else -> ((print_string "That's not an option.\n"); MCB.MarkovDict.empty;)
  in
  print_string "\nPlease enter the first word: ";
    let a = Pervasives.read_line () in
  print_string "Please enter the second word: ";
      let b = Pervasives.read_line () in
  let key = (a,b) in
  let babble = try (MCB.babble key dict) with MCB.Not_in_dict -> "That's not in our dictionary!" in
  print_string("\n" ^ babble ^ "\n");
  print_string "\nPress 'b' to babble again, and 'm' to go back to the main menu!\n";
  match (read_line ()) with
    | "b" | "B" -> mcb ()
    | "m" | "M" -> ()
    | anything_else -> ((print_string "That's not an option. Please try again!"); mcb ();)


let rec comb () = 
  print_string "What would you like to babble over?\n 1. The Constitution\n 2. Alice in Wonderland\n\n\nPress m to go back to ";
  let info = 
  (match (read_line ()) with
     | "1" -> (MCB.const_dict,PoS.const_tokens)
     | "2" -> (MCB.alice_dict,PoS.alice_tokens)
     | anything_else -> (print_string "That's not an option. Please try again!\n"; 
			 (MCB.const_dict,PoS.const_tokens))) in
  let (x,y) = info in
  print_string (PoS.babble y x PoS.posdict);
  print_string "\nPress 'b' to babble, and 'm' to go back to the main menu!\n";
  match (read_line ()) with
    | "b" | "B" -> comb ()
    | "m" | "M" -> ()
    | anything_else -> (print_string "That's not an option. Please try again!"); comb ();;
  

let rec inst () = 
   let instructions = "What would you like to do?\n 1. CFG\n 2. MCB\n 3. Combination\n\nPress q to quit.\n\n" in
    print_string instructions; 
    let input = read_line () in
      match input with 
	| "1" -> cfg (); inst ();
	| "2" -> mcb (); inst ();
	| "3" -> comb (); inst ();
	| "q" -> ()
	| anything_else -> (print_string "That's not an option. Please try again!\n"); inst ();;

let main () = 
  let loading = "Loading...\n\n" in
    print_string loading;
    (* load everything here... *)
    (*let const = MCB.make_dict (MCB.read "texts/constitution.txt") in
    let alice = MCB.make_dict (MCB.read "texts/alice.txt") in*)
      inst ();;


    let posdict = make_dict (read "newpos.txt");;
    let const_tokens = MCB.key_array MCB.const;;
    let alice_tokens = MCB.key_array MCB.alice;;


main ();;