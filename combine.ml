open Grammar;;
open MCB;;

let poslist = randomsentence();;

let babble (list: string list) (markov: dict) (word: dict): string =
  let rec helper list markov word string : string =
    match list with
      | [] -> string
      | hd::tl -> 

