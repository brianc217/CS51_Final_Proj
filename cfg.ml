let Adj = (* list of adjectives *)
let Adv = (* list of adverbs *)
let verb = (* list of verbs *)
let noun = (* list of nouns *)
let Det = (* list of dets *) 
let Con = "and" | "or"
let V = Cons(Adv; verb) | verb
let N = Cons(Adj; noun) | noun
let VP = Cons(V; NP) | Cons(Adv; Cons(V; NP)) | Cons(be; Adj) | V
let NP = PN | Cons(Det; N) | Cons(NP; Cons(Con; NP))
let sentence = Cons(NP; VP)
(* functions:
   - flatten
   - build_sentence *)