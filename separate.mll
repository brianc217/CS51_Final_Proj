
{
open Parse
let word = ""
let list = []
} 

rule separate = parse
| ' '    { word::list; word = ""; separate lexbuf }
| eof    { () }
| c      { word^c; separate lexbuf }

{
  let main () = 
    let lexbuf = Lexing.from_channel channel in
    separate lexbuf;
}
