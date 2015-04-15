open Simplex;;
open Parser;;

let lexbuf = Lexing.from_channel (open_in(Sys.argv.(1)));;
let parse () = Parser.main Lexer.token lexbuf;;

(* Main *)

let pb = parse () in
pb#solve false;;
