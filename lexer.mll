{
open Parser;;        (* le type "token" est défini dans parser.mli *)
exception Eof;;
}

let alpha  = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let float  = ('+'|'-')? digit+ ('.' digit*)?
let scForm = float (('e'|'E')('+'|'-')? digit*)?
let comment = "//" ([^ '\n']*)

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)

  | '+'	      				{ print_string "plus   \n"; PLUS }
  | '-'            			{ print_string "minus  \n"; MINUS }
  | "MINIMIZE"				{ print_string "min    \n"; MIN }
  | "MAXIMIZE"				{ print_string "max    \n"; MAX }
  | "SUBJECT TO"			{ print_string "st     \n"; ST }
  | "BOUNDS" 				{ print_string "bds    \n"; BDS }
  | "VARIABLES"				{ print_string "vars   \n"; VARS }
  | "<="     				{ print_string "leq    \n"; LEQ }
  | ">="				{ print_string "geq    \n"; GEQ }
  | '='					{ print_string "eq     \n"; EQ }
  | alpha+ ('_' digit+)* as v		{ print_string v; print_newline(); VAR(v) }
  | scForm as f		   		{ print_string f; print_newline(); VAL(float_of_string f) }
  | '\n'(comment|'\n')*                 { print_string "eol    \n"; EOL }
  | eof                        		{ print_string "eof    \n"; EOF }
