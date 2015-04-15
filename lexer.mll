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

                                          (* TODO : remove.                *)
  | '+'	      				{ (* print_string "PLUS   \n";     *) PLUS }
  | '-'            			{ (* print_string "MINUS  \n";     *) MINUS }
  | "MINIMIZE"				{ (* print_string "MIN    \n";     *) MIN }
  | "MAXIMIZE"				{ (* print_string "MAX    \n";     *) MAX }
  | "SUBJECT TO"			{ (* print_string "ST     \n";     *) ST }
  | "BOUNDS" 				{ (* print_string "BDS    \n";     *) BDS }
  | "VARIABLES"				{ (* print_string "VARS   \n";     *) VARS }
  | "END"                               { (* print_string "END    \n";     *) END }
  | "<="     				{ (* print_string "LEQ    \n";     *) LEQ }
  | ">="				{ (* print_string "GEQ    \n";     *) GEQ }
  | '='					{ (* print_string "EQ     \n";     *) EQ }
  | alpha+ ('_'? digit+)* as v		{ (* Printf.printf "VAR(%s)\n" (v);*) VAR(v) }
  | scForm as f		   		{ (* Printf.printf "VAL(%s)\n" (f);*) VAL(float_of_string f) }
  | '\n'(comment|'\n')*                 { (* print_string "EOL    \n";     *) EOL }
  | eof                        		{ (* print_string "EOF    \n";     *) EOF }
