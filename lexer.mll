{
open Parser;;        (* le type "token" est défini dans parser.mli *)
exception Eof;;
}

let alpha  = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let float  = ('+'|'-')? digit+ ('.' digit*)?
let scForm = float (('e'|'E')('+'|'-')? digit*)?

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)

  | alpha+ ('_' digit+)* as v		{VAR(v)}
  | scForm as f		   		{VAL(float_of_string f)}
  | '+'	      				{PLUS}
  | '-'            			{MINUS}
  | "MINIMIZE"				{MIN}
  | "MAXIMIZE"				{MAX}
  | "SUBJECT TO"			{ST}
  | "BOUNDS" 				{BDS}
  | "VARIABLES"				{VARS}
  | "<="     				{LEQ}
  | ">="				{GEQ}
  | '='					{EQ}
  | "//" ([^ '\n']*)			{COM}
  | '\n'                       		{EOL}
  | eof                        		{EOF}