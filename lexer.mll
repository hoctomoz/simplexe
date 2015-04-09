{
open Parser;;        (* le type "token" est défini dans parser.mli *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | ' '* 'p' ' '+ "cnf" ' '+ (['0'-'9']+ as v) ' '+ (['0'-'9']+ as c) {ENTETE((int_of_string v),(int_of_string c))}
  | '-'                        {MINUS}
  | ['1'-'9']['0'-'9']* as s   {VAR(int_of_string s)}
  | '0'                        {ZERO}
  | 'c' (' ' [^ '\n']*)?       {COM}
  | '\n'                       {EOL}
  | eof                        {EOF} 
