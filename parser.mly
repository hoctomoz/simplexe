%{
(* --- préambule: ici du code Caml --- *)

open Clause

%}
/* description des lexèmes */

%token <int> VAR
%token <(int * int)> ENTETE
%token MINUS
%token ZERO
%token COM
%token EOL, EOF            /* retour à la ligne */
%token LAST

%nonassoc LAST
%nonassoc EOL, EOF, COM, ZERO, MINUS, ENTETE, VAR


%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Clause.formule_parse * (int * int)> main     /* on _doit_ donner le type du point d'entrée */

%%
main:
  |COM EOL main                   { $3 }
  |ENTETE EOL formule EOF         { let c = $1 in $3,(fst c, snd c) }     
;
formule:
  |clause EOL formule { ($1)::($3) }
  |COM EOL formule    { $3 }
  |clause %prec LAST  { [$1] }
  |COM %prec LAST     { [] } 
;
clause:
  |ZERO               { [] }
  |literal clause     { ($1)::($2) } 
;
literal:
  |VAR             { Var($1) }
  |MINUS VAR       { NonVar($2) } 
;
