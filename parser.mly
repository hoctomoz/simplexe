%{
(* --- préambule: ici du code Caml --- *)

open Simplex
open Hashtbl

let objectiveFunctionFromList objectiveList variables nvar ncons =
    let objective = Array.make (nvar + ncons + 2) 0. in
    List.iter (
    	      fun (var, coeff) ->
	      	  let varIndex = Hashtbl.find variables var in
		  objective.(varIndex) <- objective.(varIndex) +. coeff;
	      )
	      objectiveList;
    objective

let constraintsFromList constraintsList variables nvar ncons =
    let constraints = Array.make_matrix ncons (nvar + ncons + 2) 0. in
    List.iteri (
    	      fun i (lowerBound, expression) ->
	      	  constraints.(i).(0) <- -. lowerBound;
	      	  List.iter (
		  	    fun (var, coeff) ->
			    	let varIndex = find variables var in
				constraints.(i).(varIndex) <- constraints.(i).(varIndex) +. coeff
			    ) expression;
	       ) constraintsList;
    constraints

let buildInstance max objectiveFunction constraints bounds variables =

    new Simplex.simplex 0 0 (Array.make 0 0.) (Array.make_matrix 0 0 0.)

let add expr1 expr2 =
    []

let substract expr1 expr2 =
    []

%}
/* description des lexèmes */

%token <string> VAR
%token <float> VAL
%token PLUS, MINUS
%token LEQ, GEQ, EQ
%token MIN, MAX
%token ST, BDS, VARS
%token COM
%token EOL, EOF            /* retour à la ligne */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Simplex.simplex> main     /* on _doit_ donner le type du point d'entrée */

%%
main:
  |COM EOL main                   { $3 }
  |objective objectiveFunction ST constraints BDS bounds VARS variables EOF { buildInstance $1 $2 $4 $6 $8 }
;

objective:
  |MAX				  { true }
  |MIN				  { false }
;

objectiveFunction:
  |expression			  { $1 }
;

expression:
  |expression PLUS expression	  { add $1 $3 }
  |expression MINUS expression	  { substract $1 $3 }
  |VAL VAR    	  		  { [($2, $1)] }
  |VAR	    	  		  { [($1, 1.)] }
;

constraints:
  |constraints EOL constraints    { $1 @ $3 }
  |expression LEQ VAL		  { [($3, $1)] }
  |expression GEQ VAL		  { [(-.$3, substract [] $1)] }
  |expression EQ  VAL		  { [($3, $1); (-.$3, substract [] $1)] }
;

bounds:
  |bounds EOL bounds		  { $1 @ $3 }
  |VAR GEQ VAL			  { [(true, $1, $3)] }
  |VAR LEQ VAL			  { [(false, $1, $3)] }
  |VAL LEQ VAR LEQ VAL		  { [(true, $3, $1); (false, $3, $5)] }
  |VAL GEQ VAR GEQ VAL		  { [(false, $3, $1); (true, $3, $5)] }
;

variables:
  |variables EOL variables	  { $1 @ $3 }
  |VAR	     	 		  { [$1] }
;