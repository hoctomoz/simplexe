%{
        (* --- préambule: ici du code Caml --- *)

open Simplex
open Hashtbl
open Printf
open InstanceBuilder
(* Set of variables *)
module VariableSet = Set.Make(String)


let rec buildInstance max objectiveFunction constraints bounds variableSet =
        if not max
        then buildInstance true (minusExpression objectiveFunction) (revertConstraintsList constraints) (revertConstraintsList bounds) variableSet
        else
                begin
                        let (boundedSet, varConstraints) = splitBounds bounds in
                        let unboundedSet = VariableSet.diff variableSet boundedSet in
                        let globalVariableSet = addUnboundedVariables variableSet unboundedSet in
                        let (variables, variableTable) = nameVariables globalVariableSet in
                        let globalConstraints = handleUnboundedVariables unboundedSet (varConstraints @ constraints) in
                        let ncons = List.length globalConstraints in
                        let nvar = Hashtbl.length variableTable in
                        new Simplex.simplex nvar ncons
                                (constraintsFromList globalConstraints variableTable nvar ncons)
                                (objectiveFunctionFromList objectiveFunction variableTable nvar ncons)
                                variables
                end

        %}
/* description des lexèmes */

%token <string> VAR
%token <float> VAL
%token PLUS, MINUS
%token LEQ, GEQ, EQ
%token MIN, MAX
%token ST, BDS, VARS
%token COM
%token END
%token EOL, EOF            /* retour à la ligne */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Simplex.simplex> main     /* on _doit_ donner le type du point d'entrée */

%%
main:
  |COM EOL main                         { $3 }
  |objective EOL objectiveFunction EOL ST EOL constraints BDS EOL bounds VARS EOL variables END EOL EOF { buildInstance $1 $3 $7 $10 $13 }
  ;

  objective:
  |MAX                                  { true }
  |MIN                                  { false }
  ;

  objectiveFunction:
  |expression                           { $1 }
  ;

  expression:
  |PLUS atomicExpression                { [$2] }
  |MINUS atomicExpression               { let (var, coeff) = $2 in [(var, -.coeff)] }
  |expression PLUS atomicExpression     {$3 :: $1 }
  |expression MINUS atomicExpression    { let (var, coeff) = $3 in (var, -.coeff) :: $1 }
  |atomicExpression                     { [$1] }
;
  
  atomicExpression:
  |VAL VAR                              { ($2, $1) }
  |VAR                                  { ($1, 1.) }
;

  constraints:
  |singleConstraint EOL constraints     { $1 @ $3 }
  |singleConstraint EOL                 { $1 }
;

  singleConstraint: /* constraint is a reserved word */
  |expression LEQ VAL                   { [($3, $1)] }
  |expression GEQ VAL                   { [(-.$3, minusExpression $1)] }
  |expression EQ  VAL                   { [($3, $1); (-.$3, minusExpression $1)] }
;

  bounds:
  |bound EOL bounds                     { $1 @ $3 }
  |bound EOL                            { $1 }
;
  bound:
  |VAR GEQ VAL                          { [(-.$3, [($1, -.1.)])] }
  |VAR LEQ VAL                          { [($3, [($1, 1.)])] }
  |VAL LEQ VAR LEQ VAL                  { [($1, [($3, 1.)]); (-.$5, [($3, -.1.)])] }
  |VAL GEQ VAR GEQ VAL                  { [(-.$1, [($3, -.1.)]); ($5, [($3, 1.)])] }
  ;

  variables:
  |VAR EOL variables                    { VariableSet.add $1 $3 }
  |VAR EOL                              { VariableSet.singleton $1 }
  ;
