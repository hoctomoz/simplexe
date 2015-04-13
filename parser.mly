%{
        (* --- préambule: ici du code Caml --- *)

open Simplex
open Hashtbl
(* Set of variables *)
module VariableSet = Set.Make(String)

let nameVariables variableList =
        let nvar = List.length variableList in
        let variableTable = Hashtbl.create nvar in
        let variables = Array.make nvar "" in
        List.iteri (
                fun i variable ->
                        Hashtbl.add variableTable variable i;
                        variables.(i) <- variable
                        ) variableList;
        (variables, variableTable)

let objectiveFunctionFromList objectiveList variableTable nvar ncons =
        let objective = Array.make (nvar + ncons + 2) 0. in
        List.iter (
                fun (var, coeff) ->
                        let varIndex = Hashtbl.find variableTable var in
                        objective.(varIndex) <- objective.(varIndex) +. coeff;
                        )
        objectiveList;
    objective

let constraintsFromList constraintsList variableTable nvar ncons =
        let constraints = Array.make_matrix ncons (nvar + ncons + 2) 0. in
        List.iteri (
                fun i (lowerBound, expression) ->
                        constraints.(i).(0) <- -. lowerBound;
                  List.iter (
                          fun (var, coeff) ->
                                  let varIndex = find variableTable var in
                                  constraints.(i).(varIndex) <- constraints.(i).(varIndex) +. coeff
                                  ) expression;
                  ) constraintsList;
        constraints

let splitBounds boundsList =
(* Separates bounding conditions of the form var >= 0. from other constraints on variables *)
        let (bounded, varConstraints) = List.partition (function
                | (lowerBound, [(_, coeff)]) -> lowerBound = 0. && coeff = -1.
                | _ -> assert false
        ) boundsList in
        (List.fold_left (fun boundedSet -> function
                | (0., [(var, _)]) -> VariableSet.add var boundedSet
                | _ -> assert false
        ) VariableSet.empty bounded,
        varConstraints)

let minusExpression expression =
        List.map (fun (var, coeff) -> (var, -.coeff)) expression

let revertConstraintsList constraintsList =
        List.map (fun (lowerBound, expression) -> (-.lowerBound, minusExpression expression)) constraintsList

let rec buildInstance max objectiveFunction constraints bounds variableList =
        if not max
        then buildInstance true (minusExpression objectiveFunction) (revertConstraintsList constraints) (revertConstraintsList bounds) variableList
        else
                begin

                        let (variables, variableTable) = nameVariables variableList in
                        let (bounded, varConstraints) = splitBounds bounds in
                        let globalConstraints = varConstraints @ constraints in
                        let ncons = List.length globalConstraints in
                        let nvar = Hashtbl.length variableTable in
                        new Simplex.simplex nvar ncons
                                (constraintsFromList globalConstraints variableTable nvar ncons)
                                (objectiveFunctionFromList objectiveFunction variableTable nvar ncons)
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
%token EOL, EOF            /* retour à la ligne */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Simplex.simplex> main     /* on _doit_ donner le type du point d'entrée */

%%
main:
  |COM EOL main                         { $3 }
  |objective EOL objectiveFunction EOL ST EOL constraints EOL BDS EOL bounds EOL VARS EOL variables EOF { buildInstance $1 $3 $7 $11 $15 }
  ;

  objective:
  |MAX                                  { true }
  |MIN                                  { false }
  ;

  objectiveFunction:
  |expression                           { $1 }
  ;

  expression:
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
  |VAR EOL variables                    { $1 :: $3 }
  |VAR EOL                              { [$1] }
  ;
