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
  |MAX                            { true }
  |MIN                            { false }
  ;

  objectiveFunction:
  |expression                     { $1 }
  ;

  expression:
  |expression PLUS expression     { add $1 $3 }
  |expression MINUS expression    { substract $1 $3 }
  |VAL VAR                        { [($2, $1)] }
  |VAR                            { [($1, 1.)] }
  ;

  constraints:
  |constraints EOL constraints    { $1 @ $3 }
  |expression LEQ VAL             { [($3, $1)] }
  |expression GEQ VAL             { [(-.$3, substract [] $1)] }
  |expression EQ  VAL             { [($3, $1); (-.$3, substract [] $1)] }
  ;

  bounds:
  |bounds EOL bounds              { $1 @ $3 }
  |VAR GEQ VAL                    { [(-.$3, [($1, -.1.)])] }
  |VAR LEQ VAL                    { [($3, [($1, 1.)])] }
  |VAL LEQ VAR LEQ VAL            { [($1, [($3, 1.)]); (-.$5, [($3, -.1.)])] }
  |VAL GEQ VAR GEQ VAL            { [(-.$1, [($3, -.1.)]); ($5, [($3, 1.)])] }
  ;

  variables:
  |variables EOL variables        { $1 @ $3 }
  |VAR                            { [$1] }
  ;
