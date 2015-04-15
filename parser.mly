%{
        (* --- préambule: ici du code Caml --- *)

open Simplex
open Hashtbl
open Printf
(* Set of variables *)
module VariableSet = Set.Make(String)

(******** Printing functions to check parsing ********)

let printParsedExpression =
        List.iter (fun (var, coeff) -> Printf.printf " %+f %s" (coeff) (var))

let printParsedObjectiveFunction = printParsedExpression

let printParsedConstraint = function
        | (lowerBound, expression) -> printParsedExpression expression; Printf.printf " <= %f\n" (lowerBound)

let printParsedConstraints = List.iter printParsedConstraint


(******** Printing functions to check normalization ********)

let printVariables = 
        Hashtbl.iter (fun var index -> Printf.printf "%s <-> %d\n" (var) (index))

let printParserConstraints constraints =
        Array.iteri (
                fun i const ->
                        Printf.printf "Constraint %d : " (i+1);
                        Array.iteri (
                                fun j coeff -> if j = 0 then Printf.printf "%+f " (coeff) else Printf.printf "%+f x%d " (coeff) (j)
                        ) const;
                        Printf.printf " = 0\n";
        ) constraints


(******** Conversion from parsed entry to simplex object ********)

let addUnboundedVariables variableSet unboundedSet =
        VariableSet.fold (fun unboundedVar -> VariableSet.add (unboundedVar ^ "-")) unboundedSet variableSet


let rec replaceUnboundedVariables unboundedSet = function
        | [] -> []
        | (var, coeff)::t ->
                        let partialConstraint = replaceUnboundedVariables unboundedSet t in
                        if VariableSet.mem var unboundedSet
                        then (var, coeff)::(var^"-", -.coeff)::partialConstraint
                        else (var, coeff)::partialConstraint

let handleUnboundedVariables unboundedSet =
        List.map (fun (lowerBound, expression) -> (lowerBound, replaceUnboundedVariables unboundedSet expression))

let nameVariables variableSet =
        let nvar = VariableSet.cardinal variableSet in
        let variableTable = Hashtbl.create nvar in
        let variables = Array.make nvar "" in
        let current_index = ref 1 in
        VariableSet.iter (
                fun var ->
                        Hashtbl.add variableTable var (!current_index);
                        variables.(!current_index - 1) <- var;
                        incr current_index;
                        ) variableSet;
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
                        constraints.(i).(0) <- lowerBound;
                        List.iter (
                                  fun (var, coeff) ->
                                        let varIndex = find variableTable var in
                                        constraints.(i).(varIndex) <- constraints.(i).(varIndex) -. coeff
                                  ) expression;
                        constraints.(i).(nvar + i + 1) <- -1.;
                  ) constraintsList;
        (* TODO: remove. *)
        (* printParserConstraints constraints; *)
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

let rec buildInstance max objectiveFunction constraints bounds variableSet =
        if not max
        then buildInstance true (minusExpression objectiveFunction) (revertConstraintsList constraints) (revertConstraintsList bounds) variableSet
        else
                begin
                        (* TODO: remove. Left it for debugging reasons.
                         * Printf.printf "Objective: \n";
                         * printParsedObjectiveFunction objectiveFunction;
                         * Printf.printf "\nConstraints: \n";
                         * printParsedConstraints constraints;
                         * Printf.printf "\nBounds: \n";
                         * printParsedConstraints bounds; *)
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
