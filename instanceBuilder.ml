(******** Conversion from parsed entry to simplex object ********)

open Hashtbl
module VariableSet = Set.Make(String)

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
                                        let varIndex = Hashtbl.find variableTable var in
                                        constraints.(i).(varIndex) <- constraints.(i).(varIndex) -. coeff
                                  ) expression;
                        constraints.(i).(nvar + i + 1) <- -1.;
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
