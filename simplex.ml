open Linear;;
open InstanceBuilder;;

type solution = Opt | Empty | Unbound;;

let latexPrelude = "\\documentclass{article}\n\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\n\\begin{document}\n\n";;
let latexPostlude = "\\end{document}";;

let stringOfConstant constant =
  if constant = 0. then ""
  else (Printf.sprintf " %G" constant)

let standardize max objectiveFunction constraints bounds =
  if not max
  then (minusExpression objectiveFunction, revertConstraintsList constraints, revertConstraintsList bounds)
  else (objectiveFunction, constraints, bounds)

class simplex maxP objectiveFunctionP constraintsP boundsP variableSetP (*nvarP nconsP constraintsP objectiveP variableNameP*) =

      let (objectiveFunction, constraints, bounds) = standardize maxP objectiveFunctionP constraintsP boundsP in
      let (boundedSet, varConstraints) = splitBounds bounds in
      let unboundedSet = VariableSet.diff variableSetP boundedSet in
      let globalVariableSet = addUnboundedVariables variableSetP unboundedSet in
      let (variableName, variableTable) = nameVariables globalVariableSet in
      let globalConstraints = handleUnboundedVariables unboundedSet (varConstraints @ constraints) in
      let nconsP = List.length globalConstraints in
      let nvarP = Hashtbl.length variableTable in

object (this)

  (* Variables *)
  val nvar : int = nvarP
  val ncons : int = nconsP
  val mutable objective : float array = objectiveFunctionFromList objectiveFunction variableTable nvarP nconsP
  val constraints : float array array = constraintsFromList globalConstraints variableTable nvarP nconsP
  (* On garde la constante en première position (matrix.(i).(0)) *)
  (* On rajoute une dernière colonne à la fin pour l'éventuelle première phase, sinon il faudrait redéfinir tout l'objet... *)
  val variables : int array = Array.init nconsP (fun x -> x + nvarP + 1)
  (* On repère quelle est la variable de chaque ligne que l'on assigne *)
  val variableName : string array = variableName

  (* On conserve l'invariant suivant : forall i, constraints.(i) est normalisé suivant variables.(i) *)

  method getName varIndex =
    if varIndex = nvar + ncons + 1
    then "x_0"
    else if varIndex > nvar
    then Printf.sprintf "x_%d" varIndex
    else variableName.(varIndex-1)

  method toString coeff varIndex =
    let variable = this#getName varIndex in
      if coeff = 0. then ""
      else if coeff = 1. then Printf.sprintf " + %s" variable
      else if coeff = -.1. then Printf.sprintf " - %s" variable
      else if coeff > 0. then Printf.sprintf " + %G*%s" coeff variable
      else if coeff < 0. then Printf.sprintf " - %G*%s" (-. coeff) variable
      else failwith "Erreur dans toString : l'argument n'est pas un vrai float"

  method latexName varIndex =
    let variable = this#getName varIndex in
    let totalIndices = ref 0 in
    let partialString = Str.global_substitute (Str.regexp "_") (fun _ -> incr totalIndices; "_{") variable in
    partialString ^ (String.make (!totalIndices) '}')

  method toLatex coeff varIndex =
    let variable = this#latexName varIndex in
      if coeff = 0. then ""
      else if coeff = 1. then Printf.sprintf " + %s" variable
      else if coeff = -.1. then Printf.sprintf " - %s" variable
      else if coeff > 0. then Printf.sprintf " + %G \\times %s" coeff variable
      else if coeff < 0. then Printf.sprintf " - %G \\times %s" (-. coeff) variable
      else failwith "Erreur dans toLatex : l'argument n'est pas un vrai float"

  method printConstraint i =
    if constraints.(i).(variables.(i)) <> -1.
    then
       failwith "Erreur dans print_constraint : équation non normalisée.";
    Printf.printf "%s =%s" (this#getName variables.(i)) (stringOfConstant constraints.(i).(0));
    
    for k = 1 to nvar + ncons + 1 do
      if k <> variables.(i) then begin
        print_string (this#toString (constraints.(i).(k)) k)
      end;
    done;

    print_newline()

  method print () =
    for i = 0 to ncons-1 do
      this#printConstraint i;
    done;
    print_string "-----------------";
    print_newline();
    Printf.printf "z =%s" (stringOfConstant objective.(0));
    for i = 1 to nvar + ncons + 1 do
      print_string (this#toString (objective.(i)) i)
    done;
    print_string "\n\n";

  method latexConstraint i =
    if constraints.(i).(variables.(i)) <> -1. then failwith "Erreur dans print_constraint : équation non normalisée";
    let s = ref (Printf.sprintf "%s & = %G" (this#latexName (variables.(i))) (constraints.(i).(0))) in
    
    for k = 1 to nvar + ncons + 1 do
      if k <> variables.(i)
      then s := !s ^ (this#toLatex (constraints.(i).(k)) k)
    done;
    !s ^ "\\\\\n"

  method latex () =
    let s = ref "\\begin{equation*}\n\\begin{cases}\n" in
    for i = 0 to ncons-1 do
      s := !s ^ this#latexConstraint i;
    done;
    s := !s^(Printf.sprintf "z & = %s" (stringOfConstant objective.(0)));

    for i = 1 to nvar + ncons + 1 do
      s := !s ^ (this#toLatex (objective.(i)) i)
    done;
    !s ^ "\n\\end{cases}\n\\end{equation*}\n\n";

  method printWhat b =
    if b then print_string (this#latex())
    else this#print();

  method currentPoint () =
    let point = Array.make (nvar+ncons) 0. in

    for i = 0 to ncons-1 do
      point.(variables.(i)-1) <- constraints.(i).(0)
    done;
    
    point

  method printCurrentPoint () = print_array (this#currentPoint()); print_newline();

  method evaluate () = objective.(0)

  method certificate () =
    let cert = Array.make (nvar+ncons) 0. in

    for i = 0 to ncons-1 do
      cert.(i) <- objective.(i+1)
    done;

    cert

  method printCertificate () = print_array (this#certificate()); print_newline();

  method firstPhaseNeeded () = negativeConstant constraints

  method firstPhase print =
    let z = Array.copy objective in
    let w = Array.make (nvar+ncons+2) 0. in

    let index0 = nvar + ncons + 1 in (* Indice de x0 *)

    w.(index0) <- -1.; (* Maximiser -x0 *)
    for i = 0 to ncons-1 do
      constraints.(i).(index0) <- 1.;
    done;
    objective <- w; (* On a fini de mettre en place la version complémentaire *)

    this#printWhat print;

    this#switch index0 (leavingx0 constraints); (* Première étape (pivot illégal) *)

    this#printWhat print;

    if this#secondPhase print <> Opt then failwith "Cas impossible dans firstPhase : Unbound ou Empty après rajout de x0..." (* le résultat est toujours Opt *);

    if this#evaluate () = 0. then begin
      objective <- z;
      for i = 0 to ncons-1 do
	constraints.(i).(index0) <- 0.;
      done;
      true
    end
    else false
	  


  method switch k i =
    (* Échange la variable entrante k avec la ième contrainte, conserve l'invariant de normalisation *)
    normalize k constraints.(i);
    variables.(i) <- k;
    substitute k objective constraints.(i);
    substituteMatrix i k constraints;

  method secondPhase print =
    match enteringVariable objective with
    | None -> Opt
    | Some k -> match leavingVariable k constraints with
      | None -> Unbound
      | Some i ->
	this#switch k i;
	this#printWhat print;
	this#secondPhase print;

  method solve print =
    if print then print_string latexPrelude;
    this#printWhat print;

    let sol = ref Opt in

    if this#firstPhaseNeeded () then
      if this#firstPhase print then sol := this#secondPhase print
      else sol := Empty
    else sol := this#secondPhase print;

    if print then print_string latexPostlude;

    !sol

end;;
