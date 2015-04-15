open Linear;;

type solution = Opt | Empty | Unbound;;

let latexPrelude = "\\documentclass{article}\n\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\n\\begin{document}\n\n";;
let latexPostlude = "\\end{document}";;

class simplex nvarP nconsP constraintsP objectiveP variableNameP =
object (this)

  (* Variables *)
  val nvar : int = nvarP
  val ncons : int = nconsP
  val mutable objective : float array = objectiveP
  val constraints : float array array = constraintsP
  (* On garde la constante en première position (matrix.(i).(0)) *)
  (* On rajoute une dernière colonne à la fin pour l'éventuelle première phase, sinon il faudrait redéfinir tout l'objet... *)
  val variables : int array = Array.init nconsP (fun x -> x + nvarP + 1)
  (* On repère quelle est la variable de chaque ligne que l'on assigne *)
  val variableName : string array = variableNameP

  (* On conserve l'invariant suivant : forall i, constraints.(i) est normalisé suivant variables.(i) *)

  method getName varIndex =
    if varIndex > nvar
    then Printf.sprintf "x_%d" varIndex
    else variableName.(varIndex-1)

  method printConstraint i =
    if constraints.(i).(variables.(i)) <> -1.
    then
       failwith (Printf.sprintf "Erreur dans print_constraint : équation non normalisée. constraints.(%d).(%d) = %f au lieu de -1." (i) (variables.(i)) (constraints.(i).(variables.(i))));
    Printf.printf "%s = %f" (this#getName variables.(i)) (constraints.(i).(0));
    (* TODO: remove.
     * print_string "x"; print_int variables.(i); print_string " = ";
    print_float constraints.(i).(0); *)
    
    for k = 1 to nvar + ncons + 1 do
      if k <> variables.(i) && constraints.(i).(k) <> 0. then begin
        Printf.printf " + %f*%s" (constraints.(i).(k)) (this#getName k)
	(* TODO: remove
  * print_string " + ";
	print_float constraints.(i).(k);
	print_string "*x";
	print_int k;*)
      end;
    done;

    print_newline()

  method print () =
    for i = 0 to ncons-1 do
      this#printConstraint i;
    done;
    print_string "-----------------";
    print_newline();
    print_string "z = ";
    print_float (objective.(0));
    for i = 1 to nvar + ncons + 1 do
      if objective.(i) <> 0. then begin
	print_string " + ";
	print_float (objective.(i));
	print_string "*x"; print_int i;
      end;
    done;
    print_string "\n\n";

  method latexConstraint i =
    if constraints.(i).(variables.(i)) <> -1. then failwith "Erreur dans print_constraint : équation non normalisée";
    let s = ref "" in
    s := !s
    ^"x_{"^string_of_int(variables.(i))^"} & = "
    ^string_of_float(constraints.(i).(0));
    
    for k = 1 to nvar + ncons + 1 do
      if k <> variables.(i) && constraints.(i).(k) <> 0.
      then s := !s
	^" + "
	^string_of_float(constraints.(i).(k))
	^" \\times x_{"^string_of_int(k)^"}";
    done;
    !s ^ "\\\\\n"

  method latex () =
    let s = ref "\\begin{equation*}\n\\begin{cases}\n" in
    for i = 0 to ncons-1 do
      s := !s ^ this#latexConstraint i;
    done;
    s := !s^"z & = "^string_of_float(objective.(0));

    for i = 1 to nvar + ncons + 1 do
      if objective.(i) <> 0. then begin
	s := !s
	^" + "
	^string_of_float(objective.(i))
	^" \\times x_{"^string_of_int(i)^"}";
      end;
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
