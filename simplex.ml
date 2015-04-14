open Linear;;

type solution = Opt | Empty | Unbound;;

let latexPrelude = "\\documentclass{article}\n\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\n\\begin{document}\n\n";;
let latexPostlude = "\\end{document}";;

class simplex nvarP nconsP constraintsP objectiveP =
object (this)

  (* Variables *)
  val nvar : int = nvarP
  val ncons : int = nconsP
  val objective : float array = objectiveP
  val constraints : float array array = constraintsP
  (* On garde la constante en première position (matrix.(i).(0)) *)
  (* On rajoute une dernière colonne à la fin pour l'éventuelle première phase, sinon il faudrait redéfinir tout l'objet... *)
  val variables : int array = Array.init nconsP (fun x -> x + nvarP + 1)
  (* On repère quelle est la variable de chaque ligne que l'on assigne *)

  (* On conserve l'invariant suivant : forall i, constraints.(i) est normalisé suivant variables.(i) *)

  method printConstraint i =
    if constraints.(i).(variables.(i)) <> -1.
    then
      (let errorMsg = Printf.sprintf "Erreur dans print_constraint : équation non normalisée. constraints.(%d).(%d) = %f au lieu de -1." (i) (variables.(i)) (constraints.(i).(variables.(i))) in
       failwith errorMsg);
    print_string "x"; print_int variables.(i); print_string " = ";
    print_float constraints.(i).(0);
    
    for k = 1 to nvar + ncons + 1 do
      if k <> variables.(i) && constraints.(i).(k) <> 0. then begin
	print_string " + ";
	print_float constraints.(i).(k);
	print_string "*x";
	print_int k;
      end;
    done;

    print_newline()

  method print () =
    for i = 0 to ncons-1 do
      this#printConstraint i;
    done;
    for i = 0 to nvar + ncons + 1 do
      print_string "-------";
    done;
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
    ^"x_"^string_of_int(variables.(i))
    ^" & = "
    ^string_of_float(constraints.(i).(0));
    
    for k = 1 to nvar + ncons + 1 do
      if k <> variables.(i) && constraints.(i).(k) <> 0.
      then s := !s
	^" + "
	^string_of_float(constraints.(i).(k))
	^" \\times x_"^string_of_int(k);
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
	^" \\times x_"^string_of_int(i);
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

  method firstPhase print =
    (* TODO *)
    true (* renvoie true si pas empty *)

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
    if this#firstPhase print then this#secondPhase print
    else Empty;
    if print then print_string latexPostlude;

end;;
