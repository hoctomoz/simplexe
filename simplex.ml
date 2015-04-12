open Linear;;

type solution = Opt | Empty | Unbound;;

class simplex nvarP nconsP constraintsP objectiveP =
object (this)

  (* Variables *)
  val nvar : int = nvarP
  val ncons : int = nconsP
  val mutable objective : float array = Array.make (nvarP + nconsP + 2) 0.
  val mutable constraints : float array array = Array.make_matrix nconsP (nvarP + nconsP + 2) 0.
  (* On garde la constante en première position (matrix[i][0]) *)
  (* On rajoute une dernière colonne à la fin pour l'éventuelle première phase, sinon il faudrait redéfinir tout l'objet... *)
  val variables : int array = Array.init nconsP (fun x -> x + nvarP + 1)
  (* On repère quelle est la variable de chaque ligne que l'on assigne *)

  (* On conserve l'invariant suivant : forall i, constraints.(i) est normalisé suivant variables.(i) *)

  (* Méthodes *)
  (* Setters et getter à supprimer : juste pour tester, en attendant le parser
     retirer le cara mutable aussi... *)
  method setObjective o = objective <- o

  method setConstraints m = constraints <- m

  method printConstraint i =
    if constraints.(i).(variables.(i)) <> -1. then failwith "Erreur dans print_constraint : équation non normalisée";
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

  method firstPhase () =
    (* TODO *)
    ()

  method secondPhase () =
    match enteringVariable objective with
    | None -> Opt
    | Some k -> match leavingVariable k constraints with
      | None -> Unbound
      | Some i ->
	normalize k constraints.(i);
	variables.(i) <- k;
	substitute k objective constraints.(i);
	substituteMatrix i k constraints;

	this#print ();
	this#secondPhase ();
	
  method solve () =
    this#firstPhase();
    if this#evaluate() < 0. then Empty
    else this#secondPhase();

end;;
