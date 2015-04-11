open Linear;;

type value = float;;

class simplex nvarP nconsP constraintsP objectiveP =
object (this)

  (* Variables *)
  val nvar : int = nvarP
  val ncons : int = nconsP
  val objective : value array = Array.make (nvarP + nconsP + 2) 0.
  val constraints : value array array = Array.make_matrix nconsP (nvarP + nconsP + 2) 0.
  (* NB : On garde la constante en première position (matrix[i][0]) *)
  (* On rajoute une dernière colonne à la fin pour l'éventuelle première phase, sinon il faudrait redéfinir tout l'objet... *)
  val variables : int array = Array.init nconsP (fun x -> x + nvarP)
  (* On repère quelle est la variable de chaque ligne que l'on assigne *)

  (* Méthodes *)
  method print_constraint i =
    if constraints.(i).(variables.(i)) <> -1. then failwith "Erreur dans print_constraint : équation non normalisée";
    print_string "x"; print_int variables.(i); print_string " = ";
    print_float constraints.(i).(0);
    
    for k = 1 to nvar + ncons do
      if k <> variables.(i) then begin
	print_string " + ";
	print_float constraints.(i).(k);
	print_string "*x";
	print_int k;
      end;
    done;

    print_newline()

  method print () =
    for i = 0 to ncons-1 do
      this#print_constraint i;
    done;
    for i = 0 to nvar + ncons + 1 do
      print_string "-------";
    done;
    print_newline();
    print_string "z = ";
    print_float (objective.(0));
    for i = 1 to nvar + ncons do
      print_string " + ";
      print_float (objective.(i));
      print_string "*x"; print_int i;
    done;
end;;
