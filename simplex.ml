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
  val variables : int array = Array.init nconsP (fun x -> x + nvarP + 1)
  (* On repère quelle est la variable de chaque ligne que l'on assigne *)

  (* Méthodes *)
  method print () =

end;;
