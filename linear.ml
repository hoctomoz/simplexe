let printArray a = Array.iter (fun x -> print_float x; print_char ' ') a;;

let printMatrix m = Array.iter (fun a -> printArray a; print_newline()) m;;

let ( *% ) lambda a = Array.map (fun x -> lambda *. x) a;;

let normalize i a = (1. /. a.(i)) *% a;;

let substitute i a b =
(* Substitue la variable i de a par l'équation donnée dans b *)
  if b.(i) <> 1. then failwith "Erreur dans substitute : équation non normalisée";
  
  let coeff = a.(i) in
  Array.iteri (fun k -> fun x -> a.(k) <- x -. coeff*.b.(k)) a;;

let substituteMatrix i j m =
(* Substitue la variable j de m.(k) par l'équation donnée dans m.(i), pour k <> i *) 
  Array.iteri (fun k -> fun a -> substitute j a m.(i));;

let enteringVariable const a =
(* Renvoie le plus petit indice i tel que a.(i) > 0. *)
(* const correspond à la valeur contraignant (la constante), qui est stockée en position 0 dans notre modèle (on aura const = 0) *)
  let n = Array.length a in
  let k = ref 0 in
  let check = ref true in

  while (!k < n && !check) do
    if !k <> const then begin
      if a.(!k) > 0.
      then check := false
      else incr k
    end;
  done;
    
  if !check then None
  else Some !k
;;

let leavingVariable i const m =
(* Renvoie l'indice de la ligne de m où l'équation est la plus contraignante pour i *)
(* const correspond à la valeur contraignant (la constante), qui est stockée en position 0 dans notre modèle (on aura const = 0) *)
  let n = Array.length m in
  let min = ref 0. in
  let indice = ref None in

  for k = 0 to n-1 do
    let a = m.(k) in
    if a.(i) < 0. then begin
      let coeff = -. a.(const) /. a.(i) in
      if coeff > !min then (min := coeff; indice := Some k);
    end;
  done;

  !indice
;;
