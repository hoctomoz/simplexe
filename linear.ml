let print_array m = Array.iter (fun x -> print_float x; print_char ' ') m;;

let normalize i a =
  let coeff = - 1. /. a.(i) in
  Array.iteri (fun k -> fun x -> a.(k) <- coeff *. x) a;;

let substitute i a b =
(* Substitue la variable i de a par l'équation donnée dans b *)
  if b.(i) <> -1. then failwith "Erreur dans substitute : équation non normalisée";
  
  let coeff = a.(i) in
  Array.iteri (fun k -> fun x -> a.(k) <- x +. coeff*.b.(k)) a;;

let substituteMatrix i j m =
(* Substitue la variable j de m.(k) par l'équation donnée dans m.(i), pour k <> i *) 
  Array.iteri (fun k -> fun a -> if k <> i then substitute j a m.(i)) m;;

let enteringVariable a =
(* Renvoie le plus petit indice i tel que a.(i) > 0. *)
(* La constante est localisée dans a.(0) *)
  let n = Array.length a in
  let k = ref 1 in
  let check = ref true in

  while (!k < n && !check) do
    if a.(!k) > 0.
    then check := false
    else incr k
  done;
    
  if !check then None
  else Some !k
;;

let leavingVariable i m =
(* Renvoie l'indice de la ligne de m où l'équation est la plus contraignante pour i *)
(* La constante est localisée dans a.(0) *)

  let n = Array.length m in
  let min = ref infinity in
  let indice = ref None in

  for k = 0 to n-1 do
    let a = m.(k) in
    if a.(i) < 0. then begin
      let coeff = -. a.(0) /. a.(i) in
      if coeff < !min then (min := coeff; indice := Some k);
    end;
  done;

  !indice
;;

let negativeConstant m =
  let n = Array.length m in
  let check = ref false in
  let k = ref 0 in

  while (!k < n && not(!check)) do
    if m.(!k).(0) < 0. then check := true;
    incr k;
  done;

  !check

let leavingx0 m =
(* Renvoie l'indice de la ligne de m que l'on doit échanger avec x0 lors de la première étape de la first phase ie. celle où la constante est la plus petite (et elle est négative) *)
  let n = Array.length m in
  let min = ref 0. in
  let indice = ref (-1) in

  for k = 0 to n-1 do
    let const = m.(k).(0) in
    if const < !min then (min := const; indice := k);
  done;

  if (!indice = -1) then failwith "Cas impossible dans leavingx0 : la constante minimale doit être négative";

  !indice
