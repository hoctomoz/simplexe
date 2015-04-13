open Simplex;;
open Parser;;

(* Main *)
let pb = new simplex 3 2 [] [];;
let o = [|0.; 2.; 3.; 4.; 0.; 0.; 0. |] in
pb#setObjective o;;
let m = [|[|10.; -3.; -2.; -1.; -1.; 0.; 0. |];
	  [|15.; -2.; -5.; -3.; 0.; -1.; 0. |]
	|] in
pb#setConstraints m;;

pb#print();;
pb#solve();;
print_string (pb#latex());;
