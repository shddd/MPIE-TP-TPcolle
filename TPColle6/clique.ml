type graph = (int * (int list)) list
(* liste de listes d'adjacence : le graphe contient la liste des (i, liste des voisins de i) *)


let cardinal_clique (c : bool array) =
	let res = ref 0 in
	for i = 0 to (Array.length c) - 1 do
		if c.(i) then res := !res + 1
	done;
	!res

let print_clique (c : bool array) : unit =
	for i = 0 to (Array.length c)-1 do
		if c.(i) then
			begin
			print_int i ;
			print_string " "
			end
	done;
	print_newline()

(* on suppose que g est non orienté *)
let rec arete_existe (g : graph) (i : int) (j : int) : bool =
	let rec est_voisin (l : int list) (j : int) : bool =
		match l with
		| [] -> false
		| k::t -> if k = j then true else est_voisin t j
	in
	match g with
	| [] -> false
	| (k,l)::t -> if k = i then est_voisin l j else arete_existe t i j

let est_clique (g : graph) (c : bool array) =
	if List.length g <> Array.length c then
		failwith "Erreur de taille dans est_clique"
	else
		let res = ref true in
		let i = ref 0 in
		while !res && (!i < Array.length c) do
			if c.(!i) then
				begin
				let j = ref (!i+1) in
				while (!res && !j < Array.length c) do
					if c.(!j) then
						begin
						res := arete_existe g (!i) (!j)
						end;
					j := !j+1
				done
				end;
			i := !i+1
		done;
		!res

let clique (g : graph) (k : int) : bool =
	let rec aux (i : int) (k : int) (c : bool array) : bool =
		let n = Array.length c in
		if i = n then
			if cardinal_clique c >= k then ( print_clique c ; true ) else false
		else
			begin
			c.(i) <- true ;
			if (est_clique g c) then 
				if cardinal_clique c >= k then (print_clique c ; true)
				else (aux (i+1) k c) 
			else 
				begin
				c.(i) <- false;
				if (est_clique g c) then 
					if cardinal_clique c >= k then (print_clique c ; true)
					else (aux (i+1) k c) 
				else
					false
				end 
			end
	in
	aux 0 k (Array.make (List.length g) false)

(*
let g1 = [ (0,[1;4]) ; (1,[0;2;4]) ; (2,[1;3]) ; (3,[2;4;5]) ; (4,[0;1;3]) ; (5,[3]) ]
let g2 = [ (0,[2;3;4]) ; (1,[2;3;4]) ; (2,[0;1;3]) ; (3,[0;1;2]) ; (4,[0;1]) ]
*)

(* __________ Commencez ici votre code pour la partie Cliques __________ *)

type clause = int array;;
type cnf = clause array;;

(*c(j, 1) = j
  c(j, 2) = j + l 
  c(j, 3) = j + 2*l
*)
let sommets_clause (j : int) (p : int) : int =
  j + p * 3
;;

let convertir matrix : graph =
  let num_vertices = Array.length matrix in
  let adjacency_list = ref [] in
  (* Loop over the matrix, adding a tuple (neighbor, weight) to the
     adjacency list for each non-zero entry in the matrix *)
  for i = 0 to num_vertices - 1 do
    let neighbors = ref [] in
    for j = 0 to num_vertices - 1 do
      if matrix.(i).(j) then neighbors := j :: !neighbors
    done;
    adjacency_list := (i, !neighbors) :: !adjacency_list
  done;
  !adjacency_list
;;

let reduction (g,k) (clauses:cnf) : cnf =
  let clique_vars = clique g k in
  let clique_var_set = Array.to_set clique_vars in
  List.filter (fun clause -> not (Set.is_subset clique_var_set (Array.to_set clause))) clauses
  ;;
