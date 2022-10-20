type 'a mot = 'a list

type 'a automate = {
	nb : int ; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
	sigma : 'a array ;
	i : int ;
	f : int list ;
	delta : (int * 'a, int) Hashtbl.t
}

let ajouter_transition (a : 'a automate) (q1 : int) (lettre : 'a) (q2 : int) : unit =
	assert((q1 >= 0) && (q1 < a.nb));
	assert((q2 >= 0) && (q2 < a.nb));
	assert(Array.mem lettre (a.sigma));
	match Hashtbl.find_opt (a.delta) (q1,lettre) with
	| None -> Hashtbl.add (a.delta) (q1,lettre) q2
	| Some l -> Hashtbl.replace (a.delta) (q1,lettre) (q2)

let affiche_transitions (a : 'a automate) (print_lettre : 'a -> unit) : unit =
	for i = 0 to a.nb-1 do
		for j = 0 to (Array.length a.sigma)-1 do
			match Hashtbl.find_opt a.delta (i,a.sigma.(j)) with
			| None -> ()
			| Some l -> print_int i ;
				    print_string "," ;
				    print_lettre a.sigma.(j) ;
				    print_string " -> " ;
				    print_int l ;
				    print_newline ()
		done
	done

let affiche_automate (a : 'a automate) (print_lettre : 'a -> unit) : unit =
	print_string "I : "; print_int a.i ; print_string " " ;
	print_newline ();
	print_string "F : ";
	List.iter (fun x -> print_int x ; print_string " ") a.f ;
	print_newline ();
	affiche_transitions a print_lettre

let affiche_automate_char (a : 'a automate) : unit = affiche_automate a print_char

let (a0 : char automate) = { nb = 8 ; sigma = [|'a';'b'|] ; i = 0 ; f = [0;3;4;6;7] ;
delta = Hashtbl.create 36}

let _ = ajouter_transition a0 0 'a' 2
let _ = ajouter_transition a0 0 'b' 1
let _ = ajouter_transition a0 1 'a' 2
let _ = ajouter_transition a0 1 'b' 1
let _ = ajouter_transition a0 2 'a' 3
let _ = ajouter_transition a0 2 'b' 2
let _ = ajouter_transition a0 3 'a' 5
let _ = ajouter_transition a0 3 'b' 4
let _ = ajouter_transition a0 4 'a' 5
let _ = ajouter_transition a0 4 'b' 4
let _ = ajouter_transition a0 5 'a' 6
let _ = ajouter_transition a0 5 'b' 5
let _ = ajouter_transition a0 6 'a' 5
let _ = ajouter_transition a0 6 'b' 7
let _ = ajouter_transition a0 7 'a' 5
let _ = ajouter_transition a0 7 'b' 7


(*__________Partie III : algorithme de Moore__________*)

let relation_suivante (a : 'a automate) (r : int array) : int array =
	assert(Array.length r = a.nb);
	
	(* on crée la matrice qui permet de vérifier dans quelle classe on arrive après chaque lettre *)
	let m = Array.make_matrix (a.nb) (Array.length (a.sigma)) (-1) in
	
	(* pour chaque état *)
	for i = 0 to a.nb - 1 do
		(* pour chaque lettre *)
		for j = 0 to (Array.length (a.sigma))-1 do
			(* on suppose nos automates déterministes complet, donc find trouvera toujours un résultat *)
			(* on cherche l'état où aboutit la transition i/a_j*)
			
			let k = Hashtbl.find (a.delta) (i,a.sigma.(j)) in
			(* r.(k) est sa classe d'équivalence *)
			m.(i).(j) <- r.(k)
		done
	done;

	(* on crée maintenant le tableau de la nouvelle relation r' *)
	let r' = Array.make (a.nb) (-1) in
	for i = 0 to a.nb-1 do
		(* si la classe de i est déjà donnée, il n'y a rien à faire*)
		(* sinon, alors il obtient sa propre classe d'équivalence *)
		if (r'.(i) = -1) then 
			begin
			r'.(i) <- i;
			(* et il faut chercher quels autres états ont la même classe *)
			for k = i+1 to a.nb-1 do
				if m.(i) = m.(k) && r.(i) = r.(k) then r'.(k) <- i
			done
			end
	done;
	r' 

(* calcule la limite de la suite de relations *)
let convergence (a : 'a automate) : int array =
	let r = Array.make (a.nb) (-1) in
	let indice_f = List.hd (a.f) in
	
	(* servira à garder le premier indice d'un état non acceptant *)
	let x = ref 0 in
	let trouve = ref false in
	(* on initialise le tableau r *)
	for i = 0 to a.nb-1 do
		if List.mem i a.f then
			r.(i) <- indice_f
		else
			begin
			if not (!trouve) then
				begin
				trouve := true;
				x := i
				end;
			r.(i) <- !x
			end
	done;
	
	(* on cherche la limite *)
	let rec aux (b : int array) : int array =
		let c = relation_suivante a b in
		if b = c then c
		else aux c
	in
	aux r

(* renomme le tableau précédemment obtenu *)
let renommage (r : int array) : int array =
	
	(* sert à mapper les anciens représentants de classe d'équivalence sur les nouveaux numéros de classe *)
	let rencontre = Array.make (Array.length r) false in
	let mapping = Array.make (Array.length r) (-1) in
	let prochain_entier = ref 0 in
	
	for i = 0 to (Array.length r)-1 do
		if not (rencontre.(r.(i))) then
			begin
			rencontre.(i) <- true;
			mapping.(r.(i)) <- !prochain_entier;
			prochain_entier := !prochain_entier + 1
			end
	done;

	(* le tableau qu'on renverra *)
	let s = Array.init (Array.length r) (fun x -> mapping.(r.(x))) in
	s

let moore (a : 'a automate) : 'a automate =
	let r = convergence a in
	let s = renommage r in
	let new_delta = Hashtbl.create (a.nb * a.nb) in
	
	let aux_max (a : int array) : int =
		let max = ref 0 in
		for i = 0 to (Array.length a) -1 do
			if a.(i) > !max then max := a.(i)
		done;
		!max+1
	in
	
	let rec aux_f (i : int) (acc : int list) : int list =
		if i = a.nb then acc
		else if not (List.mem i (a.f)) then aux_f (i+1) acc
		else if not (List.mem (s.(i)) acc) then aux_f (i+1) ((s.(i))::acc)
		else aux_f (i+1) acc
	in
	
	for q = 0 to (a.nb)-1 do
		for j = 0 to (Array.length (a.sigma))-1 do
			let b = a.sigma.(j) in
			let q' = Hashtbl.find (a.delta) (q,b) in
			Hashtbl.add new_delta (s.(q),b) (s.(q'))
		done
	done;
	{ nb = (aux_max s) ; sigma = a.sigma ; i = s.(a.i) ; f = aux_f 0 [] ;
delta = new_delta}
