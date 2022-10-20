type 'a mot = 'a list

type 'a automate = {
	nb : int ; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
	sigma : 'a array ;
	i : int list ;
	f : int list ;
	delta : (int * 'a, int list) Hashtbl.t
}

let ajouter_transition (a : 'a automate) (q1 : int) (lettre : 'a) (q2 : int) : unit =
	assert((q1 >= 0) && (q1 < a.nb));
	assert((q2 >= 0) && (q2 < a.nb));
	assert(Array.mem lettre (a.sigma));
	match Hashtbl.find_opt (a.delta) (q1,lettre) with
	| None -> Hashtbl.add (a.delta) (q1,lettre) [q2]
	| Some l -> if not (List.mem q2 l) then
		    Hashtbl.replace (a.delta) (q1,lettre) (q2::l)

let affiche_transitions (a : 'a automate) (print_lettre : 'a -> unit) : unit =
	for i = 0 to a.nb-1 do
		for j = 0 to (Array.length a.sigma)-1 do
			match Hashtbl.find_opt a.delta (i,a.sigma.(j)) with
			| None -> ()
			| Some l -> print_int i ;
				    print_string "," ;
				    print_lettre a.sigma.(j) ;
				    print_string " -> " ;
				    List.iter (fun x -> print_int x ; print_string " ") l ;
				    print_newline ()
		done
	done

let affiche_automate (a : 'a automate) (print_lettre : 'a -> unit) : unit =
	print_string "I : ";
	List.iter (fun x -> print_int x ; print_string " ") a.i ;
	print_newline ();
	print_string "F : ";
	List.iter (fun x -> print_int x ; print_string " ") a.f ;
	print_newline ();
	affiche_transitions a print_lettre

let affiche_automate_char (a : 'a automate) : unit = affiche_automate a print_char


(* Exemple : automate a0 *)
let (a0 : char automate) = { nb = 6 ; sigma = [|'a';'b'|] ; i = [0] ; f = [1;4] ;
delta = Hashtbl.create 36}

let _ = ajouter_transition a0 0 'a' 1
let _ = ajouter_transition a0 0 'b' 0
let _ = ajouter_transition a0 1 'a' 3
let _ = ajouter_transition a0 1 'b' 2
let _ = ajouter_transition a0 2 'a' 5
let _ = ajouter_transition a0 2 'b' 1
let _ = ajouter_transition a0 3 'a' 4
let _ = ajouter_transition a0 3 'b' 0
let _ = ajouter_transition a0 4 'a' 0
let _ = ajouter_transition a0 4 'b' 5
let _ = ajouter_transition a0 5 'a' 5
let _ = ajouter_transition a0 5 'b' 1


(* Exemple : automate a1 *)
let (a1 : char automate) = { nb = 4 ; sigma = [|'a';'b'|] ; i = [0 ; 1] ; f = [2] ; 
delta = Hashtbl.create 16}

let _ = ajouter_transition a1 0 'b' 1
let _ = ajouter_transition a1 0 'b' 2
let _ = ajouter_transition a1 1 'a' 1
let _ = ajouter_transition a1 1 'a' 2
let _ = ajouter_transition a1 1 'b' 3
let _ = ajouter_transition a1 2 'a' 2
let _ = ajouter_transition a1 2 'a' 0
let _ = ajouter_transition a1 2 'b' 3
let _ = ajouter_transition a1 3 'b' 1
let _ = ajouter_transition a1 3 'b' 2



(* verifie si un automate donné est déterministe *)
let est_deterministe (a : 'a automate) : bool =
	if List.length (a.i) <> 1 then
		false
	else
		let deter = ref true in (*  tant qu'on n'a pas trouvé de doublon *)
		let etat = ref 0 in
		while (!deter) && (!etat < a.nb) do
			let j = ref 0 in
			while (!deter) && (!j < (Array.length a.sigma)) do
				(match Hashtbl.find_opt a.delta (!etat,a.sigma.(!j)) with
				| None -> ()
				| Some(l) -> deter := ((List.length l) <= 1)
				) ;
				j := !j+1;
			done ;
			etat := !etat+1
		done;
		!deter


(* teste si un élément i appartient à l'ensemble e *)
let appartient (i : int) (e : bool array) : bool = e.(i)

(* construit la partie vide de [0;N-1] *)
let vide (n : int) : bool array = Array.make n false

(* vérifie si une partie de [0;N-1] représentée par e est vide *)
let est_vide (e : bool array) : bool = not (Array.mem true e)

(* construit l'union de deux parties représentées par un tableau *)
let union (e1 : bool array) (e2 : bool array) : bool array =
	if Array.length e1 <> Array.length e2 then
		failwith "erreur dans union : les deux tableaux n'ont pas la meme taille"
	else
		begin
			let e = Array.make (Array.length e1) false in
			for i = 0 to (Array.length e1)-1 do
				e.(i) <- e1.(i) || e2.(i)
			done;
			e
		end
		
(* construit l'intersection de deux parties représentées par un tableau *)
let intersection (e1 : bool array) (e2 : bool array) : bool array =
	if Array.length e1 <> Array.length e2 then
		failwith "erreur dans intersection : les deux tableaux n'ont pas la meme taille"
	else
		begin
			let e = Array.make (Array.length e1) false in
			for i = 0 to (Array.length e1)-1 do
				e.(i) <- e1.(i) && e2.(i)
			done;
			e
		end


(* modifie le tableau e1 de sorte que e1 devienne l'union de e1 et e2*)
let ajoute (e1 : bool array) (e2 : bool array) : unit =
	if Array.length e1 <> Array.length e2 then
		failwith "erreur dans union : les deux tableaux n'ont pas la meme taille"
	else
		begin
			for i = 0 to (Array.length e1)-1 do
				e1.(i) <- e1.(i) || e2.(i)
			done
		end

(* construit la partie de [0;N-1] contenant les états mentionnés dans state_list *)
let construit (state_list : int list) (n : int) : bool array =
	let arr = Array.make n false in
	let rec aux (li : int list) : bool array =
		match li with
		| [] -> arr
		| s::t -> arr.(s) <- true ; aux t
	in aux state_list

(* renvoie un tableau représentant les états où on arrive en lisant l dans l'automate a depuis l'état q *)
let lire_lettre_etat (a : 'a automate) (q : int) (letter : 'a) : bool array =
	let arr = Array.make (a.nb) false in
	
	(* on récupère l'ensemble des états stockés dans la table de hachage pour l'état q et la lettre "letter" *)
	match Hashtbl.find_opt (a.delta) (q,letter) with
	| None -> arr
	| Some l ->
		let rec aux (state_list : int list) : bool array =
			match state_list with
			| [] -> arr
			| q2::s -> arr.(q2) <- true ;
					   aux s
		in aux l

(* renvoie un tableau représentant les états où on arrive en lisant l dans l'automate a depuis l'ensemble d'états représentés par p *)
let lire_lettre_partie (a : 'a automate) (p : bool array) (letter : 'a) : bool array =
	let arr = Array.make (a.nb) false in
	for i = 0 to (Array.length p) - 1 do
		if p.(i) then 
			let a_ajouter = lire_lettre_etat a i letter in
			ajoute arr a_ajouter
	done;
	arr

(* renvoie un tableau représentant les états où l'on peut arriver en lisant le mot u depuis un des états de la partie e de l'ensemble des états *)
let rec lire_mot (a : 'a automate) (p : bool array) (u : 'a mot) : bool array =
	match u with
	| [] -> p
	| l::t -> let arr = lire_lettre_partie a p l in
		  lire_mot a arr t
		  
(* teste si un mot u est accepté par l'automate a *)
let rec accepte_mot (a : 'a automate) (u : 'a mot) : bool =
	let initial_arr = construit a.i a.nb in
	let accepting_arr = construit a.f a.nb in
	let end_arr = lire_mot a initial_arr u in
	not (est_vide (intersection end_arr accepting_arr))
	
(* code une partie représentée par e de taille n par un entier < 2^n *)
let vers_entier (e : bool array) : int =
	let res = ref 0 in
	for i = 0 to (Array.length e)-1 do
		res := !res * 2 ;
		if (e.(i)) then res := !res + 1
	done;
	!res


(* code le nombre p en binaire dans un tableau à n bits *)
let vers_partie (p : int) (n : int) : bool array =
	if p < 0 then
		failwith "erreur dans vers_partie : entier négatif"
	else
		begin
		let e = Array.make n false in
		let i = ref 1 in
		let q = ref p in
		while (!i <= n) do
			e.(n-(!i)) <- (!q mod 2 = 1);
			q := !q/2;
			i := !i+1
		done;
		if n < !i && !q <> 0 then
			failwith "erreur dans vers_partie : arguments invalides"
		else e
		end

(* calcule a^n *)
let rec puissance (a : int) (n : int) : int =
	if n < 0 then
		failwith "erreur dans puissance : n < 0"
	else
		begin
		if n = 0 then 1
		else
			let b = (puissance a (n/2)) in
			if n mod 2 = 0 then b*b
			else b*b*a
		end



(* transforme un automate en un automate déterministe *)
let determinise (a : 'a automate) : 'a automate =

	(* chaque état du nouvel automate sera numéroté par le numéro de la partie qu'il représente dans l'automate initial *)
	let new_nb = (puissance 2 a.nb) in
	let new_i = vers_entier (construit a.i a.nb) in
	let new_delta = Hashtbl.create (new_nb * new_nb) in
	(* l'ensemble des états acceptants de l'automate initial *)
	let partie_acceptants = construit a.f a.nb in

	(* ce tableau indique si un état a déjà été rencontré dans le nouvel automate ou non *)
	let deja_traite = Array.make new_nb false in

	(* on s'apprête à traiter la transition étiquetée par la j-ième lettre partant de q dans le nouvel automate *)
	(*  reste est l'accumulateur des états encore à traiter *)
	(* accept est l'accumulateur des états acceptants *)
	let rec aux (q : int) (j : int) (reste : int list) (accept : int list) : int list = 
		(* si on a regardé toutes les lettres pour l'état courant, on a fini de le traiter *)
		if j = (Array.length a.sigma) then
			begin
			deja_traite.(q) <- true;
			match reste with
			| [] -> accept
			| q'::t -> aux q' 0 t accept
			end
		else 
			let partie = vers_partie q a.nb in
			let nouvelle_partie = lire_lettre_partie a partie (a.sigma.(j)) in
			let nouveau_q = vers_entier nouvelle_partie in
			if nouveau_q = 0 then
				aux q (j+1) reste accept
			else 
				begin
				Hashtbl.add new_delta (q,a.sigma.(j)) [nouveau_q] ;
				let new_reste = 
					if (not (deja_traite.(nouveau_q))) && (not (List.mem nouveau_q reste)) then
						nouveau_q::reste
					else
						reste
					in
				let new_accept = 
					if (not (List.mem nouveau_q accept)) && (not (est_vide (intersection partie_acceptants nouvelle_partie))) then
						nouveau_q::accept
					else
						accept
					in 
					aux q (j+1) new_reste new_accept
				end
			
	in
	let new_f = aux new_i 0 [] [] in
	{nb = new_nb ; sigma = a.sigma ; i = [new_i] ; f = new_f ; delta = new_delta}

