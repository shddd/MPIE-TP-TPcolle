type 'a mot = 'a list;;

type 'a automate = {
	nb : int ; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
	sigma : 'a array ;
	i : int list ;
	f : int list ;
	delta : (int * 'a, int list) Hashtbl.t
};;

let ajouter_transition (a : 'a automate) (q1 : int) (lettre : 'a) (q2 : int) : unit =
	assert((q1 >= 0) && (q1 < a.nb));
	assert((q2 >= 0) && (q2 < a.nb));
	assert(Array.mem lettre (a.sigma));
	match Hashtbl.find_opt (a.delta) (q1,lettre) with
	| None -> Hashtbl.add (a.delta) (q1,lettre) [q2]
	| Some l -> if not (List.mem q2 l) then
		    Hashtbl.replace (a.delta) (q1,lettre) (q2::l)
;;
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
  ;;

let affiche_automate (a : 'a automate) (print_lettre : 'a -> unit) : unit =
	print_string "I : ";
	List.iter (fun x -> print_int x ; print_string " ") a.i ;
	print_newline ();
	print_string "F : ";
	List.iter (fun x -> print_int x ; print_string " ") a.f ;
	print_newline ();
	affiche_transitions a print_lettre
;;
let affiche_automate_char (a : 'a automate) : unit = affiche_automate a print_char;;

let (a0 : char automate) = { nb = 8 ; sigma = [|'a';'b'|] ; i = [0] ; f = [2;6] ;
delta = Hashtbl.create 36};;

let _ = ajouter_transition a0 0 'a' 1;;
let _ = ajouter_transition a0 0 'b' 5;;
let _ = ajouter_transition a0 1 'a' 6;;
let _ = ajouter_transition a0 1 'b' 2;;
let _ = ajouter_transition a0 2 'a' 0;;
let _ = ajouter_transition a0 2 'b' 2;;
let _ = ajouter_transition a0 3 'a' 2;;
let _ = ajouter_transition a0 3 'b' 6;;
let _ = ajouter_transition a0 4 'a' 7;;
let _ = ajouter_transition a0 4 'b' 5;;
let _ = ajouter_transition a0 5 'a' 2;;
let _ = ajouter_transition a0 5 'b' 6;;
let _ = ajouter_transition a0 6 'a' 6;;
let _ = ajouter_transition a0 6 'b' 4;;
let _ = ajouter_transition a0 7 'a' 6;;
let _ = ajouter_transition a0 7 'b' 2;;


(*__________Partie II : problème de l'inclusion__________*)

(* transforme un automate en automate complet *)
let completer (a : 'a automate) : 'a automate =

	(* on va construire un nouvel objet automate, auquel on va peut-être adjoindre une poubelle *)
	let creer_poubelle =  ref false in
	(* une nouvelle table de hachage, dans laquelle on va recopier les transitions existantes et ajouter celles qui manquent *)
	let new_delta = Hashtbl.create (a.nb*a.nb) in
	
	(* on va parcourir toutes les transitions possibles *)
	for q = 0 to a.nb do
		for j = 0 to (Array.length (a.sigma))-1 do
		
			(* on cherche s'il existe des transitions (q,a_j) *)
			match Hashtbl.find_opt a.delta (q,a.sigma.(j)) with
			| Some l -> Hashtbl.add new_delta (q,a.sigma.(j)) l (* si oui, on les recopie *)
			| None -> begin
					(* si non, on les crée vers l'état a.nb, la poubelle*)
					creer_poubelle := true ;
					Hashtbl.add new_delta (q,a.sigma.(j)) [a.nb]
				  end
		done;
	done;
	
	(* si on a rajouté une poubelle, on la fait boucler sur elle-même *)
	if !creer_poubelle then
		for j = 0 to (Array.length (a.sigma))-1 do
			Hashtbl.add new_delta (a.nb,a.sigma.(j)) [a.nb]
		done;
	
	(* on crée enfin le nouvel automate *)
	let new_nb = if !creer_poubelle then a.nb+1 else a.nb in 
	{ nb = new_nb ; sigma = a.sigma ; i = a.i ; f = a.f ;
delta = new_delta} 
;;
(* transforme un automate en automate complet ; on suppose que l'automate d'entrée est déjà déterministe *)
let complementaire (a : 'a automate) : 'a automate =

	(* on complète l'automate a *)
	let a' = completer a in
	
	(* on veut maintenant échanger les états acceptants et non acceptants *)
	let new_f = ref [] in
	for i = 0 to a'.nb-1 do
		if not (List.mem i (a'.f)) then
			new_f := i::(!new_f)
	done;
	
	(* on crée enfin le nouvel automate *)
	{ nb = a'.nb ; sigma = a'.sigma ; i = a'.i ; f = !new_f ;
delta = a'.delta} 
;;

(* construit l'automate produit, reconnaissant l'intersection des deux langages *)
(* on procède ici de manière naïve en construisant toutes les paires *)
(* on suppose a1 et a2 déjà déterministes, et sur le même alphabet *)
let intersection (a1 : 'a automate) (a2 : 'a automate) : 'a automate =

	(* la paire (i,j) de (a1,a2) sera représentée par l'entier ni+j dans l'automate produit *)
	let m = a1.nb in
	let n = a2.nb in
	let new_nb = m*n in

	(* on commence par construire toutes les transitions *)
	let new_delta = Hashtbl.create (new_nb*new_nb) in
	for q1 = 0 to m-1 do
		for q2 = 0 to n-1 do
			for j = 0 to (Array.length (a1.sigma))-1 do
				let aj = a1.sigma.(j) in
				match (Hashtbl.find_opt (a1.delta) (q1,aj), Hashtbl.find_opt (a2.delta) (q2,aj)) with
				| Some [q1'], Some [q2'] -> Hashtbl.add new_delta (n*q1+q2,aj) [n*q1'+q2']
				| _ -> ()
			done;
		done;
	done;

	(* on repère quels états sont initiaux dans l'automate produit *)
	let new_i = ref [] in
	for q = 0 to new_nb-1 do
		let (q1,q2) = (q/n, q mod n) in
		if (List.mem q1 (a1.i)) &&  (List.mem q2 (a2.i)) then
			new_i := q::(!new_i)
	done;
	
	(* on repère quels états sont initiaux dans l'automate produit *)
	let new_f = ref [] in
	for q = 0 to new_nb-1 do
		let (q1,q2) = (q/n, q mod n) in
		if (List.mem q1 (a1.f)) &&  (List.mem q2 (a2.f)) then
			new_f := q::(!new_f)
	done;
	
	{ nb = new_nb ; sigma = a1.sigma ; i = !new_i ; f = !new_f ;
delta = new_delta} 
;;


(*__________Partie III : minimisation d'automates__________*)


(* initialisation du tableau de separation *)
let init_separation (a : 'a automate) : bool array array =
	let n = a.nb in
	(* on crée le tableau de séparation des états *)
	let tab = Array.make_matrix n n false in
	(* on l'initialise en marquant comme séparables les paires avec exactement un état acceptant *)
	for p = 0 to a.nb-1 do
		let p_final = List.mem p a.f in
		for q = 0 to a.nb-1 do
			let q_final = List.mem q a.f in
			(* si exactement un état est acceptant, la paire est séparable *)
			if (p_final && (not q_final)) || ((not p_final) && q_final) then
				tab.(p).(q) <- true	
		done
	done;
	tab
;;
(* on realise UNE iteration de la separation, en regardant toutes les paires d'états et chaque lettre *)
(* on renvoie true si on a trouvé une paire à séparer, et qu'on devra continuer *)
(* sinon, on renvoie false *)
let etape_de_separation (a : 'a automate) (tab : bool array array) : bool =
	let n = a.nb in
	let continue = ref false in
	
	(* pour toute paire (p,q)*)
	for p = 0 to n-1 do
		for q = 0 to n-1 do
		
			(* si la paire n'est pas déjà séparée, on regarde si on peut la séparer *)
			if not (tab.(p).(q)) then
				(* parcours également sur les lettres *)
				for j = 0 to (Array.length (a.sigma))-1 do
					let aj = a.sigma.(j) in
					match (Hashtbl.find_opt (a.delta) (p,aj), Hashtbl.find_opt (a.delta) (q,aj)) with
					| Some [p'], Some [q'] -> 
						(* si p' et q' sont séparables *)
						if tab.(p').(q') then
						begin
						(* alors p et q sont séparables, et on continuera *)
						tab.(p).(q) <- true;
						continue := true
						end
					| _ -> ()
				done
		done
	done;
	!continue
	;;	

let separation (a : 'a automate) : bool array array =
	let tab = init_separation a in
	
	(* on effectue des étapes de séparation tant que c'est possible *)
	let continue = ref true in
	while !continue do
		continue := etape_de_separation a tab
	done;
	tab
;;


(* fonction separation, faite en une seule fois pour les plus déterminés *)
let separation_une (a : 'a automate) : bool array array =
	let n = a.nb in
	
	(* on crée le tableau de séparation des états *)
	let tab = Array.make_matrix n n false in
	(* on l'initialise en marquant comme séparables les paires avec exactement un état acceptant *)
	for p = 0 to a.nb do
		let p_final = List.mem p a.f in
		for q = 0 to a.nb do
			let q_final = List.mem q a.f in
			(* si exactement un état est acceptant, la paire est séparable *)
			if (p_final && (not q_final)) || ((not p_final) && q_final) then
				tab.(p).(q) <- true	
		done
	done ;
	
	(* on va maintenant marquer de nouvelles paries tant que c'est possible *)
	let continue = ref true in
	while !continue do
		(* on décidera de continuer SI on peut séparer de nouvelles paires *)
		continue := false;
		for p = 0 to a.nb do
			for q = 0 to a.nb do
				(* parcours également sur les lettres *)
				for j = 0 to (Array.length (a.sigma))-1 do
					let aj = a.sigma.(j) in
					match (Hashtbl.find_opt (a.delta) (p,aj), Hashtbl.find_opt (a.delta) (q,aj)) with
					| Some [p'], Some [q'] -> 
						if tab.(p').(q') then
						begin
						tab.(p).(q) <- true;
						continue := true (* on a marqué une paire, donc on continuera *)
						end
					| _ -> ()
				done
			done
		done
	done;
	tab
;;
(*__________Partie IV : Lemme d'Arden__________*)
type 'a ere =
Vide
| Epsilon
| Lettre of 'a
| Var of int
| Union of ('a ere)*('a ere)
| Concat of ('a ere)*('a ere)
| Etoile of ('a ere)
;;
(* affiche une char ere, pour s'aider à visualiser des exemples *)
let rec affiche (e : char ere) : unit =
	match e with
	| Vide -> print_string "O"
	| Epsilon -> print_string "E"
	| Lettre a -> print_char a
	| Var i -> print_string "x" ; print_int i
	| Union(e1,e2) -> affiche e1 ; print_string " | " ; affiche e2
	| Concat(e1,e2) -> print_string "(" ; affiche e1 ; print_string ")(" ; affiche e2 ; print_string ")"
	| Etoile(e1) -> print_string "(" ; affiche e1 ; print_string ")*"
;;
(* affiche un système d'équations, pour s'aider à visualiser des exemples *)
let rec affiche_systeme (s : (('a ere)*('a ere)) list) : unit =
	match s with
	| [] -> ()
	| e::t -> let (e1,e2) = e in
		  affiche e1 ; print_string " = " ; affiche e2 ; print_newline () ; affiche_systeme t
;;
(* donne l'équation associée à l'état q dans l'automate a *)
(* on suppose que a est déterministe *)
let equation (a : 'a automate) (q : int) : ('a ere)*('a ere) =
	assert((q >= 0) && (q < a.nb));
	let e = ref Vide in 
	
	(* pour chaque lettre qui étiquette une transition partant de q *)
	(* on va eugmenter e *)
	for j = 0 to (Array.length (a.sigma))-1 do
		match Hashtbl.find_opt (a.delta) (q,a.sigma.(j)) with
		| Some [q'] -> e := Union(!e,Concat( Lettre(a.sigma.(j)) , Var q' ))
		| _ -> ()
	done;
	
	(* si rien ne part de l'état q, on ne renvoie que Vide = Vide *)
	if !e = Vide then
		(Vide,Vide)
	else	(* sinon, on renvoie x_q = e *)
		(Var q, !e);;

(* donne le système d'équation associé à l'automate a *)
let systeme (a : 'a automate) : (('a ere)*('a ere)) list =

	(* fonction auxiliaire pour garder l'état courant et les équations déjà construites *)
	let rec aux (q : int) (acc : (('a ere)*('a ere)) list) : (('a ere)*('a ere)) list =
		if q = a.nb then acc
		else
			match equation a q with
			| Vide,Vide -> aux q acc
			| e1,e2 -> aux (q+1) ((e1,e2)::acc)
	
	in aux 0 [];;
