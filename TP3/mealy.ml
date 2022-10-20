type 'a mot = 'a list;;

type ('a,'b) mealy = {
	i : int ;
	delta : (int * 'a, int * 'b) Hashtbl.t ; (* on fusioinne en fait delta et lambda dans une seule table *)
};;


let ajouter_transition (m : ('a,'b) mealy) (q1 : int) (lettre : 'a) (q2 : int) (sortie : 'b) : unit =
	match Hashtbl.find_opt (m.delta) (q1,lettre) with
	| None -> Hashtbl.add (m.delta) (q1,lettre) (q2,sortie)
	| Some l -> ()
;;

let affiche_transitions (m : ('a,'b) mealy) (print_lettre : 'a -> unit) (print_sortie : 'a -> unit) : unit =
	Hashtbl.iter 
		(fun x y -> let (q,a) = x and (q',b) = y in
			print_int q ;
		        print_string "," ;
			print_lettre a ;
			print_string " -> " ;
			print_int q' ;
			print_string " ; " ;
			print_sortie b ;
			print_newline ()
		) 
		(m.delta)
;;

let affiche_mealy (m : ('a,'b) mealy) (print_lettre : 'a -> unit) (print_sortie : 'a -> unit) : unit =
	print_string "I : ";
	print_int (m.i) ;
	print_newline ();
	affiche_transitions m print_lettre print_sortie 
;;

let affiche_mealy_char (m : ('a,'b) mealy) : unit = affiche_mealy m print_char print_char
;;

(* Exemple : machine donnée dans le sujet *)
let (m0 : (char,char) mealy) = {i = 0 ; delta = Hashtbl.create 10};;

let _ = ajouter_transition m0 0 '1' 1 '0';;
let _ = ajouter_transition m0 1 '1' 1 '1';;
let _ = ajouter_transition m0 2 '1' 1 '0';;
let _ = ajouter_transition m0 0 '0' 2 '0';;
let _ = ajouter_transition m0 1 '0' 2 '1';;
let _ = ajouter_transition m0 2 '0' 2 '0';;

(* Exemple : machine du "ou exclusif" *)
let (m1 : (char,char) mealy) = {i = 0 ; delta = Hashtbl.create 10};;

let _ = ajouter_transition m1 0 '0' 1 '0';;
let _ = ajouter_transition m1 0 '1' 2 '0';;
let _ = ajouter_transition m1 1 '0' 1 '0';;
let _ = ajouter_transition m1 1 '1' 2 '1';;
let _ = ajouter_transition m1 2 '1' 2 '0';;
let _ = ajouter_transition m1 2 '0' 2 '1';;

(* lit la lettre a dans l'état q, et donne l'état d'arrivée et sa sortie si la transition existe *)
let lire_lettre (m : ('a,'b) mealy) (q : int) (lettre : 'a) : (int * 'b) option =
	Hashtbl.find_opt (m.delta) (q,lettre)
;;
	
(* effectue la lecture du mot dans la machine donnée *)
let lire_mot (m : ('a,'b) mealy) (u : 'a mot) : ('b mot) option =
	let rec aux (q : int) (v : 'a mot) (acc : 'b mot) : ('b mot) option =
		match v with
		| [] -> Some(List.rev acc)
		| a::t -> match (lire_lettre m q a) with
			  | None -> None
			  | Some (q',b) -> aux q' t (b::acc)
	in aux (m.i) u []
;;		
	
