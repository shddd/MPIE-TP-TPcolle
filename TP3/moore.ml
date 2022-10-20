type 'a mot = 'a list

type ('a,'b) moore = {
	out : 'b array ; (* on a (Array.length out) états, et pour tout i, out.(i) est la sortie de l'état i *)
	i : int ;
	delta : (int * 'a, int) Hashtbl.t
}

let ajouter_transition (m : ('a,'b) moore) (q1 : int) (lettre : 'a) (q2 : int) : unit =
	let nb = Array.length (m.out) in
	assert((q1 >= 0) && (q1 < nb));
	assert((q2 >= 0) && (q2 < nb));
	match Hashtbl.find_opt (m.delta) (q1,lettre) with
	| None -> Hashtbl.add (m.delta) (q1,lettre) q2
	| Some l -> ()

let affiche_transitions (m : ('a,'b) moore) (print_lettre : 'a -> unit) (print_sortie : 'a -> unit) : unit =
	Hashtbl.iter 
		(fun x y -> let (q,a) = x in
			print_int q ;
		        print_string "," ;
			print_lettre a ;
			print_string " -> " ;
			print_int y ;
			print_string " ; " ;
			print_sortie (m.out.(y)) ;
			print_newline ()
		) 
		(m.delta)

let affiche_moore (m : ('a,'b) moore) (print_lettre : 'a -> unit) (print_sortie : 'a -> unit) : unit =
	print_string "I : ";
	print_int (m.i) ;
	print_newline ();
	affiche_transitions m print_lettre print_sortie 

let affiche_moore_char (m : ('a,'b) moore) : unit = affiche_moore m print_char print_char


(* Exemple : machine donnée dans le sujet *)
let (m0 : (char,char) moore) = { out = [|'b';'a';'a';'c'|] ; i = 0 ; delta = Hashtbl.create 10}

let _ = ajouter_transition m0 0 'z' 1
let _ = ajouter_transition m0 0 'x' 3
let _ = ajouter_transition m0 0 'y' 3
let _ = ajouter_transition m0 1 'y' 2
let _ = ajouter_transition m0 1 'x' 3
let _ = ajouter_transition m0 1 'z' 3
let _ = ajouter_transition m0 2 'y' 3
let _ = ajouter_transition m0 3 'z' 2
let _ = ajouter_transition m0 3 'x' 3


(* Exemple : machine question 5 *)
let (m1 : (char,char) moore) = { out = [|'a';'b'|] ; i = 0 ; delta = Hashtbl.create 5}

let _ = ajouter_transition m1 0 'b' 0
let _ = ajouter_transition m1 0 'a' 1
let _ = ajouter_transition m1 1 'a' 1
let _ = ajouter_transition m1 1 'b' 0

(* Exemple : machine question 6 *)
let (m2 : (char,char) moore) = { out = [|'y';'b';'x';'b'|] ; i = 0 ; delta = Hashtbl.create 10}

let _ = ajouter_transition m2 0 'a' 2
let _ = ajouter_transition m2 0 'b' 1
let _ = ajouter_transition m2 1 'b' 1
let _ = ajouter_transition m2 1 'a' 2
let _ = ajouter_transition m2 2 'a' 0
let _ = ajouter_transition m2 2 'b' 3
let _ = ajouter_transition m2 3 'b' 3
let _ = ajouter_transition m2 3 'a' 0

(* lit la lettre a dans l'état q, et donne l'état d'arrivée et sa sortie si la transition existe *)
let lire_lettre (m : ('a,'b) moore) (q : int) (lettre : 'a) : (int * 'b) option =
	match Hashtbl.find_opt (m.delta) (q,lettre) with
	| None -> None
	| Some (q') -> Some (q',m.out.(q'))
	
(* effectue la lecture du mot dans la machine donnée *)
let lire_mot (m : ('a,'b) moore) (u : 'a mot) : ('b mot) option =
	let rec aux (q : int) (v : 'a mot) (acc : 'b mot) : ('b mot) option =
		match v with
		| [] -> Some(List.rev acc)
		| a::t -> match (lire_lettre m q a) with
			  | None -> None
			  | Some (q',b) -> aux q' t (b::acc)
	in aux (m.i) u []
		
	

