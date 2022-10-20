(*__________Partie I : préliminaires__________*)

type 'a mot = 'a list;;

type sigma = A | B;;

type 'a exp_reg =
	Vide
	| Epsilon
	| Symbole of 'a
	| Union of ('a exp_reg * 'a exp_reg)
	| Concat of ('a exp_reg * 'a exp_reg)
	| Etoile of ('a exp_reg)

;;
		
let e1 = Concat(Concat (Symbole 'a', Symbole 'b'), Etoile( Union(Symbole 'c', Symbole 'd') ) );;
let e2 = Union( Etoile( Concat( Symbole 'a', Etoile( Concat( Symbole 'a', Symbole 'b' ) )  ) ) , Etoile( Concat( Symbole 'b', Symbole 'a') ) )
;;
let rec affiche (e : char exp_reg) : unit =
	match e with
	| Vide -> print_string "O"
	| Epsilon -> print_string "E"
	| Symbole a -> print_char a
	| Union(e1,e2) -> affiche e1 ; print_string " | " ; affiche e2
	| Concat(e1,e2) -> print_string "(" ; affiche e1 ; print_string ")(" ; affiche e2 ; print_string ")"
	| Etoile(e1) -> print_string "(" ; affiche e1 ; print_string ")*"
;;
let rec affiche_mot (u : char mot) : unit =
	match u with
	| [] -> print_string "-"
	| a::t -> print_char 'a' ; affiche_mot t
;;
let rec accepte_vide (e : 'a exp_reg) : bool =	
	match e with
	| Vide -> false
	| Epsilon -> true
	| Symbole a -> false
	| Union(e1,e2) -> (accepte_vide e1) || (accepte_vide e2)
	| Concat(e1,e2) -> (accepte_vide e1) && (accepte_vide e2)
	| Etoile(e1) -> true
;;
let rec taille (e : 'a exp_reg) : int =
	match e with
	| Vide -> 1
	| Epsilon -> 1
	| Symbole a -> 1
	| Union(e1,e2) -> 1+(taille e1)+(taille e2)
	| Concat(e1,e2) -> 1+(taille e1)+(taille e2)
	| Etoile(e1) -> 1+(taille e1)
;;
(* Idée 1 : algorithme par backtracking *)
(* Idée 2 : algorithme par transformation d'expressions *)
(* Idée 3 : cas particuliers, langages k-testables *)
(* Idée 4 : langages quotients *)

(*__________Partie II : langages quotients__________*)

let rec derivee (e : 'a exp_reg) (lettre : 'a) : 'a exp_reg =
	match e with
	| Vide -> Vide
	| Epsilon -> Vide
	| Symbole a -> if lettre = a then Epsilon else Vide
	| Union(e1,e2) -> Union(derivee e1 lettre, derivee e2 lettre)
	| Concat(e1,e2) -> let cont = Concat (derivee e1 lettre, e2) in
			   if accepte_vide e1 then Union(cont, derivee e2 lettre)
			   else cont
	| Etoile e0 -> Concat(derivee e0 lettre, e)
;;
let u1 = ['b';'a';'b';'a';'b';'a'];;
let u2 = [];;

(* ceci permet de s'assurer de la complexité de derivee en regardant le pire cas de la taille de l'expression générée *)
(*
let rec pire_cas (h : int) : 'a exp_reg =
	if h = 0 then Epsilon
	else let e = pire_cas (h-1) in Concat(e,e)
	
for i = 0 to 10 do
	let e = pire_cas i in
	let e' = derivee e 'a' in
	let t = taille e in
	let t' = taille e' in
	print_string ((string_of_int t)^(" -> ")^(string_of_int t')^"\n")
done;;
*)

let rec simplifie (e : 'a exp_reg) : 'a exp_reg =
	match e with
	| Vide -> Vide
	| Epsilon -> Epsilon
	| Symbole a -> Symbole a
	| Union(e1,e2) -> let e1' = simplifie e1 and e2' = simplifie e2 in
			  (match e1',e2' with
			  | Vide,Vide -> Vide
			  | Vide,_ -> e2'
			  | _,Vide -> e1'
			  | Epsilon,_ -> if accepte_vide e2' then e2' else Union(Epsilon,e2')
			  | _,Epsilon -> if accepte_vide e1' then e1' else Union(e1',Epsilon)
			  | _,_ -> Union(e1',e2')
			  )
	| Concat(e1,e2) -> let e1' = simplifie e1 and e2' = simplifie e2 in
			  (match e1',e2' with
			  | Vide,_ -> Vide
			  | _,Vide -> Vide
			  | Epsilon,_ -> e2'
			  | _,Epsilon -> e1'
			  | _,_ -> Concat(e1',e2')
			  )
	| Etoile (Etoile (e0)) -> simplifie(Etoile e0)
	| Etoile e0 -> Etoile(simplifie e0)
;;
let rec accepte (e : 'a exp_reg) (u : 'a mot) : bool =
	match u with
	| [] -> accepte_vide e
	| a::t -> let e' = derivee e a in
		  accepte (simplifie e') t
;;
(*__________Partie III : préfixes et suffixes__________*)

let linearise (e : 'a exp_reg) : ('a * int) exp_reg =
	let rec aux (e : 'a exp_reg) (k : int) : (('a * int) exp_reg) * int =
		match e with
		| Vide -> (Vide,k)
		| Epsilon -> (Epsilon,k)
		| Symbole a -> (Symbole (a,k), (k+1))
		| Union(e1,e2) -> let (e1,l) = aux e1 k in
				  let (e2,m) = aux e2 l in
				  (Union(e1,e2),m)
		| Concat(e1,e2) -> let (e1,l) = aux e1 k in
				   let (e2,m) = aux e2 l in
				   (Concat(e1,e2),m)
		| Etoile(e1) -> let (e1,l) = aux e1 k in
				(Etoile(e1),l)
	in let (new_e,_) = aux e 0 in new_e
;;	
let premieres (e : 'a exp_reg) : 'a list =
	let rec aux (e : 'a exp_reg) (acc : 'a list) : 'a list =
		match e with
		| Vide -> acc
		| Epsilon -> acc
		| Symbole a -> a::acc
		| Union(e1,e2) -> let acc' = (aux e1 acc) in (aux e2 acc')
		| Concat(e1,e2) -> let acc' = (aux e1 acc) in 
				   if accepte_vide e1 then (aux e2 acc')
				   else acc'
		| Etoile(e1) -> (aux e1 acc)
	in aux e []
;;
let dernieres (e : 'a exp_reg) : 'a list =
	let rec aux (e : 'a exp_reg) (acc : 'a list) : 'a list =
		match e with
		| Vide -> acc
		| Epsilon -> acc
		| Symbole a -> a::acc
		| Union(e1,e2) -> let acc' = (aux e1 acc) in (aux e2 acc')
		| Concat(e1,e2) -> let acc' = (aux e2 acc) in 
				   if accepte_vide e2 then (aux e1 acc')
				   else acc'
		| Etoile(e1) -> (aux e1 acc)
	in aux e []
;;
let facteurs (e : 'a exp_reg) : ('a mot) list =
	let construit_facteurs (l1 : 'a list) (l2 : 'a list) : ('a mot) list =
		List.concat (List.map (fun a -> (List.map (fun x -> a::[x]) l2)) l1)
	in
	let rec aux (e : 'a exp_reg) (acc : ('a mot) list) : ('a mot) list =
		match e with
		| Vide -> acc
		| Epsilon -> acc
		| Symbole a -> acc
		| Union(e1,e2) -> let acc' = (aux e1 acc) in (aux e2 acc')
		| Concat(e1,e2) -> let d1 = dernieres e1 in
				   let p2 = premieres e2 in
				   let acc' = (aux e1 acc) in 
				   let acc'' = (aux e2 acc') in
				   List.append (construit_facteurs d1 p2) acc''
		| Etoile(e1) -> let d1 = dernieres e1 in
				let p1 = premieres e1 in
				let acc' = (aux e1 acc) in
				List.append (construit_facteurs d1 p1) acc'
	in aux e []
;;
(*__________Partie IV : Matrices d'expressions régulières, algorithme de Conway__________*)
type 'a mat_reg = ('a exp_reg) array array;;

let nulle (n : int) = Array.make_matrix n n Vide;;

let somme (a : 'a mat_reg) (b : 'a mat_reg) : 'a mat_reg =
	let n = Array.length a in
	let p = Array.length (a.(0)) in
	let c = Array.make_matrix n p Vide in
	for i = 0 to (n-1) do
		for j = 0 to (p-1) do
			c.(i).(j) <- Union(a.(i).(j),b.(i).(j))
		done
	done;
	c;;

let produit (a : 'a mat_reg) (b : 'a mat_reg) : 'a mat_reg =
	let n = Array.length a in
	let p = Array.length (a.(0)) in
	let q = Array.length (b.(0)) in
	let c = Array.make_matrix n p Vide in
	for i = 0 to (n-1) do
		for j = 0 to (q-1) do
			let e = ref (Concat(a.(i).(0),b.(0).(j))) in
			for k = 1 to (p - 1) do
				e := Union(!e,Concat(a.(i).(k),b.(k).(j)))
			done;
			c.(i).(j) <- !e
		done
	done;
	c;;
	
let decoupe (m : 'a mat_reg) (i : int) (j : int) : ('a mat_reg) * ('a mat_reg) * ('a mat_reg) * ('a mat_reg) =
	let n = Array.length m in
	let a = Array.make_matrix i j Vide in
	let b = Array.make_matrix i (n-j) Vide in
	let c = Array.make_matrix (n-i) j Vide in
	let d = Array.make_matrix (n-i) (n-j) Vide in
	for k = 0 to n-1 do
		for l = 0 to n-1 do
			if(k < i) && (l < j) then
				a.(k).(l) <- m.(k).(l)
			else if (k < i) && (l >= j) then
				b.(k).(l-j) <- m.(k).(l)
			else if (k >= i) && (l < j) then
				c.(k-i).(l) <- m.(k).(l)
			else
				d.(k-i).(l-j) <- m.(k).(l)
		done
	done;
	(a,b,c,d);;

let recolle (a : 'a mat_reg) (b : 'a mat_reg) (c : 'a mat_reg) (d : 'a mat_reg) : 'a mat_reg =
	let i = Array.length a in
	let j = Array.length a.(0) in
	let n = (Array.length a)+(Array.length c) in
	let m = Array.make_matrix n n Vide in
	for k = 0 to n-1 do
		for l = 0 to n-1 do
			if(k < i) && (l < j) then
				m.(k).(l) <- a.(k).(l)
			else if (k < i) && (l >= j) then
				m.(k).(l) <- b.(k).(l-j)
			else if (k >= i) && (l < j) then
				m.(k).(l) <- c.(k-i).(l)
			else
				m.(k).(l) <- d.(k-i).(l-j)
		done
	done;
	m;;

let rec etoile (m : 'a mat_reg) : 'a mat_reg =
	let n = Array.length m in
	if n = 1 then
		Array.make_matrix 1 1 (Etoile(m.(0).(0)))
	else
		let (a,b,c,d) = decoupe m (n/2) (n/2) in
		let a' = etoile (somme a (produit b (produit (etoile d) c))) in
		let b' = produit (produit (etoile a) b ) (etoile (somme d (produit c (produit (etoile a) b)) )) in
		let c' = produit (produit (etoile d) c ) (etoile (somme a (produit b (produit (etoile d) c)) )) in
		let d' = etoile (somme d (produit c (produit (etoile a) b))) in
		recolle a' b' c' d'
;;
