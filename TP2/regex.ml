type 'a mot = 'a list

type 'a exp_reg =
	Vide
	| Epsilon
	| Symbole of 'a
	| Union of ('a exp_reg * 'a exp_reg)
	| Concat of ('a exp_reg * 'a exp_reg)
	| Etoile of ('a exp_reg)

type sigma = A | B

(* teste si u est vide *)
let est_vide (u : 'a mot) : bool = (List.length u = 0)

(* donne la longueur du mot u *)
let longueur (u : 'a mot) : int = (List.length u)

(* donne le miroir du mot u *)
let rec miroir (u : 'a mot) : 'a mot =
	(* construit le miroir du mot v et l'ajoute devant res *)
	let rec aux_miroir (v : 'a mot) (res : 'a mot) : 'a mot =
		match v with
		| [] -> res
		| a::s -> aux_miroir s (a::res)
	in aux_miroir u []
(* ou, en une ligne :
let rec miroir (u : 'a mot) : 'a mot = List.rev u
*)

(* teste si u est pr�fixe de v *)
let rec est_prefixe (u :'a mot) (v : 'a mot) : bool =
 	match (u,v) with
 	| ([],_) -> true
 	| (a::s,b::t) -> if a = b then est_prefixe s t else false
 	| _ -> false

(* teste si u est suffixe de v *)
let rec est_suffixe (u : 'a mot) (v : 'a mot) : bool =
	est_prefixe (miroir u) (miroir v)

(* teste si u est facteur de v *)
let rec est_facteur (u :'a mot) (v : 'a mot) : bool =
	match u with
	| [] -> true
	| __ -> match v with
		| [] -> false
		| _::t -> if est_prefixe u v then true else est_facteur u t

(* teste si u est un sous-mot de v *)
let rec est_sous_mot (u :'a mot) (v : 'a mot) : bool =
	match u with
	| [] -> true
	| a::s -> match v with
		| [] -> false
		| b::t -> if a = b then est_sous_mot s t else est_sous_mot u t
		
(* produit la concat�nation uv � partir de u et v *)
let concat (u : 'a mot) (v : 'a mot) : 'a mot =
	let rec aux_concat (u_mir : 'a mot) (v : 'a mot) : 'a mot =
		match u_mir with
		| [] -> v
		| a::t -> aux_concat t (a::v)
	in aux_concat (miroir u) v

(* renvoie le couple (v,w) o� u = vw tel que |v| = i *)
let coupe (u : 'a mot) (i : int) : ('a mot * 'a mot) =
	if i > longueur u || i < 0 then
		failwith "erreur dans coupe : i invalide"
	else
		begin
		(* ajoute les j premi�res lettres de u' au d�but de v' ; il faudra r�cup�rer le miroir de v' � la fin *)
		let rec aux_coupe (u' :'a mot) (v' : 'a mot) (j : int) : ('a mot * 'a mot) =
			if j = 0 then (miroir v',u')
			else
				begin
					match u' with
					| [] -> failwith "erreur dans aux_separe dans separe : ce cas ne devrait m�me pas arriver"
					| a::t -> aux_coupe t (a::v') (j-1)
				end
		in aux_coupe u [] i
		end

(* renvoie le pr�fixe de longueur i de u *)
let prefixe (u : 'a mot) (i : int) : ('a mot) =
	let (v,_) = coupe u i in v

(* renvoie le suffixe de longueur i de u *)
let suffixe (u : 'a mot) (i : int) : ('a mot) =
	let (_,w) = coupe u ((longueur u) - i) in w
	
(* teste si un mot u peut �tre engendr� par une expression e *)
let rec test (u : 'a mot) (e : 'a exp_reg) : bool =
	let n = longueur u in
	match e with
	| Vide -> false
	| Epsilon -> (n = 0)
	| Symbole(a) -> (u = [a])
	| Union(e1,e2) -> (test u e1) || (test u e2)
	| Concat(e1,e2) -> 
		let trouve = ref false and i = ref 0 in
		begin
		while (!i <= n) && (not !trouve) do
			let (v,w) = coupe u (!i) in
			if (test v e1) && (test w e2) then trouve := true
			else i := !i + 1
		done;
		!trouve
		end
	| Etoile(e1) -> 
		if n = 0 then true
		else let trouve = ref false and i = ref 0 in
		begin
		while (!i <= n) && (not !trouve) do
			let (v,w) = coupe u (!i) in
			if (test v e1) && (test w e) then trouve := true
			else i := !i + 1
		done;
		!trouve
		end

(*   e = ab* | ba*   *)
let e = Union( Concat( Symbole A , Etoile( Symbole B )) , Concat( Symbole B , Etoile( Symbole A )) )
let u1 = []
let u2 = [A]
let u3 = [B]
let u4 = [A;B]
let u5 = [A;B;B;B;B]
let u6 = [B;A]
let u7 = [B;A;A;A;A]
let u8 = [A;A]
let u9 = [B;B]
let u10 = [A;B;B;A;B]
let u11 = [B;A;A;A;B]

(* renvoie un mot de char correspondant � la string donn�e*)
let string_to_mot (s : string) : char mot =
	let n = String.length s in
	let rec aux (i : int) (res : char mot) : char mot =
		if i = 0 then res
		else aux (i-1) (s.[i-1]::res)
	in aux n []
