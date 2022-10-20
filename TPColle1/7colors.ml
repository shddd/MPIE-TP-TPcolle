(*__________Section I : Structure unir-trouver__________*)

(* Ceci est la structure naïve en tableaux *)
(*
type unirtrouver = int array

let init (taille : int) : unirtrouver =
  Array.init taille (fun x -> x)

let trouver (partition : unirtrouver) (elt : int) : int =
  partition.(elt)

let unir (partition : unirtrouver) (elt : int) (elt' : int) : unit =
  (* On prend le représentant de la classe de elt comme nouveau
     représentant pour la classe fusionnée *)
  let old_repr = partition.(elt') in
  let replace_repr (curr_repr : int) =
    if curr_repr = old_repr
    then partition.(elt)
    else curr_repr
  in
  for i = 0 to (Array.length partition)-1 do
  	partition.(i) <- replace_repr (partition.(i))
  done;
  ()
*)

(* Ceci est la structure en arbre optimisée, avec l'union par rang et la compression de chemins *)
(* les arbres sont implémentés à l'aide du tableau parent *)
(* on dispose également du rang de chaque élément *)
type unirtrouver = {parent : int array ; rang : int array};;

let init (taille : int) : unirtrouver =
  {parent = Array.init taille (fun x -> x) ; rang = Array.make taille 0}
;;

let rec trouver (partition : unirtrouver) (elt : int) : int =
  if partition.parent.(elt) = elt then elt
  else
  	begin
  	(* compression de chemin : quand on a trouvé la racine, on la désigne comme parent *)
  	partition.parent.(elt) <- trouver partition (partition.parent.(elt)) ;
  	partition.parent.(elt)
  	end
;;

let unir (partition : unirtrouver) (elt : int) (elt' : int) : unit =
  (* on récupère les deux représentants et les deux rangs de ces représentants *)
  let repr1 = trouver partition elt in
  let repr2 = trouver partition elt' in
  let rang1 = partition.rang.(elt) in
  let rang2 = partition.rang.(elt') in
  (* si inégalité, le représentant de rang le plus haut devient parent de l'autre *)
  if rang1 > rang2 then
  	partition.parent.(repr2) <- repr1
  else if rang2 > rang1 then
  	partition.parent.(repr1) <- repr2
  (* si égalité des rangs, par convention, c'est la première classe qui absorbe la deuxième *)
  (* on met à jour le rang de sa racine*)
  else
  	begin
  	partition.parent.(repr2) <- repr1 ;
  	partition.rang.(repr1) <- partition.rang.(repr2) +1 
  	end
;;


(*__________Section II : Creation d'images__________*)

type image = {
	taille : int ; (* n, la longueur et la largeur de l'image *)
	plateau : unirtrouver ; (* n^2 cases réparties dans leurs classes *)
	couleurs : (int,int) Hashtbl.t (* on ne garde que la couleur des représentants des classes dans la table*)
}
;;
(* QUESTION à l'oral : complexité spatiale *)

(* dans une image de taille n, associe un entier unique au couple (i,j) *)
let identifiant (n : int) (i : int) (j : int) : int =
	if (i < 0) || (i >= n) || (j < 0) || (j >= n) then
		failwith "erreur dans identifiant : case invalide"
	else
		(*(print_int (n*i+j) ; print_string "\n" ; n*i+j)*)
		n*i+j
;;

(* crée une image de n cases par n cases*)
let nouvelle_image (n : int) : image =

	(* on initialise les champs *)
	let plat = init (n * n) in
	let col = Hashtbl.create (n * n) in
	Random.self_init ();
	
	(* cases initiales du joueur A et du joueur B *)
	Hashtbl.add col 0 (-1);
	Hashtbl.add col ((n*n)-1) (-2);
	
	(* on colorie aléatoirement les autres cases *)
 	for k = 1 to (n*n) - 2 do
 		Hashtbl.add col k (Random.int 7)
  	done;
	{taille = n ; plateau = plat ; couleurs = col}
;;
(* QUESTION A L'ORAL : c'est quoi, une table de hachage, et une fonction de hachage ? Comment ça marche ? Complexité de nouvelle_image *)

(* renvoie le couple (représentant,couleur) d'une case *)
let trouver_image (img : image) (i : int) (j : int) : int *int =
	let numero_case = identifiant img.taille i j in
	let repr_case = trouver img.plateau numero_case in
	(repr_case, Hashtbl.find img.couleurs repr_case)
;;
(* affiche une image à partir de ses couleurs*)
let afficher_image (img : image) : unit =
  let n = img.taille in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let (_,color) = trouver_image img i j in
      if color >= 0 then
      	print_int color
      else if color = -1 then
      	print_string "A"
      else if color= -2 then
        print_string "B"
    done;
    print_newline ();
  done
;;
  
(*__________Section III : Unification de composantes__________*)


(* unit les composantes de la case (i,j) et de la case (k,l) ; on suppose que les composantes sont de la même couleur *)
let unir_meme_couleur (img : image) (i : int) (j : int) (k : int) (l : int) : unit =
	let numero_case1 = identifiant img.taille i j in
	let numero_case2 = identifiant img.taille k l in
	(* on cherche les anciens représentants, pour savoir lequel éliminer de la table de hachage *)
	let (repr1,_) = trouver_image img i j in
	let (repr2,_) = trouver_image img k l in
	(* on réalise l'union *)
	unir img.plateau numero_case1 numero_case2 ;
	(* on élimine de la table le représentant qui n'a pas été conservé *)
	let new_repr = trouver img.plateau numero_case1 in
	if new_repr = repr1 then
		Hashtbl.remove img.couleurs repr2
	else
		Hashtbl.remove img.couleurs repr1
;;
(* QUESTION : complexité de cette fonction *)


(* réalise l'unification des composantes qui existent à la génération de l'image *)
let construire_composantes_connexes (img : image) : unit =
	let n = img.taille in
	(* on parcourt l'image et on y construit déjà les composantes connexes existences *)
	for i = 0 to n - 1 do
	  for j = 0 to n - 1 do
	    let (repr1,couleur1) = trouver_image img i j in
	    if (i < n-1) then
	    	begin
	   	let (repr2,couleur2) = trouver_image img (i+1) j in
	   	if (couleur1 = couleur2 && repr1 <> repr2) then 
	    		unir_meme_couleur img i j (i+1) j 
	    	end;
	    if (j < n-1) then
	    	begin
	   	let (repr3,couleur3) = trouver_image img i (j+1) in
	   	if (couleur1 = couleur3 && repr1 <> repr3) then 
	    		unir_meme_couleur img i j i (j+1) 
	    	end
	  done
	done
;;

(* unit les composantes de la case (i,j) et de la case (k,l) ; la case (i,j) possède la couleur d'un joueur, et la classe qui résulte de l'union doit conserver cette couleur *)
let unir_avec_joueur (img : image) (i : int) (j : int) (k : int) (l : int) : unit =
	let numero_case1 = identifiant img.taille i j in
	let numero_case2 = identifiant img.taille k l in
	(* on récupère les anciens représentants et la couleur *)
	let (repr1,joueur) = trouver_image img i j in
	let (repr2,_) = trouver_image img k l in

	(* si les deux cases étaient déjà dans la même classe, il n'y a rien à faire *)
	(* si les deux sont différentes, on le précise à travers ce test pour éviter de faire des mauvaises manipulations dans la table de hachage *)
	if repr1 <> repr2 then
		begin
		unir img.plateau numero_case1 numero_case2 ;
		(* on regarde quel représentant a été conservé *)
		let new_repr = trouver img.plateau numero_case1 in
		(* si c'est l'ancien représentant de la classe du joueur, on supprime l'autre *)
		if new_repr = repr1 then 
			Hashtbl.remove img.couleurs repr2
		(* si c'est l'autre, il faut supprimer le premier et mettre à jour la couleur du second *)
		else
			begin
			Hashtbl.remove img.couleurs repr1 ;
			Hashtbl.replace img.couleurs repr2 joueur
			end
		end
;;


(*__________Section IV : Fonctions du jeu__________*)

(* la liste des voisines de la case (i,j) de la couleur col *)
(* sa longueur est donc entre 1 et 4 *)
let voisines (img : image) (i : int) (j : int) (col : int) : (int*int) list =
	let n = img.taille in
	(* on regarde la voisine du haut *)
	let l0 = [] in
	let l1 = 
		if i = 0 then l0
		else
			let (_,couleur) = trouver_image img (i-1) j in
			if couleur = col then (i-1,j)::l0 else l0
	in
	let l2 = 
		if j = 0 then l1
		else
			let (_,couleur) = trouver_image img i (j-1) in
			if couleur = col then (i,j-1)::l1 else l1
	in
	let l3 = 
		if i = n-1 then l2
		else
			let (_,couleur) = trouver_image img (i+1) j in
			if couleur = col then (i+1,j)::l2 else l2
	in
	let l4 = 
		if j = n-1 then l3
		else
			let (_,couleur) = trouver_image img i (j+1) in
			if couleur = col then (i,j+1)::l3 else l3
	in l4
;;
			
(* on utilise l'algo suivant : si le joueur joue la couleur col, on parcourt toute l'image *)
(* si une case a pour voisine une case de couleur k, et est de la couleur col, on réalise l'union *)
(* rappel : joueur = -1 pour le joueur A, et -2 pour le joueur B *)
let coup (img : image) (joueur : int) (col : int) : unit =
	let n = img.taille in
	for k = 0 to n - 1 do
	  for l = 0 to n - 1 do
	    let (_,couleur) = trouver_image img k l in
	    if couleur = joueur then 
	    let vsn = voisines img k l col in 
	    List.iter (fun z -> let (x,y) = z in unir_avec_joueur img k l x y) vsn
	   (* on récupère une voisine de la case (k,l) de la couleur du joueur s'il en existe une *)
	   (* si elle existe, on fait l'union *)
	  done
	done
;;
(* idée de question à l'oral : est-ce qu'on pourrait pas faire mieux pour ne pas tester les voisines de toutes les cases ? *)
(* réponse : tout représenter par un graphe, où chaque sommet serait une composante connexe (donnée par l'ensemble de ses cases), et possèderait une couleur *)
(* faire l'union est alors facile, trouver un représentant est facile aussi *)
(* et on garde facilement l'information des voisins *)


(* calcul des scores *)
let score (img : image) : int*int =
	let n = img.taille in
	let s1 = ref 0 in
	let s2 = ref 0 in
	for k = 0 to n - 1 do
	  for l = 0 to n - 1 do
	    let (_,couleur) = trouver_image img k l in
	    if couleur = -1 then s1 := !s1 + 1
	    else if couleur = -2 then s2 := !s2+1
	  done
	done;
	(!s1,!s2)
;;
let afficher_scores (img : image) : unit =
	let (s1,s2) = score img in
	print_string "Scores :\n";
	print_string "Joueur A : ";
	print_int s1 ;
	print_string "\nJoueur B : ";
	print_int s2 ;
	print_string "\n"
;;
let fin_de_partie (img : image) : bool =
	let (s1,s2) = score img in
	let n = img.taille in
	(s1 > ((n*n)/2)) || (s2 > ((n*n)/2))
;;	
(* simule une partie entre deux joueurs sur un plateau de taille donnée *)
let jouer_partie (n : int) : unit =

	(* initialisation du jeu *)
	let img = nouvelle_image n in
	construire_composantes_connexes img;

	(* variable qui repère le joueur dont c'est le tour *)
	let turn = ref (-1) in
	
	(* boucle de jeu *)
	while (not (fin_de_partie img)) do
	
		(* afficher plateau *)
		print_string "Voici le plateau :\n";
		afficher_image img;
		print_string "\n";

		(* afficher scores *)
		afficher_scores img ;

		(* afficher instruction du tour *)
		let s = 
			if !turn = -1 then "Au tour du joueur A. Que jouez-vous ?\n"
			else "Au tour du joueur B. Que jouez-vous ?\n"
		in
		print_string s;
		
		(* lire la couleur et jouer le coup *)
		let couleur = int_of_string (read_line ()) in
		coup img !turn couleur ;
		
		(* tour suivant *)
		turn := if !turn = -1 then -2 else -1 ;

		print_newline ();
		print_newline ();
		print_newline ()
	done ;
	afficher_scores img ;
	let (s1,s2) = score img in
	if s1 > s2 then
		print_string "Le joueur A gagne.\n"
	else if s1 < s2 then
		print_string "Le joueur B gagne.\n"
	else
		print_string "Egalité.\n"
;;
