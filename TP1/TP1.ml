type image = int array array


let nouvelle_image (taille : int) : image =
  let img = Array.make_matrix taille taille 0 in
  Random.self_init ();
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      img.(i).(j) <- Random.int 2
    done
  done;
  img

let afficher_image_nb (img : image) : unit =
  let taille = Array.length img in
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      if img.(i).(j) = 0
      then Printf.printf ". "
      else Printf.printf "# "
    done;
    Printf.printf "\n"
  done

(* dans une image de taille n, associe un entier unique au couple (i,j) *)
let identifiant (n : int) (i : int) (j : int) : int =
	if (i < 0) || (i >= n) || (j < 0) || (j >= n) then
		failwith "erreur dans identifiant : case invalide"
	else
		(*(print_int (n*i+j) ; print_string "\n" ; n*i+j)*)
		n*i+j

(* pour une image donnée, renvoie la partition correpsondant aux composantes connexes *)
let composantes (img : image) : Tableau.unirtrouver =
  let taille = Array.length img in
  let partition = Tableau.init (taille * taille) in
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      (*print_string "HELP ! i = "; print_int i ; print_string " et j = " ; print_int j ; print_string "\n";*)
      if (i < taille - 1 && img.(i).(j) = img.(i + 1).(j))
      then Tableau.unir partition (identifiant taille i j) (identifiant taille (i+1) j);
      if (j < taille - 1 && img.(i).(j) = img.(i).(j + 1))
      then Tableau.unir partition (identifiant taille i j) (identifiant taille i (j+1))
    done
  done;
  partition
  

(* pour une image donnée, fournit une version colorée où chaque composante connexe est d'une couleur distincte *)
let recolore (img : image) : image =
  let taille = Array.length img in
  let partition = composantes img in
  (* on initialise une table de hachage, qui stocke les liens (composante,couleur) *)
  let color_association = Hashtbl.create taille in
  let color_counter = ref 0 in
  let colored_img = Array.make_matrix taille taille 0 in
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      let repr = Tableau.trouver partition (i * taille + j) in
      if not (Hashtbl.mem color_association repr)
      then (Hashtbl.add color_association repr !color_counter;
            incr color_counter);
      colored_img.(i).(j) <- Hashtbl.find color_association repr
    done
  done;
  colored_img

let color_to_char (color : int) : char =
  char_of_int ((int_of_char 'a') + color)

let afficher_image (img : image) : unit =
  let taille = Array.length img in
  for i = 0 to taille - 1 do
    for j = 0 to taille - 1 do
      Printf.printf "%c " (color_to_char img.(i).(j))
    done;
    Printf.printf "\n"
  done

let _ =
  let img = nouvelle_image 10 in
  afficher_image_nb img;
  let colored_img = recolore img in
  afficher_image colored_img
