open Regex

(* construit une expression r�guli�re repr�sentant en fait l'alphabet entier *)
let rec construire_alphabet (current : char) (res : char exp_reg) =
	if current = 'Z' then
		construire_alphabet 'a' (Union(res,Symbole('Z')))
	else
		if current = 'z' then
			(Union(res,Symbole('z')))
		else
			let next = Char.chr (1 + (Char.code current)) in
			construire_alphabet next (Union(res,Symbole(current)))

let alphabet = construire_alphabet 'A' Vide		

let list_of_dinosaurs = ["Velociraptor";
    "Cryolophosaurus";
    "Dilophosaurus";
    "Spinosaurus";
    "Suchomimus";
    "Baryonyx";
    "Irritator";
    "Utahraptor";
    "Sinotyrannus";
    "Chilantaisaurus";
    "Shaochilong";
    "Dilong";
    "Allosaurus";
    "Ceratosaurus";
    "Torvosaurus";
    "Sinraptor";
    "Acrocanthosaurus";
    "Tyrannotitan";
    "Carcharodontosaurus";
    "Mapusaurus";
    "Giganotosaurus";
    "Megaraptor";
    "Skorpiovenator";
    "Neovenator";
    "Afrovenator";
    "Carnotaurus";
    "Abelisaurus";
    "Coelophysis";
    "Megalosaurus";
    "Dromaeosaurus";
    "Achillobator";
    "Eocarcharia";
    "Kryptops";
    "Rugops";
    "Rajasaurus";
    "Spinosaurus";
    "Tyrannosaurus";
    "Tarbosaurus";
    "Daspletosaurus";
    "Gorgosaurus";
    "Albertosaurus";
    "Alioramus";
    "Appalachiosaurus";
    "Zhuchengtyrannus";
    "Dryptosaurus";
    "Alectrosaurus"
]

let print_result (string_list : string list) (e : char exp_reg) : unit =
	let for_one_word (x : string) : unit =
		(* print_string ("testing "^x^"\n"); *)
		let new_x = string_to_mot x in
		if test new_x e then (print_string x ; print_string "\n")
	in
	List.iter for_one_word string_list
	
(* Q18 *)
let e1 = Concat(Etoile(alphabet),
		Concat(Symbole 'r',
			Concat(Symbole 'a',
				Concat(Symbole 'p',
					Concat(Symbole 't',
						Concat(Symbole 'o',Symbole 'r')
		)))))	
			

(* Q19 *)
let e2 = Concat(Symbole 's',
		Concat(Symbole 'a',
			Concat(Symbole 'u',
				Concat(Symbole 'r',
					Concat(Symbole 'u', Symbole 's')
	))))
	
let e3 = Concat(Symbole 'A',
		Concat(Etoile(alphabet),e2))

(* Q20 *)
let e4 = Concat(Symbole 'M',
		Concat(Symbole 'e',
			Concat(Symbole 'g', Symbole 'a')))
let e5 = Union(Concat(e4,Etoile(alphabet)),Concat(Etoile(alphabet),e2))

let _ =
  print_result list_of_dinosaurs e1; print_string "\n\n";
  print_result list_of_dinosaurs e3; print_string "\n\n";
  print_result list_of_dinosaurs e5
