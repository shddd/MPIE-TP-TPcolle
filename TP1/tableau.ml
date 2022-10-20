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
