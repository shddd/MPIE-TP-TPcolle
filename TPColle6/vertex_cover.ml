 type graph = (int * (int list)) list
(* liste de listes d'adjacence : le graphe contient la liste des (i, liste des voisins de i) *)


let cardinal_couverture (c : bool array) =
	let res = ref 0 in
	for i = 0 to (Array.length c) - 1 do
		if c.(i) then res := !res + 1
	done;
	!res

let print_couverture (c : bool array) : unit =
	for i = 0 to (Array.length c)-1 do
		if c.(i) then
			begin
			print_int i ;
			print_string " "
			end
	done;
	print_newline()

let est_couverture (g : graph) (c : bool array) =
	if List.length g <> Array.length c then
		failwith "Erreur de taille dans est_couverture"
	else
		let rec aux (i : int) (l : int list) : bool =
			match l with
			| [] -> true
			| j::t -> (c.(i) || c.(j)) && aux i t
		in
		let rec aux2 (l : (int * (int list)) list) : bool =
			match l with
			| [] -> true
			| (i,l')::t -> (aux i l') && (aux2 t)
		in
		aux2 g


				
let couverture_sommets (g : graph) (k : int) : bool =

	let rec aux (i : int) (k : int) (c : bool array) : bool =
		if i = Array.length c then
			let b = est_couverture g c in
			if b then print_couverture c ;
			b
		else
			begin
			c.(i) <- true ;
			if (cardinal_couverture c) <= k && (aux (i+1) k c) then
				true
			else
				begin
				c.(i) <- false;
				cardinal_couverture c <= k && (aux (i+1) k c) 
				end
			end
	in
	aux 0 k (Array.make (List.length g) false)


let g1 = [ (0,[1;2;3;4;5]) ; (1,[0;2]) ; (2,[0;1]) ; (3,[0]) ; (4,[0]) ; (5,[0]) ]
let g2 = [ (0,[1;2]) ; (1,[0;2;4]) ; (2,[0;1;3]) ; (3,[2;4]) ; (4,[1;3;5]) ; (5,[4]) ]


(* __________ Commencez ici votre code pour la partie Couvertures par Sommets __________ *)

type clause = int array
type cnf = clause array;;

let c1 = [|1;2;-3|]
let c2 = [|-1;-2;3|]
let c3 = [|1;-2;-3|]
let (f1:cnf) = [|c1;c2;c3|];;

let max_var_cnf (form:cnf) =
  let max= ref 0 in
  let n = Array.length form in
  for i=0 to n-1 do
    let k = Array.length form.(i) in
    for j=0 to k-1 do
      if form.(i).(j)> !max then
        max := form.(i).(j)
      done;
    done;
  !max;;

max_var_cnf f1;;
let m = 10;;

let sommets_var i b =
  if b then i else i+m;;
  
let l =10;;
let sommets_clauses m i p l =
  2*m+i+(p-1)*l;;
  
let ajout_arete (g:graph) (i:int) (j:int)=
  let rec aux (g:graph) (l:int) (m:int)=
    match g with
    |(k,li)::tl when k=l ->(l,m::li)::tl
    |hd::tl->hd::(aux tl l m)
    |_->g
    in
  let gp = aux g i j in
  aux gp j i;;
  
ajout_arete g1 1 3;;
  
let reduction (form:cnf) =
  let m = max_var_cnf form in
  let l = Array.length form in
  let (g:graph ref) = ref [] in
  for i=0 to 2*m+3*l-1 do
    g:=(i,[])::!g;
  done;
  for i = 0 to m-1 do
    g := ajout_arete (!g) i (i+m);
  done;
  for j = 0 to l-1 do
    g:=ajout_arete (!g) (sommets_clauses m j 1 l) (sommets_clauses m j 2 l);
    g:=ajout_arete (!g) (sommets_clauses m j 1 l) (sommets_clauses m j 3 l);
    g:=ajout_arete (!g) (sommets_clauses m j 3 l) (sommets_clauses m j 2 l);
  done;
  for j = 0 to l-1 do
    for p = 1 to 3 do
      let i = form.(j).(p-1) in
      if i>0 then
        g:= ajout_arete (!g) (i-1) (sommets_clauses m j p l)
      else
        g:= ajout_arete (!g) (m-i-1) (sommets_clauses m j p l)
      done;
    done;
  (!g, m+2*l);;
    
let solve (form:cnf) =
  let g,k = reduction form in
  couverture_sommets g k;;
  
solve f1;;
