type clause = int list
type cnf = clause list
type valuation = bool array
(* convention : les clauses contiennent des entiers non nuls *)
(* l'entier i représente la variable "x_i" si i > 0*)
(* l'entier -i représente le littéral "non x_i" si i > 0*)
(* dans une valuation, la valeur de x_i est donnée par la case i-1 *)

(* initialise la valuation par un tableau de false *)
let init_val (n : int) : valuation =
	Array.make n false

(* donne la valeur de vérité du littéral i *)
let valeur_litteral (v : valuation) (i : int) : bool =
	if(i > 0) then
		v.(i-1)
	else
		not v.((-i)-1)

(* donne la valeur b à la variable x_i *)
let choisir (v : valuation) (i : int) (b : bool) : unit =
	v.(i-1) <- b
;;

(*I CNF-SAT*)

let ex : cnf = [[1;2;-3];[1;-2;3];[-1;2;3]];;

let rec max_var_cnf (c : cnf) : int =
  let rec max_l k = 
    match k with
      | [] -> 0
      | h :: t -> max (abs h) (max_l t)
    in
  match c with 
    | [] -> 0
    | h :: t -> max (max_l h) (max_var_cnf t)
;;

max_var_cnf ex;;

let rec peut_sat_clause (c : clause) (v : valuation) (i : int) : bool =
  match c with
    | [] -> false
    | h :: tl -> valeur_litteral v h || peut_sat_clause tl v i 
;;

peut_sat_clause (List.hd ex) [|true; true; true|] 3;;

let rec peut_sat_cnf (c : cnf) (v : valuation) (i : int) : bool =
  match c with
    | [] -> true
    | h :: t -> peut_sat_clause h v i && peut_sat_cnf t v i
;;

peut_sat_cnf ex [|true; true; true|] 3;;

let rec sat_bt (c : cnf) (v : valuation) (i : int) : valuation option =
  if i = max_var_cnf c && (peut_sat_cnf c v i) then Some(v)
  else if i = max_var_cnf c && not(peut_sat_cnf c v i) then None
  else
  begin
    v.(i+1) <- true;
    if (peut_sat_cnf c v (i+1)) then (sat_bt c v (i+1))
    else 
      begin
      v.(i+1) <- false;
      sat_bt c v (i+1);
      end
  end
;;

let sat (c : cnf) : valuation option =
  sat_bt c (Array.make (max_var_cnf c) false) 0;; 
;;

(*II MAXSAT*)

let nb_clause_non_sat (c : cnf) (v : valuation) (i : int) : int =
  let acc = ref 0 in
  let rec eval k acc = 
    match k with
    | [] -> false
    | h :: tl -> if not (peut_sat_clause h v i) then acc := !acc +1 ;
                  eval tl acc 
    in
  eval c acc;
  !acc
;;

nb_clause_non_sat ex [|false; false; true|] 2;;

let cout (c : cnf) (v : valuation) : int =
  nb_clause_non_sat c v (max_var_cnf c)
;;

let eval (c : cnf) (v : valuation) (i: int)=
  nb_clause_non_sat c v i
;;

let cout_option (c : cnf) (v : valuation option) : int =
  match v with
    |Some(s) -> cout c s
    |None -> List.length c +1
;;

let maxsat_bb (c : cnf) (v : valuation) (i : int) (best_v : valuation option) : valuation option = 
  
;;










