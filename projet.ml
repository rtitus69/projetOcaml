open Graphics

type state = D | A;;

type generation = state array array;;

type rule = state*state*state*state*state;;

type automaton = rule list;;

type formule = Vrai | Faux
	       |Var of string
	       |Neg of formule
	       |Et of formule * formule
	       |Ou of formule * formule;;

let char_to_state c = 
  if(c='A') then A 
  else if(c='D') then D 
  else failwith "Le caractere ne correspond a aucun etat";;

let string_to_state s :rule= 
  if(String.length s = 5) then 
    (char_to_state (String.get s 0), char_to_state(String.get s 1),  char_to_state (String.get s 2),  char_to_state (String.get s 3), char_to_state (String.get s 4))
  else failwith "La syntaxe d'une regle n'est pas correcte";;
      
let allregle fichier :automaton=
  let p =  try Some(input_line fichier) with
      End_of_file-> None in 
  match p with
  |None-> failwith "Le fichier n'a pas la bonne structure"
  |Some w -> if(w<>"Regles") then failwith "Le fichier n'a pas la bonne structure" 
    else 
      let rec aux fichier l = 
	let ligne = try Some(input_line fichier) with
	    End_of_file-> None in 
	match ligne with
	|None-> failwith "Le fichier n'a pas la bonne structure"
	|Some w -> if(w="GenerationZero") then l 
	  else if(String.length w = 5) then aux fichier ((string_to_state w)::l) 
	  else failwith "Le fichier n'a pas la bonne structure" in
      aux fichier [];;

(*reste a tester si il ya bien taille ligne dans le fichier
Le failwith fait-il sortir de la boucle for ?
-> Sinon il va falloir raise Erreur (qui permet de sortir du for) dans les cas ou on a mis failwith 
et try apres la fonction gene dans parse with le failwith
->On peut faire directement les changement dans let f i j quand on initialise dans parse la generation*)
let gene fichier taille init_gene = 
  for i = 0 to (taille-1) do
    let ligne = try Some(input_line fichier) with
	End_of_file-> None in 
    match ligne with
    |None-> failwith "Le fichier n'a pas la bonne structure"
    |Some w -> if((String.length w)<>taille) then failwith "Le fichier n'a pas la bonne structure" 
      else for j = 0 to (taille-1) do
	init_gene.(i).(j)<-(char_to_state(String.get w j))
	done
  done ;;
    
let parse fichier = 
  let fic = open_in fichier in
  let taille = 
    let p = try Some(input_line fic) with
    |End_of_file-> None in 
  match p with
  |None-> failwith "Le fichier n'a pas la bonne structure"
  |Some w -> int_of_string w in
  let automaton = allregle fic in
  let (generation:generation) = 
    let f i j = D in
    let t1 i = Array.init taille (f i) in 
    Array.init taille t1 
  in gene fic taille generation;
  (taille,automaton,generation);;

let a = parse "automate.txt";;

let stringState s = match s with
  |A -> "A"
  |D -> "D";;

let show_generation (g:generation):unit=
  print_newline();
  for i=0 to (Array.length g)-1 do
    print_string(" ");
    for i=0 to (Array.length g)-1 do
      print_string("+---");
    done;
    print_string("+");
    print_newline();
    print_string(" | ");
    for j = 0 to Array.length g.(i) -1 do
      print_string(stringState g.(i).(j)^" | ");
    done;
    print_newline();
  done;
  print_string(" ");
  for i=0 to (Array.length g)-1 do
      print_string("+---");
    done;
  print_string("+");;

let alpha:generation = [|[|D;D;A;D|];[|D;A;A;A|];[|D;D;A;D|];[|D;D;D;D|]|];;

open_graph(" 500x500");;

let show_generation2 (g:generation) :unit=
  set_window_title("Jeu De La Vie");
  let l = (size_x())/ (Array.length g) in
  for i=(Array.length g)-1 downto 0  do
    for j=0 to (Array.length g)-1 do
      if(g.(i).(j)=A)  then set_color blue
      else set_color white;
      fill_rect (i*l) (j*l) l l
    done;
  done;
  set_color black;
  let auxh n = 
    for i = 1 to n do 
      let space = ( size_y()) / n in
      let y = space*i in
      moveto 0 y ;
      lineto (size_x()) y
    done in
  auxh (Array.length g);
  let auxv n = 
    for i=1 to n do 
      let space = ( size_x()) / n in
      let x = space*i in
      moveto x 0 ;
      lineto x (size_y())
    done in
  auxv (Array.length g);;

  (*show_generation2 alpha;;*)

Array.length alpha ;;
show_generation alpha ;;  
(*show_generation c3;; *)

let v_nord g i j = match i,j with
  |0,j -> g.(Array.length g -1).(j)
  |i,j -> g.(i-1).(j);;
  
let v_sud g i j = match i,j with
  |i,j  when i = Array.length g -1 -> g.(0).(j)
  |i,j -> g.(i+1).(j);;


let v_est g i j = match i,j with
  |i,j when j=Array.length g - 1 -> g.(i).(0)
  |i,j -> g.(i).(j+1);;

let v_ouest g i j = match i,j with
  |i,0 -> g.(i).(Array.length g - 1)
  |i,j -> g.(i).(j-1);;



let verif_regle (g:generation) ((r1,r2,r3,r4,r5):rule) i j =
  let n = v_nord g i j and e =  v_est g i j and s =  v_sud g i j and o =  v_ouest g i j in
  if (r1=n && r2= e && r3 = s && r4 = o && r5 = g.(i).(j)) then true
  else false ;;
								 
let verif_aut (aut:automaton)  g i j =
  let rec aux aut = match aut with
    |[] -> D
    |a::q -> if (verif_regle g a i j) then A else aux q in aux aut;;   

  (*show_generation c3;;
    verif_aut c2 c3 1 2 ;;*)
let rec make_gen list gen n m = match n,m,list with
  |_,_,[] -> gen
  |_,m,a::q when m = Array.length gen -1 ->
    gen.(n).(m) <- a ;
    make_gen q gen (n+1) 0;
  |n,m,a::q -> gen.(n).(m) <- a ;
	       make_gen q gen n (m+1);;
  
  
  
let list_to_Array list taille  =
  let (generat:generation) = 
    let f i j = D in
    let t1 i = Array.init taille (f i) in 
    Array.init taille t1 
  in (make_gen list generat 0 0);;
  
  
let next_generation aut gen =
  let list = ref [] in 
  for i = 0 to Array.length gen -1 do
    for j = 0 to Array.length gen -1 do
      list:= (verif_aut aut gen i j )::!list
    done;
  done;
  (*list_to_Array (List.rev !list) (Array.length gen) ;;*)
  make_gen (List.rev(!list)) gen 0 0 ;;
    
let (c1,c2,c3) = a ;;

show_generation c3;;

let b = next_generation c2 c3 ;;
    
show_generation c3;;

show_generation b;;



let aut = [(D,D,D,D,A);(D,D,A,D,A);(A,D,D,D,A);(D,D,A,D,D);(D,A,D,D,D)];;

(*n=[] au debut*)
let rec d_vers_a aut n :rule list= match aut with 
  |[]->n
  |a::l->let (r1,r2,r3,r4,r5) = a in
	 if(r5=D) then d_vers_a l (a::n)
	 else  d_vers_a l n;;

d_vers_a aut [];;

let enleve l aut = 
  let rec aux l1 aut l2 = match l1 with
    |[] -> l2
    |a::q -> if (List.mem a aut) then aux q aut l2 else aux q aut (a::l2) in
  aux l aut [];;

(*l=[(AAAAA)] au debut*)
let all_regle_at =
  let rec all_regle_a i l=
    let rec all_regle_ab i l = 
      if(i<=4) then 
	match l with
	|[]->[]
	|a::q-> 
      begin 
	let (r1,r2,r3,r4,r5) = a in
	let t = [|r1;r2;r3;r4;r5|] in
	t.(i) <- D;
	let m = (t.(0),t.(1),t.(2),t.(3),t.(4)) in
	(all_regle_ab i q)@(m::a::[])
      end
      else l in
  if(i<=3) then
    all_regle_a (i+1) (all_regle_ab i l)
  else l in
  all_regle_a 0 [(A,A,A,A,A)];;

let recupere_regle_a aut =
  let rec all_regle_a i l=
    let rec all_regle_ab i l = 
      if(i<=4) then 
	match l with
	|[]->[]
	|a::q-> 
      begin 
	let (r1,r2,r3,r4,r5) = a in
	let t = [|r1;r2;r3;r4;r5|] in
	t.(i) <- D;
	let m = (t.(0),t.(1),t.(2),t.(3),t.(4)) in
	(all_regle_ab i q)@(m::a::[])
      end
      else l in
  if(i<=3) then
    all_regle_a (i+1) (all_regle_ab i l)
  else l in
  enleve (all_regle_a 0 [(A,A,A,A,A)]) aut;;

recupere_regle_a [];;
(*let g = Neg(Ou(Neg(Et(Var "a" , Var "b")),Neg(Et(Var"c",Var"d"))));;
descente_neg g ;;*)
let neg f = match f with
  |Neg(g) -> begin match g with
    |Vrai->Faux
    |Faux->Vrai
    |_->Neg(g)
  end
  |_->f;;

let rec descente_neg f = match f with
  |Neg(g) -> begin match g with
    |Neg(d) ->d
    |Ou(g',d') -> descente_neg (Et(Neg(g'),Neg(d')))
    |Et(g',d') -> descente_neg (Ou(Neg(g'), Neg(d'))) 
    |g'-> descente_neg (Neg(g'))
  end
  |Et(g,d) -> Et(descente_neg g, descente_neg d)
  |Ou(g,d) -> Ou(descente_neg g, descente_neg d)
  |Vrai -> Faux
  |Faux -> Vrai
  |_-> f;;

(*Traduit le passage de negation d'une regle plus la correspondance pour la variable donc A->D->neg xi si on a D->A->xi xi la var A ou D element de depart de la regle*)
let neg s var =
  if(s=A) then Neg(var) else var;;

(*applique transforme aux deux liste de regle qui change letat: d_vers_a et recupere regle_a
commence f a vrai*)
let transforme (list:rule list) (t1:formule) (t2:formule) (t3:formule) (t4:formule) (t5:formule) :formule= 
  let rec aux prem (list:rule list) (t1:formule) (t2:formule) (t3:formule) (t4:formule) (t5:formule) (f:formule) = match list with
    |[]->f
    |a::q->let (r1,r2,r3,r4,r5) =  a in
	   if(prem=true) then aux false q t1 t2 t3 t4 t5 ((Ou((neg r1 t1),Ou((neg r2 t2),Ou((neg r3 t3),Ou((neg r4 t4),(neg r5 t5)))))))
	   else aux false q t1 t2 t3 t4 t5 (Et(f,Ou((neg r1 t1),Ou((neg r2 t2),Ou((neg r3 t3),Ou((neg r4 t4),(neg r5 t5))))))); in
  aux true (list:rule list) (t1:formule) (t2:formule) (t3:formule) (t4:formule) (t5:formule) Vrai;;
 
transforme ( [(D, A, D, A, A); (D, A, D, D, A); (D, A, A, A, A); (D, A, A, D, A);  (D, D, D, A, A); (D, D, D, D, A); (D, D, A, A, A); (D, D, A, D, A);
	  (A, A, D, A, A); (A, A, D, D, A); (A, A, A, A, A); (A, A, A, D, A);
	  (A, D, D, A, A); (A, D, D, D, A)]) (Var"p")  (Var"q")  (Var"s")  (Var"d")  (Var"e") ;;

let simplifie_Vrai f =
    let rec aux f = match f with
      |Var s -> f
      |Neg s -> f
      |Ou(g,d) -> Ou( aux g, aux d)
      |Et(Vrai,d) -> aux d
      |Et(d,Vrai) -> aux d
      |Et(g,d) -> Et( aux g, aux d)
      |_-> f
    in aux f ;;

let stables (aut:automaton) t :formule=
  let tab_var k = 
    let f i j = Var ("x"^(string_of_int (i+1))^(string_of_int (j+1))) in
    let t1 i = Array.init k (f i) in 
    Array.init k t1 in
  let variable = tab_var t in
  let formule = ref Vrai in
  for i=0 to t-1 do
    for j=0 to t-1 do
      match !formule with
      |Vrai->formule := simplifie_Vrai (Et((transforme (d_vers_a aut []) (v_nord variable i j) (v_ouest variable i j) (v_sud variable i j) (v_est variable i j) (variable.(i).(j))), (transforme (recupere_regle_a aut) (v_nord variable i j) (v_ouest variable i j) (v_sud variable i j) (v_est variable i j) (variable.(i).(j)))))
      |_->formule := simplifie_Vrai(Et(!formule,Et( (transforme (d_vers_a aut []) (v_nord variable i j) (v_ouest variable i j) (v_sud variable i j) (v_est variable i j) (variable.(i).(j))), (transforme (recupere_regle_a aut) (v_nord variable i j) (v_ouest variable i j) (v_sud variable i j) (v_est variable i j) (variable.(i).(j))))))
    done;
  done; 
  !formule;;
  
let aut = [(D, A, D, A, D); (D, A, D, D, A); (D, A, A, A, D); (D, A, A, D, A);
	  (D, D, D, A, A); (D, D, D, D, D); (D, D, A, A, A); (D, D, A, D, D);
	  (A, A, D, A, D); (A, A, D, D, A); (A, A, A, A, A); (A, A, A, D, A);
	  (A, D, D, A, A); (A, D, D, D, D)];;

let c = stables aut 5;;

let beta = [|[|D; D; D; A; A|]; [|D; A; A; D; D|]; [|A; D; D; D; A|];
    [|D; D; A; A; D|]; [|A; A; D; D; D|]|];;
let rec interface aut gene_zero = 
  show_generation2 gene_zero;
  let etat = wait_next_event[Button_down] in
  clear_graph();
  interface aut (next_generation aut gene_zero);;

  interface aut beta;;
