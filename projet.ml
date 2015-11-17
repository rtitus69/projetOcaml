type state = D | A;;

type generation = state array array;;

type rule = state*state*state*state*state;;

type automaton = rule list;;

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

  Array.length alpha ;;
show_generation alpha ;;  


show_generation c3;; 
let v_nord g i j = match i,j with
  |0,j -> g.(Array.length g -1).(j)
  |i,j -> g.(i-1).(j);;
  
let v_sud g i j = match i,j with
  |i,j  when i = Array.length g -1 -> g.(0).(j)
  |i,j -> g.(i+1).(j);;


let v_est g i j = match i,j with
  |i,0 -> g.(i).(Array.length g -1)
  |i,j -> g.(i).(j+1);;

let v_ouest g i j = match i,j with
  |i,j when j=Array.length g -1  -> g.(i).(0)
  |i,j -> g.(i).(j-1);;



let verif_regle (g:generation) ((r1,r2,r3,r4,r5):rule) i j =
  let n = v_nord g i j and e =  v_est g i j and s =  v_sud g i j and o =  v_ouest g i j in
  if (r1=n && r2= e && r3 = s && r4 = o && r5 = g.(i).(j)) then true
  else false ;;
								 
let verif_aut (aut:automaton)  g i j =
  let rec aux aut = match aut with
    |[] -> D
    |a::q -> if (verif_regle g a i j) then A else aux q in aux aut;;   

  show_generation c3;;
    verif_aut c2 c3 1 2 ;;
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
   list_to_Array (List.rev !list) (Array.length gen) ;;

  
    

let (c1,c2,c3) = a ;;
  show_generation c3;;
  let b = next_generation c2 c3 ;;
    
    show_generation c3;;
      show_generation b;;
