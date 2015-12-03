open Types

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
