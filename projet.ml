type state = Dead | Alive;;

type generation = state array array;;


type rule = state*state*state*state*state;;

type automaton = rule list;;

(*let rec lire fichier =
  try 
    let reglei = input_ligne i in 
    relgei::lire
  with End_of_file -> [];;*)


(*let rec donne_couple liste (regle,generation) = match liste with
  |a::q->if(a<>"GenerationZero")  then donne_couple q (a::regle,generation) else (regle,q)*)

let char_to_state c = 
  if(c='A') then Alive 
  else if(c='D') then Dead 
  else failwith "Le caractere ne correspond a aucun etat";;

let string_to_state s :rule= 
  if(String.length s = 5) then 
    (char_to_state (String.get s 0), char_to_state(String.get s 1),  char_to_state (String.get s 2),  char_to_state (String.get s 3), char_to_state (String.get s 4))
  else failwith "La syntaxe d'une regle n'est pas correcte";;
      
let allregle fichier :automaton=
  let rec aux fichier l = 
     let ligne = try Some(input_line fichier) with
	 End_of_file-> None in 
     match ligne with
     |None-> failwith "Le fichier n'a pas la bonne structure"
     |Some w -> if(w="GenerationZero") then l 
       else if(String.length w = 5) then aux fichier ((string_to_state w)::l) 
       else failwith "Le fichier n'a pas la bonne structure" in
  aux fichier [];;
let 
let parse fichier = 
  let fic = open_in fichier in
  let taille = try Some(input_line fic) with
      End_of_file-> None in 
  match taille with
  |None-> failwith "Le fichier n'a pas la bonne structure"
  |Some w -> int_to_string w in
let nom_Regle= try Some(input_line fic) with
    End_of_file-> None in match taille with
    |None-> failwith "Le fichier n'a pas la bonne structure"
    |Some w -> if(w<>"Regles") then failwith "Le fichier n'a pas la bonne structure" else allregle fichier in
let generation = 






    let all = lire i in
    match donne_couple all ([],[]) with
    |(l1,l2)-> let regle = l1 and generation = l2 in
