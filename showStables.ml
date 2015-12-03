#use "projet.ml" ;;    


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
    
(*Methode qui change les variables en chiffre pour utiliser dans minisat*)
let varToInt var taille =
  if (String.length var = 3)
  then
    let i = int_of_string(String.sub var 1 1) and j = int_of_string(String.sub var 2 1) in ((i-1)*taille + j)
  else failwith "Erreur dans la formulation de la formule" ;;


(*Methode qui compte le nombre de clauses à partir d'une fnc*)
let nbClause (f:formule) =
  let rec aux f i = match f with
    |Var(s) -> i
    |Ou(g,d) -> aux g i + aux d 0
    |Et(g,d) -> aux g (i+1) + aux d 0
    |Neg(g) -> aux g i
    |Vrai | Faux -> i
  in aux f 1 ;;

let fa = Et(Et(Var("x3"),Ou(Var("x1"),Var("x2"))),Et(Ou(Var("x1"),Neg(Var("x3"))),Ou(Ou(Var("x3"),Var("x2")),Neg(Var("x1")))));;
nbClause f ;;

(*Methode qui fait une liste des clauses d'une formule fnc bien formé*)
let fncToListClause f =
  let rec aux f l = match f with
    |Ou(g,d) -> Ou(g,d)::l
    |Neg(s) -> Neg(s)::l
    |Var(s) -> Var(s)::l
    |Et(g,d) -> (aux g l )@(aux d [])
    |g -> g::l
  in aux f [] ;;
  
  fncToListClause fa ;;

(*Methode qui convertit une convertit une clause  en mode dimacs*)
  let convertFormule f taille   =
    let rec aux f s taille = match f with
      |Var(a) -> let t = varToInt a taille  in
		 if s = "" then string_of_int t  else s^" "^(string_of_int t)
      |Neg(g) -> begin match g with
		       |Var(a) -> let t = varToInt a taille  in
				  if s = "" then "-"^(string_of_int t) else s^" -"^(string_of_int t )
		       |_ -> failwith "Erreur dans les clauses"
		 end 
      |Ou(g,d) -> (aux g s taille)^" "^(aux d "" taille)
      |_ -> failwith "Erreur dans les clauses"
    in (aux f "" taille)^" 0" ;;

  let f = Ou(Var"x11",Neg(Var"x22"));;
  let g = convertFormule f  7 ;;


  (*Methode qui ecrit les clauses en mode dimacs sur un fichier à partir d'une liste de clauses*)
  let rec writeOnDimacs listclauses fichier taille  = match listclauses with
    |[] -> output_string fichier "\n";
	   close_out fichier;
    |a::q -> output_string fichier (convertFormule a taille);
	     output_string fichier "\n";
	     writeOnDimacs q fichier taille ;;
							



(* Methode qui crée le fichier .dimacs*)
  let create_dimacs formule taille  =
    let fichier = open_out "entrée.dimacs" in
    let nombredeclauses = nbClause formule in 
    output_string fichier "p cnf ";
    output_string fichier  ((string_of_int taille)^" ");
    output_string fichier  (string_of_int nombredeclauses );
    output_string fichier "\n";
    let listeclauses = fncToListClause formule in
    writeOnDimacs listeclauses fichier taille ;;    

    
 let fa = Et(Et(Var("x11"),Ou(Var("x21"),Var("x22"))),Et(Ou(Var("x13"),Neg(Var("x18"))),Ou(Ou(Var("x13"),Var("x22")),Neg(Var("x77")))));;

 (*Methode qui execute le fichier .dimacs // fichier est un string qui se termine par .dimacs*)
 let executeDimacs fichier   =
   if (Sys.file_exists fichier)
   then
     Sys.command ("minisat "^fichier^" sortie")
   else failwith "Le fichier dimacs n'est pas présent";;


 let str = "-1 4 5" ;;
 let c = String.sub str 0 1;;
   
 (*Methode qui fait une liste de state à partir d'un string *)
 let sortieToStateList string =
   let rec aux s l = match s with
     |" 0" -> List.rev l
     |_ -> let i = String.sub s 0 1 and j= String.sub s 1 (String.length s -1) in
	   if i=" "
	   then aux j l
	   else if i="-"
	   then let a =ref "" and b =ref ""  and c =ref 0 in
		while !b <> " " do
		  a:=!a^(String.sub j !c 1);
		  b:=String.sub j (!c+1) 1 ;
		  c:= !c + 1;
		done ;
		let k = String.sub j !c (String.length j-(!c)) in
		aux k (D::l)
	   else let a =ref "" and b =ref ""  and c =ref 0 in
		while !b <> " " do
		  a:=!a^(String.sub s !c 1);
		  b:=String.sub s (!c+1) 1 ;
		  c:= !c + 1;
		done ;
		let k = String.sub s !c (String.length s-(!c)) in
		aux k (A::l)
   in aux string [] ;;
   


let s = "1 -2 3 -4 0";; 
 let k = sortieToStateList s ;;



 (*Methode qui recupere sortie et fait une generation*)
 let sortieToGeneration sortie taille =
   let fichier = open_in sortie in
   let p = input_line fichier in
   if p="UNSAT"
   then failwith "La formule n'est pas satisfaisable"
   else
     let k = input_line fichier in
     let list = sortieToStateList k in
     close_in fichier ;
     list_to_Array list taille ;;
     
   
 let g = sortieToGeneration "sortie" 4 ;;
     
show_generation g ;;

let aut = [(D, A, D, A, D); (D, A, D, D, A); (D, A, A, A, D); (D, A, A, D, A);
	  (D, D, D, A, A); (D, D, D, D, D); (D, D, A, A, A); (D, D, A, D, D);
	  (A, A, D, A, D); (A, A, D, D, A); (A, A, A, A, A); (A, A, A, D, A);
	  (A, D, D, A, A); (A, D, D, D, D)];;
  
let c = stables aut 5 ;;  
 
 (* Methode ShowStables *)
let show_stables formule taille =
  create_dimacs formule taille;
  let k = executeDimacs "entrée.dimacs" in 
  sortieToGeneration "sortie" taille ;;


let gen = show_stables c 5 ;;
  show_generation(show_stables c 5);;
let alpha = show_generation(next_generation aut gen) ;;
