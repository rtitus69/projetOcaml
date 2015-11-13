type state = Dead | Alive
type generation = state of int * int

let rec lire fichier =
  try 
    let reglei = input_ligne i in 
    relgei::lire
  with End_of_file -> [];;


let rec donne_couple liste (regle,generation) = match liste with
  |a::q->if(a<>"GenerationZero")  then donne_couple q (a::regle,generation) else (regle,q)

  
let parse fichier = 
  let i = open_in fichier in
  let taille= int_of_string(input_line i) in 
  let test = input_line i in
  if(test="Regles") then begin
    let all = lire i in
    match donne_couple all ([],[]) with
    |(l1,l2)-> let regle = l1 and generation = l2
