open Types 
open Graphics 

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
let c = show_generation alpha;;  
  
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
  
  
