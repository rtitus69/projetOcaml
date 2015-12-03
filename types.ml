type state = D | A;;

type generation = state array array;;

type rule = state*state*state*state*state;;

type automaton = rule list;;

  
type formule = Vrai | Faux
	       |Var of string
	       |Neg of formule
	       |Et of formule * formule
	       |Ou of formule * formule;;
