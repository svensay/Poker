type couleur = Pique | Coeur | Carreau | Trefle ;;
type rang = Valeur of int;;
type carte = Carte of rang * couleur ;;
type donne = Main of carte*carte;;
type table = Flop of carte*carte*carte | Turn of carte*carte*carte*carte | River of carte*carte*carte*carte*carte;;
type comb = QuinteFlush of rang
	   | Carre of rang*rang(*suite de 4*)
	   | Full of rang*rang(*suite de 3*)
	   | Couleur of rang*rang*rang*rang*rang (*en cas d'égalité faut verifier la carte suivante*)
	   | Suite of rang
	   | Brelan of rang*rang*rang
	   | DoublePaire of rang*rang*rang (*1er rang -> 1er paire, 2éme rang -> 2éme paire, 3éme -> derniére carte*)
	   | Paire of rang*rang*rang*rang(*1er rang -> 1er paire, le reste des rangs sont les cartes qu'on compare en cas d'égalité*)
	   | CarteHaute of rang*rang*rang*rang*rang(*Compare la meilleur rang et en cas d'égalité on regarde la prochaine plus forte et ainci de suite*);;

let compare_hands d1 d2 t = ;;

let compare_comb c1 c2 = match c1 with
  | QuinteFlush r -> print_endline("QuinteFlush")
  | Carre (r1,r2) -> print_endline("Carre")
  | Full  (r1,r2) ->  print_endline("Full")
  | Couleur (r1,r2,r3,r4,r5) -> print_endline("Couleur")
  | Suite r -> print_endline("Suite")
  | Brelan (r1,r2,r3) ->  print_endline("Brelan")
  | DoublePaire (r1,r2,r3) ->  print_endline("DoublePaire")
  | Paire (r1,r2,r3,r4) -> print_endline("Paire")
  | CarteHaute (r1,r2,r3,r4,r5) -> print_endline("CarteHaute")
;;

let compute_comb d t = ;;
  
let test1 = QuinteFlush(Valeur(10));;
let test2 =  QuinteFlush(Valeur(9));;

compare_comb test1 test2;;  
             
