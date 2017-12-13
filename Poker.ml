type couleur = Pique | Coeur | Carreau | Trefle ;;
type rang = Valeur of int;;
type carte = Carte of rang * couleur ;;
type donne = Main of carte*carte;;
type table = Flop of carte*carte*carte | Turn of carte*carte*carte*carte | River of carte*carte*carte*carte*carte;;
type comb = QuinteFlush of rang
	   |Carre of rang*rang(*suite de 4*)
	   |Full of rang*rang(*suite de 3*)
	   |Couleur of rang*rang*rang*rang*rang (*en cas d'égalité faut verifier la carte suivante*)
	   |Suite of rang
	   |Brelan of rang*rang*rang
	   |DoublePair of rang*rang*rang (*1er rang -> 1er paire, 2éme rang -> 2éme paire, 3éme -> derniére carte*)
	   |Pair of rang*rang*rang*rang(*1er rang -> 1er paire, le reste des rangs sont les cartes qu'on compare en cas d'égalité*)
	   |CarteHaute of rang*rang*rang*rang*rang(*Compare la meilleur rang et en cas d'égalité on regarde la prochaine plus forte et ainci de suite*);;
