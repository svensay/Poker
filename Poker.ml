type couleur = Pique | Coeur | Carreau | Trefle ;;
type rang = Valeur of int;;
type carte = Carte of rang * couleur ;;
type donne = Main of carte*carte;;
type table = Flop of carte*carte*carte | Turn of carte*carte*carte*carte | River of carte*carte*carte*carte*carte;;
type comb = QuinteFlush of rang
	   |Carre of rang*rang(*suite de 4*)
	   |Full of rang*rang(*suite de 3*)
	   |Couleur of rang*rang*rang*rang*rang (*en cas d'�galit� faut verifier la carte suivante*)
	   |Suite of rang
	   |Brelan of rang*rang*rang
	   |DoublePair of rang*rang*rang (*1er rang -> 1er paire, 2�me rang -> 2�me paire, 3�me -> derni�re carte*)
	   |Pair of rang*rang*rang*rang(*1er rang -> 1er paire, le reste des rangs sont les cartes qu'on compare en cas d'�galit�*)
	   |CarteHaute of rang*rang*rang*rang*rang(*Compare la meilleur rang et en cas d'�galit� on regarde la prochaine plus forte et ainci de suite*);;
