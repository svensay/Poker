type couleur = Pique | Coeur | Carreau | Trefle ;;
type valeur = Point of int | Valet | Dame | Roi ;;
type carte = Carte of valeur * couleur ;;
type donne = Main of carte*carte;;
type table = Flop of carte*carte*carte | Turn of carte*carte*carte*carte | River of carte*carte*carte*carte*carte;;
type comb = QuinteFlush of valeur
	   |Carre of valeur(*suite de 4*)
	   |Full of valeur(*suite de 3*)
	   |Couleur of valeur*valeur*valeur*valeur*valeur (*en cas d'�galit� faut verifier la carte suivante*)
	   |Suite of valeur
	   |Brelan of valeur
	   |DoublePair of valeur*valeur*valeur (*1er valeur -> 1er paire, 2�me valeur -> 2�me paire, 3�me -> derni�re carte*)
	   |Pair of valeur*valeur*valeur*valeur(*1er valeur -> 1er paire, le reste des valeurs sont les cartes qu'on compare en cas d'�galit�*)
	   |CarteHaute of valeur*valeur*valeur*valeur*valeur(*Compare la meilleur valeur et en cas d'�galit� on regarde la prochaine plus forte et ainci de suite*);;
