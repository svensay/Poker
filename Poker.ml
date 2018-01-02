type couleur = Pique | Coeur | Carreau | Trefle ;;
type rang = Valeur of int;;
type carte = Carte of rang * couleur ;;
type donne = Main of carte*carte;;
type table = Flop of carte*carte*carte | Turn of carte*carte*carte*carte | River of carte*carte*carte*carte*carte;;
type comb = QuinteFlush of rang
      | Carre of rang*rang(*suite de 4*)
      | Full of rang*rang(*1er rang la valeur du brelan et 2eme celle de la pair*)
      | Couleur of rang*rang*rang*rang*rang (*en cas d'égalité faut verifier la carte suivante*)
      | Suite of rang
      | Brelan of rang*rang*rang(*suite de 3*)
      | DoublePaire of rang*rang*rang (*1er rang -> 1er 
paire, 2éme rang -> 2éme paire, 3éme -> derniére carte*)
      | Paire of rang*rang*rang*rang(*1er rang -> 1er paire, le reste des rangs sont les cartes qu'on compare en cas d'égalité*)
      | CarteHaute of rang*rang*rang*rang*rang(*Compare la meilleur rang et en cas d'égalité on regarde la prochaine plus forte et ainci de suite*);;
;;

(* Compare deux combinaisons "du même type" *)
let compare_comb_equals c1 c2 = match c1, c2 with
  | QuinteFlush r, QuinteFlush r2 | Suite r, Suite r2 -> 
                  if r > r2 then 1
                  else if r < r2 then -1
                  else 0
  | Carre (r1,r2), Carre (r3,r4) | Full (r1,r2), Full (r3,r4) -> 
                  if r1 > r3 then 1
                  else if r1 < r3 then -1
                  else if r2 > r4 then 1
                  else if r2 < r4 then -1
                  else 0
  | Brelan (r1,r2,r3), Brelan (r4,r5,r6) | DoublePaire (r1,r2,r3), DoublePaire (r4,r5,r6) -> 
                  if r1 > r4 then 1
                  else if r1 < r4 then -1
                  else if r2 > r5 then 1
                  else if r2 < r5 then -1
                  else if r3 > r6 then 1
                  else if r3 < r6 then -1
                  else 0
  | Paire (r1,r2,r3,r4), Paire (r5,r6,r7,r8) -> 
                  if r1 > r5 then 1
                  else if r1 < r5 then -1
                  else if r2 > r6 then 1
                  else if r2 < r6 then -1
                  else if r3 > r7 then 1
                  else if r3 < r7 then -1
                  else if r4 > r8 then 1
                  else if r4 < r8 then -1
                  else 0
  | Couleur (r1,r2,r3,r4,r5), Couleur (r6,r7,r8,r9,r10) | CarteHaute (r1,r2,r3,r4,r5), CarteHaute (r6,r7,r8,r9,r10) -> 
                  if r1 > r6 then 1
                  else if r1 < r6 then -1
                  else if r2 > r7 then 1
                  else if r2 < r7 then -1
                  else if r3 > r8 then 1
                  else if r3 < r8 then -1
                  else if r4 > r9 then 1
                  else if r4 < r9 then -1
                  else if r5 > r10 then 1
                  else if r5 < r10 then -1
                  else 0
  | _ , _  -> failwith("Pas possible")
;;

(* let valeur_comb c = match c with
  | QuinteFlush r -> print_endline("QuinteFlush"); 9
  | Carre (r1,r2) -> print_endline("Carre"); 8
  | Full  (r1,r2) ->  print_endline("Full"); 7
  | Couleur (r1,r2,r3,r4,r5) -> print_endline("Couleur"); 6
  | Suite r -> print_endline("Suite"); 5
  | Brelan (r1,r2,r3) ->  print_endline("Brelan"); 4
  | DoublePaire (r1,r2,r3) ->  print_endline("DoublePaire"); 3
  | Paire (r1,r2,r3,r4) -> print_endline("Paire"); 2
  | CarteHaute (r1,r2,r3,r4,r5) -> print_endline("CarteHaute"); 1
;; *)

let valeur_comb c = match c with
  | QuinteFlush r -> 9
  | Carre (r1,r2) -> 8
  | Full  (r1,r2) ->  7
  | Couleur (r1,r2,r3,r4,r5) -> 6
  | Suite r -> 5
  | Brelan (r1,r2,r3) ->  4
  | DoublePaire (r1,r2,r3) ->  3
  | Paire (r1,r2,r3,r4) -> 2
  | CarteHaute (r1,r2,r3,r4,r5) -> 1
;;



let print_rang (r:rang) = match r with
  Valeur i -> print_newline (); print_int(i); print_newline ();
;;

let print_comb c = 
  print_endline("DEBUT PRINT COMB");
  match c with
  | QuinteFlush r -> print_endline("QuinteFlush"); print_rang r;
  | Carre (r1,r2) -> print_endline("Carre"); print_rang r1; print_rang r2;
  | Full  (r1,r2) ->  print_endline("Full"); print_rang r1; print_rang r2;
  | Couleur (r1,r2,r3,r4,r5) -> print_endline("Couleur"); print_rang r1; print_rang r2; print_rang r3; print_rang r4; print_rang r5;
  | Suite r -> print_endline("Suite"); print_rang r;
  | Brelan (r1,r2,r3) -> print_endline("Brelan");  print_rang r1; print_rang r2; print_rang r3;
  | DoublePaire (r1,r2,r3) ->  print_endline("DoublePaire"); print_rang r1; print_rang r2; print_rang r3;
  | Paire (r1,r2,r3,r4) -> print_endline("Paire"); print_rang r1; print_rang r2; print_rang r3; print_rang r4;
  | CarteHaute (r1,r2,r3,r4,r5) -> print_endline("CarteHaute"); print_rang r1; print_rang r2; print_rang r3; print_rang r4; print_rang r5;
  print_endline("FIN PRINT COMB");
;;

let compare_comb c1 c2 =
(*         print_endline("c1");
        print_comb c1; 
        print_endline("c2");
        print_comb c2;
 *)        
        if valeur_comb c1 > valeur_comb c2 then 1
        else if valeur_comb c2 > valeur_comb c1 then -1
        else compare_comb_equals c1 c2
;;

(*Donne la valeur du rang rank*)
let rankToValue rank =
  match rank with Valeur value -> value
;;

(*Renvoie la liste de carte de la donne d et de la table t*)
let list_card d t =
        let l = [] in
        match d with
        | Main (c1,c2) -> match t with
          | Flop (c3,c4,c5) -> (c1::(c2::(c3::(c4::(c5::l)))))
          | Turn (c3,c4,c5,c6) -> (c1::(c2::(c3::(c4::(c5::(c6::l))))))
          | River (c3,c4,c5,c6,c7) -> (c1::(c2::(c3::(c4::(c5::(c6::(c7::l)))))))
;;

(*retourne la liste des rank*)
(*par ordre decroissant*)
let list_rank tab_rang =
  let rec aux i l =
    if i > 12 then l
    else match tab_rang.(i) with
      | 0 -> aux (i+1) l
      | 1 -> aux (i+1) (Valeur(i+2)::l)
      | 2 -> aux (i+1) (Valeur(i+2)::(Valeur(i+2)::l))
      | 3 -> aux (i+1) (Valeur(i+2)::(Valeur(i+2)::(Valeur(i+2)::l)))
      | 4 -> aux (i+1) (Valeur(i+2)::(Valeur(i+2)::(Valeur(i+2)::(Valeur(i+2)::l))))
      |_ -> failwith("Mauvaise utilistation de la fonction list_rank")
  in aux 0 []
;;

(*Ajoute la combinaison Carre dans l*)
(*i valeur du Carre(Ex:i=4, Carre de 4)*)
(*list_rank : liste des rangs de la table et de la donne(voir compute_comp*)
let rec carreAdd list_rank i l =
  let list_rank_sans_i = (List.filter (fun x -> i != rankToValue x) list_rank)
  in match list_rank_sans_i with
    |h::t -> Carre(Valeur(i),h)::l
    |[] -> failwith("Mauvaise utilistation de la fonction carreAdd")
;;

(*Ajoute la combinaison Brelan dans l*)
(*i valeur du Brelan(Ex:i=10, Brelan de 10)*)
(*list_rank : liste des rangs de la table et de la donne(voir compute_comp*)
let rec brelanAdd list_rank i l =
  let list_rank_sans_i = (List.filter (fun x -> i != rankToValue x) list_rank)
  in match list_rank_sans_i with
    |h1::h2::t -> Brelan(Valeur(i),h1,h2)::l
    |[]|_::[] -> failwith("Mauvaise utilistation de la fonction brelanAdd")
;;

(*Ajoute la combinaison Paire dans l*)
(*i valeur de la Pair(Ex:i=5, Paire de 5)*)
(*list_rank : liste des rangs de la table et de la donne(voir compute_comp*)
let rec pairAdd list_rank i l =
  let list_rank_sans_i = (List.filter (fun x -> i != rankToValue x) list_rank)
  in match list_rank_sans_i with
    |h1::h2::h3::_ -> Paire(Valeur(i),h1,h2,h3)::l
    |[] |_::[] |_::_::[] -> failwith("Mauvaise utilistation de la fonction pairAdd")
;;

(*Créer une combinaison Couleur avec les valeurs de list_i(Voir colorAdd)*)
let creatColor list_i =
  match list_i with
    |h1::h2::h3::h4::h5::_ -> Couleur(Valeur(h5),Valeur(h4),Valeur(h3),Valeur(h2),Valeur(h1))
    |[]|_::[]|_::_::[]|_::_::_::[]|_::_::_::_::[] -> failwith("Mauvaise utilisation de la mehtode creatColor")
;;

(*Renvoie si suite de couleur(variable : suite) : QuinteFlush à l*)
(*Si 5 couleur non succecive (variable : color) : Couleur à l*)
(*list_color : tableau de boolean qui est true si a l'indide i, la carte de valeur i+1 est de la couleur de list_color(Voir compute_comp) *)
(*list_i : accumule les i true*)
let rec colorOrQuinteFlushAdd list_color color suite list_i i l =
  if i < 0 then match list_color.(12) with
    |true when (suite+1) = 5 -> QuinteFlush(Valeur(i+6))::l
    |_ -> l  
  else match list_color.(i) with
    |true when suite+1 = 5 -> colorOrQuinteFlushAdd list_color (color+1) 0 ((i+2)::list_i) (i-1) (QuinteFlush(Valeur(i+6))::l)
    |true when color+1 = 5 -> colorOrQuinteFlushAdd list_color 0 (suite+1) ((i+2)::list_i) (i-1) ((creatColor ((i+2)::list_i))::l)
    |true -> colorOrQuinteFlushAdd list_color (color+1) (suite+1) ((i+2)::list_i) (i-1) l
    |false -> colorOrQuinteFlushAdd list_color color 0 list_i (i-1) l
;;


(*Ajoute une DoublePaire a l avec le plus grand rang entre rang1 et rang2 en 1er et le plus grand rank dans list_rank hors rang1 et rang2(Voir findPaire)*)
let rec doublePairAdd list_rank rang1 rang2 l =
  let list_rank_sans = (List.filter (fun x -> (rankToValue rang1)  != (rankToValue x) && (rankToValue rang2) != (rankToValue x)) list_rank)
  in match list_rank_sans with
    |h::_ when (rankToValue rang1) >  (rankToValue rang2)  -> DoublePaire(rang1,rang2,h)::l
    |h::_ when (rankToValue rang1) <  (rankToValue rang2)  -> DoublePaire(rang2,rang1,h)::l
    |[]|_::_-> failwith("Mauvaise utilistation de la fonction doublePairAdd")
;;

(*si combinaison = Paire alors ajoute DoublePaire a list_comb*)
(*si combinaison = Brelan alors ajoute Full a list_comb*)
(*Méthode auxiliaire utiliser dans auxDoubleAndFull*)
let findPaire combinaison list_rank list_comb r1 = match combinaison with
  | Paire (k1,k2,k3,k4) -> doublePairAdd list_rank r1 k1 list_comb
  | Brelan (k1,k2,k3) -> Full(k1,r1)::list_comb
  | _ -> failwith("Mauvaise utilistation de la fonction auxDoubleAndFull")
;;

(*si combinaison = Paire alors ajoute Full a list_comb*)
(*Méthode auxiliaire utiliser dans auxDoubleAndFull*)
let findBrelan combinaison list_comb r1 =  match combinaison with
  | Paire (k1,k2,k3,k4) -> Full(r1,k1)::list_comb
  |_ -> failwith("Mauvaise utilistation de la fonction auxDoubleAndFull")
;;

(*Si on trouve un Brelan ou une Paire dans list_comb_tmp, ajoute une DoublePaire ou un Full dans list_comb*)
(*sinon renvoie list_comb*)
(*Méthode auxiliaire utiliser dans doubleAndFull*)
let rec auxDoubleAndFull combinaison list_comb list_comb_tmp list_rank = match list_comb_tmp with
  |[] -> list_comb
  |h::t -> match h with
      |Paire (r1,r2,r3,r4) -> auxDoubleAndFull combinaison (findPaire combinaison list_rank list_comb r1) t list_rank
      |Brelan (r1,r2,r3) -> auxDoubleAndFull combinaison (findBrelan combinaison list_comb r1) t list_rank
      |_ -> auxDoubleAndFull combinaison list_comb t list_rank
;;


(*Si on trouve un Brelan ou une Paire dans list_comb_tmp, on utilise auxDoubleAndFull pour chercher soit un autre Brelan soit une autre Paire pour savoir si il y a un Full ou une Double.*)
(*S'il y'a alors ajoute a list_comb*)
(*Sinon retourne list_comb*)
let rec doubleAndFull list_comb list_comb_tmp list_rank = match list_comb_tmp with
  |[] -> list_comb
  |h::t -> match h with
      |Paire (r1,r2,r3,r4) -> auxDoubleAndFull h list_comb t list_rank
      |Brelan (r1,r2,r3) ->  auxDoubleAndFull h list_comb t list_rank
      |_ -> doubleAndFull list_comb t list_rank
;;

(*Initialise l,coeur,pique,trefle et carreau avec card*)
let rec count card l coeur pique trefle carreau =
  match card with
    | [] -> ()
    | h::tl -> match h with
	| Carte ((rank:rang),color) -> match rank with
	    | Valeur v -> l.(v-2) <- l.(v-2)+1;
	      match color with
		| Pique -> pique.(v-2) <- true;count tl l coeur pique trefle carreau
		| Coeur -> coeur.(v-2) <- true;count tl l coeur pique trefle carreau
		| Carreau -> carreau.(v-2) <- true;count tl l coeur pique trefle carreau
		| Trefle -> trefle.(v-2) <- true;count tl l coeur pique trefle carreau
;;

(*Ajoute Paire, Brelan, Carre et Suite a lc*)
let rec list_comb i lc suite l liste_rang =(*Ajoute les Paire, Brelan et Carre*)
  if i < 0 then match l.(12) with (*Condition pour la suite As 2 3 4 5*)
    | 1| 2| 3| 4 when (suite+1) = 5 -> Suite(Valeur(i+6))::lc
    |_ -> lc
  else match l.(i) with
    | 0 -> list_comb (i-1) lc 0 l liste_rang
    | 1 | 2 | 3 when (suite+1) = 5 -> list_comb (i-1) (Suite(Valeur(i+6))::lc) 0 l liste_rang
    | 1 -> list_comb (i-1) lc (suite+1) l liste_rang
    | 2 -> list_comb (i-1) (pairAdd liste_rang (i+2) lc) (suite+1) l liste_rang
    | 3 -> list_comb (i-1) (brelanAdd liste_rang (i+2) lc) (suite+1) l liste_rang
    | 4 -> list_comb (i-1) (carreAdd liste_rang (i+2) lc) (suite+1) l liste_rang
    | _ -> failwith("Mauvaise utilistation de la fonction list_comb")
;;

(*Ajoute CarteHaute a lc*)
let carteHauteAdd liste_rang lc = match liste_rang with
  | h1::h2::h3::h4::h5::_ -> CarteHaute(h1,h2,h3,h4,h5)::lc
  | []| _::[]| _::_::[]| _::_::_::[]| _::_::_::_::[] -> failwith("Mauvaise utilisation de la mehtode carteHauteAdd")
;;


let compute_comb d t = 
  let l = Array.make 13 0
  and coeur = Array.make 13 false
  and pique = Array.make 13 false
  and trefle = Array.make 13 false
  and carreau = Array.make 13 false
  and card = list_card d t
  in count card l coeur pique trefle carreau;
  let lc = colorOrQuinteFlushAdd trefle 0 0 [] 12 (colorOrQuinteFlushAdd carreau 0 0 [] 12 (colorOrQuinteFlushAdd coeur 0 0 [] 12 (colorOrQuinteFlushAdd pique 0 0 [] 12 [])))(*Ajoute Couleur ou QuinteFlush*)
  in let liste_rang = list_rank l
     in let final_list = list_comb 12 (carteHauteAdd liste_rang lc) 0 l liste_rang(*Ajoute CarteHaute*)
	in doubleAndFull final_list final_list liste_rang (*Ajoute DoublePaire ou Full*)
	
;;

(* retourne la combinaison maximale de la liste (on suppose qu'il y a au moins 1 élément dans la liste) *)
let combMax l =
  let rec aux c l = match l with
    | [] -> c
    | h::t -> if (compare_comb h c) == 1 then aux h t
      else aux c t  
  in aux (List.hd l) l
;;

let compare_hands d1 d2 t = 
  let l1 = compute_comb d1 t
  and l2 = compute_comb d2 t in
  (* print_comb (combMax l1); print_comb (combMax l2); *)
  compare_comb (combMax l1) (combMax l2) 
;;



(* (*Initialise l,coeur,pique,trefle et carreau avec card*)
let count2 card l coeur pique trefle carreau = match card with
  | [] -> ()
  | h::tl -> match h with
    | Carte ((rank:rang),color) -> match rank with
      | Valeur v -> l.(v-2) <- l.(v-2)+1;
          match color with
    | Pique -> pique.(v-2) <- true;count tl l coeur pique trefle carreau
    | Coeur -> coeur.(v-2) <- true;count tl l coeur pique trefle carreau
    | Carreau -> carreau.(v-2) <- true;count tl l coeur pique trefle carreau
    | Trefle -> trefle.(v-2) <- true;count tl l coeur pique trefle carreau
;;



let compute_comb_max d t *)

(*True si c1 = c2 sinon false. Ici c1 et c2 sont des Couleur*) 
let same_color c1 c2 = match c1,c2 with
  |(Pique,Pique) |(Coeur,Coeur) |(Carreau,Carreau) |(Trefle,Trefle) -> true
  |_,_ -> false
;;
  
(*True si card1 = card2 sinon false.Ici card1 et card2 sont des Cartes*)
let same_card card1 card2 = match card1,card2 with
  |(Carte (r1,c1)) ,(Carte (r2,c2)) when ((rankToValue r1) = (rankToValue r2)) && (same_color c1 c2) ->  true
  |_,_ -> false
;;
  
(*Creer une list de valeur pour les rangs*)
let rec make_list_value i liste_de_rang  = if i < 0 then liste_de_rang else make_list_value (i-1) (Valeur(i+2)::liste_de_rang);;
   

(*Ajoute a liste_des_cartes tous les cartes de couleur color*)
(*valeur : Voir make_list_value*)
let rec make_card_with_one_color color valeur liste_des_cartes = match valeur with
  |[] -> liste_des_cartes
  |h::t -> make_card_with_one_color color t (Carte(h,color)::liste_des_cartes)
;;

(*Initialise un paquet de carte (liste_des_cartes) avec le tableau de couleur avec le tableau de valeur*)
let rec init_paquet_carte valeur couleur liste_des_cartes = match couleur with
  |[] -> liste_des_cartes
  |h::t -> init_paquet_carte valeur t (make_card_with_one_color h valeur liste_des_cartes)
;;
  
(*Creer un paquet de carte dans liste_des_cartes*)
let cree_paquet_carte liste_des_cartes =
  let couleur = [Pique;Coeur;Carreau;Trefle]
  and valeur = make_list_value 12 []
  in init_paquet_carte valeur couleur liste_des_cartes
;;

(*Ajoute un donne dans liste_donne_d2 avec comme 1er carte "card" et 2eme carte le reste des cartes dans list_card*)
let rec add_donne card list_card liste_donne_d2 = match list_card with
  | [] -> liste_donne_d2
  | h::t -> add_donne card t (Main(card,h)::liste_donne_d2)
;;

(* Supprime les cartes de la donne d de la liste des cartes *)
let supprimeCartesDonne d l =  match d with
  | Main(c1,c2) -> List.filter (fun carte -> (not ((same_card c1 carte) || (same_card c2 carte)))) l 
;;

(* Supprime les cartes de la table t de la liste des cartes *)
let supprimeCartesTable t l =  match t with
  | Flop (c3,c4,c5) -> List.filter (fun carte -> (not ((same_card c3 carte) || (same_card c4 carte) || (same_card c5 carte)))) l
  | Turn (c3,c4,c5,c6) -> List.filter (fun carte -> (not ((same_card c3 carte) || (same_card c4 carte) || (same_card c5 carte) || (same_card c6 carte)))) l
  | _ -> l
;;

(* Prend un Turn et crée une liste de toutes les River possibles *)
let listRiverWithTurn t paquet =
  let rec aux res t paquet = match paquet with
    | [] -> res
    | h::q -> match t with
              | Turn(c1,c2,c3,c4) -> aux ((River(c1,c2,c3,c4,h))::res) t q 
              | _ -> failwith("Impossible")

  in aux [] t paquet
;;

(* Prend un Flop et crée une liste de toutes les River possibles *)
let listRiverWithFlop t paquet =
  let rec aux res t paquet = match paquet with
    | [] -> res
    | h::q -> match t with
              | Flop(c1,c2,c3) -> aux ((listRiverWithTurn (Turn(c1,c2,c3,h)) q)@res) t q 
              | _ -> failwith("Impossible")

  in aux [] t paquet
;;

(* Prend un Flop ou un Turn et renvoit la liste de toutes les River possibles  *)
let genereTable paquet table = match table with
    | Turn(_,_,_,_) -> listRiverWithTurn table paquet
    | Flop(_,_,_) -> listRiverWithFlop table paquet
    | _ -> failwith("Impossible")
;;

let proba_with_compare d1 d2 t =
  let probaJ1 = compare_hands d1 d2 t in
  match probaJ1 with
    | 1 -> (1.0, 0.0)
    | 0 -> (0.5, 0.5)
    | -1 -> (0.0, 1.0)
    | _ -> failwith("Impossible")
;;

(* Execute proba_with_compare avec une liste de River *)
let proba_with_compare_list d1 d2 liste_river =
  let rec aux j1 j2 d1 d2 liste_river = match liste_river with
    | [] -> (j1,j2)
    | h::t -> let (x, y) = proba_with_compare d1 d2 h in
              aux (j1 +. x) (j2 +. y) d1 d2 t
  in let (resJ1, resJ2) = aux 0. 0. d1 d2 liste_river in
  ((resJ1 /. float_of_int (List.length liste_river)),(resJ2 /. float_of_int (List.length liste_river)))
;;

let proba_double d1 d2 t =
  let paquetCarte = cree_paquet_carte [] in
  let paquetCarteSansD1 = supprimeCartesDonne d1 paquetCarte in
  let paquetCarteSansD1etD2 = supprimeCartesDonne d2 paquetCarteSansD1 in
  let paquetCarteSansD1etD2etTable = supprimeCartesTable t paquetCarteSansD1etD2 in
  match t with
    | River(_,_,_,_,_) -> proba_with_compare d1 d2 t
    | Turn(_,_,_,_) -> proba_with_compare_list d1 d2 (genereTable paquetCarteSansD1etD2etTable t)
    | Flop(_,_,_) -> proba_with_compare_list d1 d2 (genereTable paquetCarteSansD1etD2etTable t)
    | _ -> failwith("Impossible car on suppose qu'il y a au moins 3 cartes sur la table");
;;

let proba_simple d1 t =
  let liste_carte_pour_d2 = match d1,t with(*Supprime les cartes de d1 et t dans liste_carte_pour_d2*)
  |Main(c1,c2),Flop(c3,c4,c5) -> List.filter (fun carte_tab -> not((same_card c1 carte_tab) || (same_card c2 carte_tab) || (same_card c3 carte_tab) || (same_card c4 carte_tab) || (same_card c5 carte_tab))) (cree_paquet_carte [])
  |Main(c1,c2),Turn(c3,c4,c5,c6) -> List.filter (fun carte_tab -> not((same_card c1 carte_tab) || (same_card c2 carte_tab) || (same_card c3 carte_tab) || (same_card c4 carte_tab) || (same_card c5 carte_tab) || (same_card c6 carte_tab))) (cree_paquet_carte [])
  |Main(c1,c2),River(c3,c4,c5,c6,c7) ->List.filter (fun carte_tab -> not((same_card c1 carte_tab) || (same_card c2 carte_tab) || (same_card c3 carte_tab) || (same_card c4 carte_tab) || (same_card c5 carte_tab) || (same_card c6 carte_tab)  || (same_card c7 carte_tab))) (cree_paquet_carte [])
  in let rec toute_donne_d2 liste_carte_pour_d2 liste_donne_d2 = match liste_carte_pour_d2 with(*Creer une liste de toutes les donnes possible pour d2*)
       |[] -> liste_donne_d2
       |h::t -> toute_donne_d2 t (add_donne h t liste_donne_d2)
     in let donne_d2 = toute_donne_d2 liste_carte_pour_d2 []
        in let rec tableau_proba donne_d2 tab_prob_d1_win = match donne_d2 with (*Creer la liste des probabilités de victoire de d1 avec toute les donne possibles de d2*)
	  |[] -> tab_prob_d1_win
	  |h::t -> tableau_proba t (fst(proba_double d1 h t)::tab_prob_d1_win)
	   in let tab_prob_d1_win = tableau_proba donne_d2 []
	      in let rec proba_win_d1 tab_prob_d1_win accumulateur = match tab_prob_d1_win with
		|[] -> accumulateur
		|h::t -> proba_win_d1 t (accumulateur*.h)
		 in proba_win_d1 tab_prob_d1_win 1.0		 
;;

let char_to_valeur char = match char with
  |'A' -> 14
  |'R' -> 13
  |'D' -> 12
  |'V' -> 11
  |'1' -> 10
  |'9' -> 9
  |'8' -> 8
  |'7' -> 7
  |'6' -> 6
  |'5' -> 5
  |'4' -> 4
  |'3' -> 3
  |'2' -> 2
  |_ -> failwith("Mauvaise valeur dans le fichier");
;;

let char_to_color char = match char with
  |'p' -> Pique
  |'o' -> Coeur
  |'a' -> Carreau
  |'t' -> Trefle
  |_ -> failwith("Mauvaise valeur dans le fichier");    
;;

let make_card_with_string string =
  let valeur = char_to_valeur (String.get string 0)
  in let color = if valeur = 10 then match (String.get string 2) with
    |'c' -> char_to_color (String.get string 3)
    |_ -> char_to_color (String.get string 2)
    else match (String.get string 1) with
    |'c' -> char_to_color (String.get string 2)
    |_ -> char_to_color (String.get string 1)
     in Carte(Valeur(valeur),color)
;;

let make_donne line =
  let index_space = String.rindex line ' '
  in let first_card = String.sub line 0 index_space
  and second_card = String.sub line (index_space+1) ((String.length line)-(index_space+1))
     in Main(make_card_with_string first_card,make_card_with_string second_card)
;;

let rec string_to_tabString string tab_String =
  try
    let index_space = String.rindex string ' '
    in string_to_tabString (String.sub string 0 index_space) ((String.sub string (index_space+1) ((String.length string)-(index_space+1)))::tab_String)
  with
      Not_found -> string::tab_String
	
;;

let make_table string =
  let tab_table = string_to_tabString string []
  in match tab_table with
    |h1::h2::h3::[] -> Flop(make_card_with_string h1,make_card_with_string h2,make_card_with_string h3)
    |h1::h2::h3::h4::[] -> Turn(make_card_with_string h1,make_card_with_string h2,make_card_with_string h3,make_card_with_string h4)
    |h1::h2::h3::h4::h5::[] -> River(make_card_with_string h1,make_card_with_string h2,make_card_with_string h3,make_card_with_string h4,make_card_with_string h5)
    |[]|_::[]|_::_::[]|_::_::_::_::_::_::_ -> failwith("Mauvaise ligne de table");
;;

let lecture_de_fichier file =
  let reader = open_in file
  in try
       let d1 = make_donne (input_line reader)
       in let d2_string = input_line reader
	  in let table = make_table (input_line reader)
	     in match d2_string with
	       |"?" -> print_string("Joueur 1: ");print_float(proba_simple d1 table);print_newline()
	       |_ -> let d2 = make_donne d2_string
		     in let proba_d = proba_double d1 d2 table
			in match proba_d with
			  |(1.0,0.0) -> print_endline("Le joueur 1 est gagnant.")
			  |(0.0,1.0) -> print_endline("Le joueur 2 est gagnant.")
			  |(p1,p2) -> print_string("Joueur 1: ");
			    print_float(p1);
			    print_newline();
			    print_string("Joueur 2: ");
			    print_float(p2);
			    print_newline()
    with
      |End_of_file -> failwith("Erreur de fichier")
;;

let test1 = Suite(Valeur(7));;
let test2 = Suite(Valeur(8));;
let test3 = Suite(Valeur(9));;
let test4 = Suite(Valeur(10));;
let test5 = QuinteFlush(Valeur(6));;

let lstComb = test1::test2::test3::test4::test5::[];;

let max = combMax lstComb;;

let main1 = Main(Carte(Valeur(3),Pique),Carte(Valeur(2),Coeur));;
let main2 = Main(Carte(Valeur(13),Pique),Carte(Valeur(8),Coeur));;
let table = River(Carte(Valeur(9),Coeur),Carte(Valeur(14),Pique),Carte(Valeur(5),Pique),Carte(Valeur(4),Pique),Carte(Valeur(2),Pique));;
let table2 = Turn(Carte(Valeur(9),Coeur),Carte(Valeur(14),Pique),Carte(Valeur(5),Pique),Carte(Valeur(4),Pique));;

let compute1 = compute_comb main1 table;;
let compute2 = compute_comb main2 table;;
let max1 = combMax compute1;;
let max2 = combMax compute2;;

let b = compare_hands main1 main2 table;;

let c = compare_comb test1 test2;;  

let a = proba_simple main1 table;;


let paquetDeCartesCree = cree_paquet_carte [];;
let remove1 = supprimeCartesDonne main1 paquetDeCartesCree;;
let remove2 = supprimeCartesDonne main2 remove1;;
let testprobaDouble1 = proba_double main1 main2 table;;
let testprobaDouble2 = proba_double main1 main2 table2;;

open Graphics;;
open_graph " 500x500";;

let color_to_string color = match color with
  | Pique -> "p"
  | Coeur -> "co"
  | Carreau -> "ca"
  | Trefle -> "t"
;;

let rank_to_string rank = match rank with
  | 14 -> "A"
  | 13 -> "R"
  | 12 -> "D"
  | 11 -> "V"
  | 10 -> "10"
  | 9 -> "9"
  | 8 -> "8"
  | 7 -> "7"
  | 6 -> "6"
  | 5 -> "5"
  | 4 -> "4"
  | 3 -> "3"
  | 2 -> "2"
  |_ -> failwith("Mauvaise valeur de rang");
;;


let draw_card carte x y =
  draw_rect x y 25 50;
  match carte with
    |Carte (r,c) -> moveto (x+7) (y+35);
      draw_string (rank_to_string (rankToValue r));
      moveto (x+7) (y+5);
      draw_string (color_to_string c)     
;;


draw_card (Carte(Valeur(14),Pique)) 100 100;;
(*close_graph*)

