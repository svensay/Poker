
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
;;

(* Pas encore fini *)
(* Compare deux combinaisons "du même type" *)
let compare_comb_equals c1 c2 = match c1, c2 with
  | (QuinteFlush r), (QuinteFlush r2) | (Suite r), (Suite r2) -> 
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
      | _ -> aux (i+1) (Valeur(i+1)::l)
  in aux 0 []
;;

let rec carreAdd list_rank i l = match list_rank with
  |h::t when (match h with Valeur value -> value) = i -> carreAdd t i l
  |h::_ -> Carre(Valeur(i),h)::l
  |[] -> failwith("Tableau Vide")
;;

let rec pairAdd list_rank i l =
  let list_rank_sans_i = (List.filter (fun x -> i != (match x with Valeur value -> value)) list_rank)
  in match list_rank_sans_i with
    |h1::h2::h3::t -> Paire(Valeur(i),h1,h2,h3)::l
    |[] |_::[] |_::_::[] -> failwith("Mauvaise utilistation de la fonction pairAdd")
;;



let compute_comb d t =
  let l = Array.make 13 0
  and coeur = Array.make 13 false
  and pique = Array.make 13 false
  and trefle = Array.make 13 false
  and carreau = Array.make 13 false
  and card = list_card d t in
  let rec count c =
    match c with
      | [] -> ()
      | h::tl -> match h with
	  | Carte ((rank:rang),color) -> match rank with
	      | Valeur v -> l.(v-1) <- l.(v-1)+1;
		match color with
		  | Pique -> pique.(v-1) <- true;count tl 
		  | Coeur -> coeur.(v-1) <- true;count tl
		  | Carreau -> carreau.(v-1) <- true;count tl
		  | Trefle -> trefle.(v-1) <- true;count tl
  in count card;
  
  let liste_rang = list_rank l in
  let rec list_comb i lc =
    if i < 0 then lc
    else match l.(i) with
      | 0 -> list_comb (i-1) lc
      | 1 -> list_comb (i-1) (Suite(Valeur(i+5))::lc)
      | 2 -> list_comb (i-1) (pairAdd liste_rang (i+1) lc)
      | 3 -> list_comb (i-1) (Suite(Valeur(i+5))::lc)
      | 4 -> list_comb (i-1) (carreAdd liste_rang (i+1) lc)
      | _ -> failwith("Pas possible")
  in list_comb 12 []
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
        print_comb (combMax l1); print_comb (combMax l2);
        compare_comb (combMax l1) (combMax l2) 
;;

let test1 = Suite(Valeur(7));;
let test2 = Suite(Valeur(8));;
let main1 = Main(Carte(Valeur(1),Pique),Carte(Valeur(1),Coeur));;
let main2 = Main(Carte(Valeur(13),Pique),Carte(Valeur(9),Coeur));;
let table = River(Carte(Valeur(9),Pique),Carte(Valeur(10),Carreau),Carte(Valeur(11),Trefle),Carte(Valeur(8),Coeur),Carte(Valeur(2),Pique));;

let a = compute_comb main1 table;;

let b = compare_hands main1 main2 table;;

let c = compare_comb test1 test2;;  
