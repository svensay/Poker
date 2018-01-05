open Graphics;;
open Poker;;

open_graph " 500x500";;
let w = 500;;
let h = 500;;

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
  draw_rect x y 25 40;
  match carte with
    |Carte (r,c) -> moveto (x+7) (y+25);
                    draw_string (rank_to_string (rankToValue r));
                    moveto (x+7) (y+4);
                    draw_string (color_to_string c)
;;

let draw_d1 d1 = match d1 with
  | Main (c1,c2) -> draw_card c1 150 390;draw_card c2 180 390
;;
let draw_d2 d2 = match d2 with
  | Main (c1,c2) -> draw_card c1 150 330;draw_card c2 180 330
;;

let draw_table t = match t with
  | Flop(c1,c2,c3) -> draw_card c1 150 270; draw_card c2 180 270; draw_card c3 210 270
  | Turn(c1,c2,c3,c4) -> draw_card c1 150 270; draw_card c2 180 270; draw_card c3 210 270; draw_card c4 240 270
  | River(c1,c2,c3,c4,c5) -> draw_card c1 150 270; draw_card c2 180 270; draw_card c3 210 270; draw_card c4 240 270; draw_card c5 270 270
;;

let draw_proba_double d1 d2 t = moveto 0 200; lineto w 200;
  let time = Sys.time()
  in let proba_d = proba_double d1 d2 t
  in let true_time = (Sys.time() -. time)
	in match proba_d with
	  |(1.0,0.0) -> moveto 150 150;
            	    draw_string "Le joueur 1 est gagnant.";
            	    moveto 150 50;
            	    draw_string ("("^(string_of_float true_time)^"s)");
	  |(0.0,1.0) -> moveto 150 150;
            	    draw_string "Le joueur 2 est gagnant.";
            	    moveto 150 50;
            	    draw_string ("("^(string_of_float true_time)^"s)");
	  |(0.5,0.5) -> moveto 150 150;
            	    draw_string "Egalite";
            	    moveto 150 50;
            	    draw_string ("("^(string_of_float true_time)^"s)");
	  |(p1,p2) -> moveto 150 150;
          	    draw_string "Joueur 1 :";
          	    moveto 250 150;
          	    draw_string (string_of_float p1);
          	    moveto 150 100;
          	    draw_string "Joueur 2 : ";
          	    moveto 250 100;
          	    draw_string (string_of_float p2);
          	    moveto 200 50;
          	    draw_string ("("^(string_of_float true_time)^"s)");
;;

let draw_proba_simple d1 t = moveto 0 200;lineto w 200;
  let time = Sys.time()
  in let prob_simp = proba_simple d1 t
  in let true_time = (Sys.time() -. time)
	in moveto 100 150;
	draw_string "Joueur 1 : ";
	moveto 200 150;
	draw_string (string_of_float prob_simp);
	moveto 350 150;
	draw_string ("("^(string_of_float true_time)^"s)");
;;

let rec read_d1 () =
  let d1_string = read_line ()
  in try
       let d1 = make_donne d1_string
       in draw_d1 d1; d1
    with
      |SYNTAXE_ERROR -> print_string("Mauvaise entree pour la donne 1, veuillez reessayer :"); read_d1 ()
;;

let rec read_d2 () =
  let d2_string = read_line ()
  in try
       match d2_string with
      	 |"?" -> None
      	 |_ -> let d2 = make_donne d2_string
	             in draw_d2 d2; Some d2
    with
      |SYNTAXE_ERROR -> print_string("Mauvaise entree pour la donne 2, veuillez reessayer :"); read_d2 ()
;;

let rec read_tab () = 
  let t_string = read_line ()
  in try
       let t = make_table t_string
       in draw_table t;t
    with
      |SYNTAXE_ERROR -> print_string("Mauvaise entree pour la table, veuillez reessayer :"); read_tab ()
;;

let affichage () =
  print_string("Veuillez entrer la 1ere donne: ");
  let d1 = read_d1 () 
  in print_string("Veuillez entrer la 2eme donne: ");
  let d2_option = read_d2 ()
  in print_string("Veuillez entrer la table: ");
  let t = read_tab ()
  in match d2_option with
    |None ->  moveto 150 330;
              draw_string "?";
              draw_proba_simple d1 t;
    |Some d2 -> draw_d2 d2;
                draw_proba_double d1 d2 t  
;;

let rec loop() =
  affichage ();print_string("Voulez vous une autre configuration (y\\n) : ");
  let rec answers () =
    let repeat =  read_line ()
    in match repeat with
      |"y" -> clear_graph ();loop()
      |"n" -> close_graph ()
      | _ -> print_string("Mauvaise reponse reesayer (y\\n) : ");answers ()
  in answers ()
  
;;

loop ();;
