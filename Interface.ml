(* #load "graphics.cma";; *)
open Graphics;;
open Poker;;

(* open_graph " 500x500";; *)

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


(* let draw_card carte x y =
  draw_rect x y 25 50;
  match carte with
    |Carte (r,c) -> moveto (x+7) (y+35);
      draw_string (rank_to_string (rankToValue r));
      moveto (x+7) (y+5);
      draw_string (color_to_string c)     
;;


draw_card (Carte(Valeur(14),Pique)) 450 400;; *)
(*close_graph*)

