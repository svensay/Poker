(* #load "graphics.cma";; *)
open Graphics;;
open Poker;;


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

let draw_d1 d1 = match d1 with
  |Main (c1,c2) -> draw_card c1 150 390;draw_card c2 180 390
;;
let draw_d2 d2 = match d2 with
  |Main (c1,c2) -> draw_card c1 150 330;draw_card c2 180 330
;;

let draw_table t = match t with
 |Flop(c1,c2,c3) -> draw_card c1 150 270;draw_card c2 180 270;draw_card c3 210 270
 |Turn(c1,c2,c3,c4) -> draw_card c1 150 270;draw_card c2 180 270;draw_card c3 210 270;draw_card c4 240 270
 |River(c1,c2,c3,c4,c5) -> draw_card c1 150 270;draw_card c2 180 270;draw_card c3 210 270;draw_card c4 240 270;draw_card c5 270 270
;;

let draw_proba () = moveto 0 200;lineto 500 200;
;;

draw_d1 wtf1;;
draw_d2 wtf2;;
draw_table wtf3;;
draw_proba ();;
