open Poker;;

let lecture_de_fichier file =
	let reader = open_in file
  	in try
  		let d1 = make_donne (input_line reader)
      in let d2_string = input_line reader
	    in let table = make_table (input_line reader)
      in match d2_string with
       		|"?" -> print_string("Joueur 1: "); print_float(proba_simple d1 table); print_newline()
         	|_ -> let d2 = make_donne d2_string
        in let proba_d = proba_double d1 d2 table
      	in match proba_d with
        	|(1.0,0.0) -> print_endline("Le joueur 1 est gagnant.")
        	|(0.0,1.0) -> print_endline("Le joueur 2 est gagnant.")
			    |(0.5,0.5) -> print_endline("Egalite.")
        	|(p1,p2) -> print_string("Joueur 1: ");
          				print_float(p1);
          				print_newline();
          				print_string("Joueur 2: ");
          				print_float(p2);
          				print_newline()
    with
      | End_of_file | SYNTAXE_ERROR -> failwith("Erreur de fichier")
;;


let t = Sys.time() in
lecture_de_fichier Sys.argv.(1);
Printf.printf "Temps d'execution: %fs\n" (Sys.time() -. t)