open Poker

let lecture_de_fichier file =
	printf "%s\n" file;
  	let reader = open_in file
	in try
		let d1 = make_donne (input_line reader)
		in let d2_string = input_line reader
   		in let table = make_table (input_line reader)
   		in match d2_string with
	       |"?" -> print_string("Joueur 1: ");
	       		let (x, y) = proba_simple d1 table in
	       		   print_float x; print_float y;
	       		   print_newline();
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

let () = lecture_de_fichier Sys.argv.(2);;