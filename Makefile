#Edition des liens et création de l'éxécutable
compute: poker.cmo lecture.cmo
	ocamlc -o compute poker.cmo lecture.cmo

#Compilation du corps du module poker
poker.cmo: poker.ml
	ocamlc -c poker.ml

#Compilation du corps du module lecture
lecture.cmo: lecture.ml poker.cmi
	ocamlc -c lecture.ml

#Effacer fichiers auxiliaires
clean:
	rm *.cmi *.cmo