#Edition des liens et création de l'éxécutable
compute: Poker.cmo Lecture.cmo
	ocamlc -o compute graphics.cma Poker.cmo Lecture.cmo

#Compilation du corps du module Poker
Poker.cmo: Poker.ml
	ocamlc -c Poker.ml

#Compilation du corps du module Lecture
Lecture.cmo: Lecture.ml
	ocamlc -c Lecture.ml

#Effacer fichiers auxiliaires
clean:
	rm *.cmi *.cmo