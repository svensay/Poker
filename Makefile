all: compute interface

#Edition des liens et création de l'éxécutable
compute: Poker.cmo Lecture.cmo
	ocamlc -o compute Poker.cmo Lecture.cmo

interface: Poker.cmo Interface.cmo
	ocamlc -o interfaceG graphics.cma Poker.cmo Interface.cmo

#Compilation du corps du module Poker
Poker.cmo: Poker.ml
	ocamlc -c Poker.ml

#Compilation du corps du module Lecture
Lecture.cmo: Lecture.ml
	ocamlc -c Lecture.ml

#Compilation du corps du module Interface
Interface.cmo: Interface.ml
	ocamlc -c Interface.ml

#Effacer fichiers auxiliaires
clean:
	rm *.cmi *.cmo compute interfaceG