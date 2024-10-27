# Nom du programme final
EXEC = main

# Dossier de sortie pour les fichiers binaires et objets
BUILD_DIR = bin

# Fichiers sources
SRC = src/lambda_ast.ml src/reduction.ml src/typing_ast.ml src/typing.ml src/main.ml src/terms.ml

# Fichiers objets générés (extension .cmo)
OBJ = $(SRC:.ml=.cmo)

# Commandes OCaml
OCAMLC = ocamlfind ocamlc

# on utilise le package ounit2 pour les tests unitaires
OCAMLFLAGS = -w -g -package ounit2 -I src
LIBS = -linkpkg

# Cibles par défaut
all: create_build_dir $(BUILD_DIR)/$(EXEC)

# Crée le répertoire pour les binaires s'il n'existe pas
create_build_dir:
	@mkdir -p $(BUILD_DIR)

# Compile les fichiers sources et crée l'exécutable
$(BUILD_DIR)/$(EXEC): $(OBJ)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OBJ) 

# Compile chaque fichier source individuellement
%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

# Ajouter une cible pour les tests avec OUnit2
test_reductions: all
	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o bin/test_program src/lambda_ast.ml src/reduction.ml src/terms.ml tests/tests_reductions.ml
	./bin/test_program
	
test_typing: all
	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o bin/test_program src/lambda_ast.ml src/typing_ast.ml src/typing.ml src/terms.ml tests/tests_typing.ml
	./bin/test_program

retest_reductions: clean all test_reductions
retest_typing: clean all test_typing
retest : clean all test_reductions test_typing


# Nettoyer les fichiers de test
clean: 
	rm -f *.cmo *.cmi src/*.cmo src/*.cmi tests/*.cmo tests/*.cmi *.log *.cache 

# Clean complet : supprime également les fichiers binaires
fclean: clean
	rm -rf $(BUILD_DIR)

# Recompile tout le projet
re: fclean all
