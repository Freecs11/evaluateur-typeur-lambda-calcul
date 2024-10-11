# Nom du programme final
EXEC = main

# Dossier de sortie pour les fichiers binaires et objets
BUILD_DIR = bin

# Fichiers sources
SRC = lambda_ast.ml reduction.ml typing_ast.ml typing.ml main.ml

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
test: all
	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o test_program lambda_ast.ml reduction.ml tests.ml
	./test_program
	

# Nettoyer les fichiers de test
clean: 
	rm -f *.cmi *.cmo *.o test_program *.log *.cache

# Clean complet : supprime également les fichiers binaires
fclean: clean
	rm -rf $(BUILD_DIR)

# Recompile tout le projet
re: fclean all
