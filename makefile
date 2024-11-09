# # Nom du programme final
# EXEC = main

# # Dossier de sortie pour les fichiers binaires et objets
# BUILD_DIR = bin

# # Fichiers sources
# SRC = src/lambda_ast.ml src/reduction.ml src/typing_ast.ml src/typing.ml src/main.ml src/terms.ml

# # Fichiers objets générés (extension .cmo)
# OBJ = $(SRC:.ml=.cmo)

# # Commandes OCaml
# OCAMLC = ocamlfind ocamlc

# # on utilise le package ounit2 pour les tests unitaires
# OCAMLFLAGS = -w -g -package ounit2 -I src
# LIBS = -linkpkg

# # Cibles par défaut
# all: create_build_dir $(BUILD_DIR)/$(EXEC)

# # Crée le répertoire pour les binaires s'il n'existe pas
# create_build_dir:
# 	@mkdir -p $(BUILD_DIR)

# # Compile les fichiers sources et crée l'exécutable
# $(BUILD_DIR)/$(EXEC): $(OBJ)
# 	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OBJ) 

# # Compile chaque fichier source individuellement
# %.cmo: %.ml
# 	$(OCAMLC) $(OCAMLFLAGS) -c $<

# # Ajouter une cible pour les tests avec OUnit2
# test_reductions: all
# 	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o bin/test_program src/lambda_ast.ml src/reduction.ml src/terms.ml tests/tests_reductions.ml
# 	./bin/test_program
	
# test_typing: all
# 	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o bin/test_program src/lambda_ast.ml src/typing_ast.ml src/typing.ml src/terms.ml tests/tests_typing.ml
# 	./bin/test_program

# # tests de reductions avec les nouveaux types 
# test_redWTypes : all
# 	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o bin/test_program src/lambda_ast.ml src/typing_ast.ml src/typing.ml src/reduction.ml src/terms.ml tests/tests_reduction_w_new_types.ml
# 	./bin/test_program 

# test_tyWTypes : all
# 	ocamlfind ocamlc $(OCAMLFLAGS) $(LIBS) -o bin/test_program src/lambda_ast.ml src/typing_ast.ml src/typing.ml src/reduction.ml src/terms.ml tests/tests_typing_w_new_types.ml
# 	./bin/test_program

# retest_reductions: clean all test_reductions
# retest_nrt : clean all test_redWTypes
# retest_typing: clean all test_typing
# retest_nt : clean all test_tyWTypes
# retest : clean all test_reductions test_typing test_redWTypes test_tyWTypes


# # Nettoyer les fichiers de test
# clean: 
# 	rm -f *.cmo *.cmi src/*.cmo src/*.cmi tests/*.cmo tests/*.cmi *.log *.cache 

# # Clean complet : supprime également les fichiers binaires
# fclean: clean
# 	rm -rf $(BUILD_DIR)

# # Recompile tout le projet
# re: fclean all




# Nom du programme final
EXEC = main

# Dossier de sortie pour les fichiers binaires et objets
BUILD_DIR = bin

# Fichiers sources
SRC = src/lambda_ast.ml src/reduction.ml src/typing_ast.ml src/typing.ml src/terms.ml

# Fichiers objets générés (dans BUILD_DIR, extension .cmo)
OBJ = $(patsubst src/%.ml, $(BUILD_DIR)/%.cmo, $(SRC))

# Fichiers tests
TESTS = tests/tests_reductions.ml tests/tests_typing.ml tests/tests_reduction_w_new_types.ml tests/tests_typing_w_new_types.ml

# Commandes OCaml
OCAMLC = ocamlfind ocamlc

# On utilise le package ounit2 pour les tests unitaires
OCAMLFLAGS = -w -g -package ounit2 -I src -I $(BUILD_DIR)
LIBS = -linkpkg

# Cibles par défaut
all: create_build_dir $(BUILD_DIR)/$(EXEC)

# Crée le répertoire pour les binaires s'il n'existe pas
create_build_dir:
	@mkdir -p $(BUILD_DIR)

# Compile les fichiers sources et crée l'exécutable principal
$(BUILD_DIR)/$(EXEC): $(OBJ)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OBJ)

# Compile chaque fichier source individuellement dans BUILD_DIR
$(BUILD_DIR)/%.cmo: src/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

# Compile chaque fichier test individuellement dans BUILD_DIR
$(BUILD_DIR)/%.cmo: tests/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

# Ajouter une cible pour les tests avec OUnit2
# Chaque test est compilé en un exécutable séparé dans BUILD_DIR avec ses propres logs
test_reductions: all $(BUILD_DIR)/tests_reductions.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS) -o $(BUILD_DIR)/test_reductions \
		$(OBJ) $(BUILD_DIR)/tests_reductions.cmo
	cd $(BUILD_DIR) && ./test_reductions | tee test_reductions.log

test_typing: all $(BUILD_DIR)/tests_typing.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS) -o $(BUILD_DIR)/test_typing \
		$(OBJ) $(BUILD_DIR)/tests_typing.cmo
	cd $(BUILD_DIR) && ./test_typing | tee test_typing.log

# Tests de reductions avec les nouveaux types 
test_redWTypes: all $(BUILD_DIR)/tests_reduction_w_new_types.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS) -o $(BUILD_DIR)/test_redWTypes \
		$(OBJ) $(BUILD_DIR)/tests_reduction_w_new_types.cmo
	cd $(BUILD_DIR) && ./test_redWTypes | tee test_redWTypes.log

test_tyWTypes: all $(BUILD_DIR)/tests_typing_w_new_types.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS) -o $(BUILD_DIR)/test_tyWTypes \
		$(OBJ) $(BUILD_DIR)/tests_typing_w_new_types.cmo
	cd $(BUILD_DIR) && ./test_tyWTypes | tee test_tyWTypes.log

# Cibles pour re-exécuter les tests après nettoyage
retest_reductions: clean all test_reductions
retest_nrt: clean all test_redWTypes
retest_typing: clean all test_typing
retest_nt: clean all test_tyWTypes
retest: clean all test_reductions test_typing test_redWTypes test_tyWTypes

# Nettoyer les fichiers de test et objets dans BUILD_DIR et supprimer les artefacts à la racine
clean:
	rm -f $(BUILD_DIR)/*.cmo $(BUILD_DIR)/*.cmi \
	      $(BUILD_DIR)/*.log $(BUILD_DIR)/*.cache \
	      *.log *.cache

# Clean complet : supprime également le répertoire bin
fclean: clean
	rm -rf $(BUILD_DIR)

# Recompile tout le projet
re: fclean all
