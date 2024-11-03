(* tests/tests_typing.ml *)

open OUnit2
open Typing_ast
open Typing
open Lambda_ast

(* Test de l'inférence de type pour une variable *)
let test_var_type _ctxt =
  let term = Var "x" in
  let env = [("x", TNat)] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect" (types_alpha_equal infer_typered expected)

(* Test de l'inférence de type pour une abstraction *)
let test_abs_type _ctxt =
  let term = Abs ("x", Add (Var "x", Int 2)) in
  let env = [("x", TNat)] in
  let infer_typered = infer_type term env in
  let expected = TArr (TNat, TNat) in
  assert_bool "Type inféré incorrect" (types_alpha_equal infer_typered expected)

(* Test de l'inférence de type pour une application *)
let test_app_type _ctxt =
  let term = App (Abs ("x", Add (Var "x", Int 2)), Int 3) in
  let env = [("x", TNat)] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect" (types_alpha_equal infer_typered expected)

(* Test de l'inférence de type pour des opérations avec variables *)
let test_add_with_variable_type _ctxt =
  let term = Add (Var "x", Int 3) in
  let env = [("x", TNat)] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect" (types_alpha_equal infer_typered expected)

(* Test de l'inférence de type pour des abstractions imbriquées *)
let test_nested_abs_type _ctxt =
  let term = Abs ("x", Abs ("y", Add (Var "x", Var "y"))) in
  let env = [("x", TNat); ("y", TNat)] in
  let infer_typered = infer_type term env in
  let expected = TArr (TNat, TArr (TNat, TNat)) in
  print_endline ("Type inféré : " ^ (print_type infer_typered));
  assert_bool "Type inféré incorrect" (types_alpha_equal infer_typered expected)

(* Test d'un terme mal typé *)
let test_malformed_term_type _ctxt =
  let term = Add (Abs ("x", Var "x"), Int 3) in
  let env = [] in
  print_endline ("Testing malformed term: " ^ print_term term);
  assert_raises
    (Unification_Failed "Unification failed : different constructors")
    (fun () -> infer_type term env)

(* Tests pour les listes *)
let test_list_type _ctxt =
  let term = Cons (Int 1, Cons (Int 2, Nil)) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TList TNat in
  assert_bool "Type inféré incorrect pour liste" 
    (types_alpha_equal infer_typered expected)

let test_head_type _ctxt =
  let term = Head (Cons (Int 1, Nil)) in
  let env = [] in
  let infer_typered = infer_type term env in
  (* Le type de Head est ∀X.[X] → X selon le sujet *)
  let expected = TForall ("X", TArr (TList (TNat), TNat)) in
  print_endline ("Type inféré pour Head: " ^ print_type infer_typered);
  print_endline ("Type attendu pour Head: " ^ print_type expected);
  assert_bool "Type inféré incorrect pour Head" 
    (types_alpha_equal infer_typered expected)

(* Tests pour les fonctions polymorphes *)
let test_polymorphic_identity _ctxt =
  let term = Let ("id", 
    Abs ("x", Var "x"),
    App (Var "id", Int 1)
  ) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect pour identité polymorphe" 
    (types_alpha_equal infer_typered expected)

let test_polymorphic_map _ctxt =
  (* Définir map via Let pour bénéficier du let-polymorphisme *)
  let term = Let ("map", 
    Fix ("self", 
      Abs ("f",
        Abs ("lst",
          IfEmpty (Var "lst",
            Nil,
            Cons (
              App (Var "f", Head (Var "lst")),
              App (App (Var "self", Var "f"), Tail (Var "lst"))
            )
          )
        )
      )
    ),
    App (
      App (Var "map", Abs ("x", Add (Var "x", Int 1))),
      Cons (Int 1, Cons (Int 2, Nil))
    )
  ) in
  
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TList TNat in
  Printf.printf "Type inféré: %s\n" (print_type infer_typered);
  assert_bool "Type inféré incorrect pour map"
    (types_alpha_equal infer_typered expected)

(* Tests pour la factorielle *)
let test_factorial_type _ctxt =
  let multiply = Fix ("mult", Abs ("x", 
    Abs ("y",
      IfZero (Var "y",
        Int 0,
        Add (Var "x", 
          App (App (Var "mult", Var "x"), Sub (Var "y", Int 1))
        )
      )
    )
  )) in
  let term = Fix ("factorial", Abs ("n",
    IfZero (Var "n",
      Int 1,
      App (
        App (multiply, Var "n"),
        App (Var "factorial", Sub (Var "n", Int 1))
      )
    )
  )) in
  let env = [] in
  let infer_typered = infer_type  term env in
  let expected = TArr (TNat, TNat) in
  assert_bool "Type inféré incorrect pour factorielle" 
    (types_alpha_equal infer_typered expected)

(* Suite de tests *)
let typing_tests =
  "Typing Tests" >::: [
    (* "test_var_type" >:: test_var_type;
    "test_abs_type" >:: test_abs_type;
    "test_app_type" >:: test_app_type;
    "test_add_with_variable_type" >:: test_add_with_variable_type;
    "test_nested_abs_type" >:: test_nested_abs_type;
    "test_malformed_term_type" >:: test_malformed_term_type;
    "test_list_type" >:: test_list_type;
    "test_head_type" >:: test_head_type;
    "test_polymorphic_identity" >:: test_polymorphic_identity; *)
    "test_polymorphic_map" >:: test_polymorphic_map;
    (* "test_factorial_type" >:: test_factorial_type; *)
    (* Ajoutez d'autres tests ici *)
  ]

(* Exécution des tests *)
let () =
  run_test_tt_main typing_tests
