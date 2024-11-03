open Typing_ast  
open Lambda_ast  
open Typing       
open OUnit2 
open Terms




(* ----------------- Tests d'unification ----------------- *)

(* test simple d'unification , on teste T1 = T1 et T1 -> T2 = T1 -> T2 *)
let test_unification_equation_simple _ =
  let eqs = [
    (TVar "T1", TVar "T1");
    (TArr (TVar "T1", TVar "T2"), TArr (TVar "T1", TVar "T2"))
  ] in
  let (eqs', env) = unification eqs [] in
  let expected_eqs = [] in
  let expected_env = [] in
  assert_equal ~msg:"Liste d'équations résiduelles incorrecte" expected_eqs eqs';
  assert_equal ~msg:"Environnement de substitution incorrect" expected_env env


(* test d'unification un peu plus complexe, on teste T1 = T2 et T1 -> T2 = T3 -> T4 
    on s'attend à ce que l'unification réussisse et que l'environnement de substitution soit 
    T3 -> T4, T2 -> T3, T1 -> T2
*)
let test_unification_equation_complex _ =
  let eqs = [
    (TVar "T1", TVar "T2");
    (TArr (TVar "T1", TVar "T2"), TArr (TVar "T3", TVar "T4"))
  ] in
  let (eqs', env) = unification eqs [] in
  let expected_eqs = [] in
  let expected_env = [
    ("T3", TVar "T4");
    ("T2", TVar "T3");
    ("T1", TVar "T2")
  ] in
  assert_equal ~msg:"Liste d'équations résiduelles incorrecte" expected_eqs eqs';
  assert_equal ~msg:"Environnement de substitution incorrect" expected_env env

(* Test d'unification échouée due à une substitution cyclique *)
let test_unification_failure _ =
  let eqs = [
    (TVar "T1", TArr (TVar "T1", TNat))  (* Tentative de T1 = T1 -> TNat, ce qui est cyclique *)
  ] in
  assert_raises
    (Unification_Failed "Unification failed")
    (fun () -> unification eqs [])

  (* Test d'unification avec occurs_check *)
let test_unification_occurs_check _ =
  let eqs = [
    (TVar "T1", TVar "T2"); (* T1 = T2 *)
    (TVar "T2", TArr (TVar "T1", TNat)) (* T2 = T1 -> Nat *)
  ] in
  assert_raises
    (Unification_Failed "Unification failed")
    (fun () -> unify eqs [] 100)

(* Test d'unification réussie avec substitution multiple *)
let test_unification_multiple_substitutions _ =
  let eqs = [ 
    (TVar "T1", TVar "T2"); (* T1 = T2 *)
    (TVar "T2", TVar "T3"); (* T2 = T3 *)
    (TVar "T3", TNat)       (* T3 = Nat *)
  ] in
  let (eqs', env) = unification eqs [] in
  let expected_eqs = [] in
  let expected_env = [
    ("T3", TNat); 
    ("T2", TVar "T3");
    ("T1", TVar "T2");
  ] in
  assert_equal ~msg:"Liste d'équations résiduelles incorrecte" expected_eqs eqs';
  assert_equal ~msg:"Environnement de substitution incorrect" expected_env env

(* ----------------- Tests de substitution ----------------- *)

(* Fonction d'unité pour tester la substitution *)
let test_substitution _ =
  let original_type = TArr (TVar "T1", TArr (TVar "T2", TNat)) in (* T1 -> (T2 -> Nat) *)
  let var_to_substitute = "T1" in (* On veut substituer T1 par Nat *)
  let replacement = TNat in 
  let expected_type = TArr (TNat, TArr (TVar "T2", TNat)) in
  let substituted_type = subtitute_type_into_type var_to_substitute replacement original_type in
  assert_equal ~msg:"Substitution incorrecte" expected_type substituted_type

(* Un autre test de substitution*)
let test_substitution_complex _ =
  let original_type = TArr (TVar "T1", TArr (TVar "T2", TArr (TVar "T3", TNat))) in (* T1 -> (T2 -> (T3 -> Nat)) *)
  let var_to_substitute = "T2" in (* On veut substituer T2 par (T4 -> T5) *)
  let replacement = TArr (TVar "T4", TVar "T5") in
  let expected_type = TArr (TVar "T1", TArr (TArr (TVar "T4", TVar "T5"), TArr (TVar "T3", TNat))) in
  let substituted_type = subtitute_type_into_type var_to_substitute replacement original_type in
  assert_equal ~msg:"Substitution complexe incorrecte" expected_type substituted_type

let test_substitution_into_equa _ =
  let original_equa = [
    (TVar "T1", TVar "T2");
    (TArr (TVar "T1", TVar "T2"), TArr (TVar "T3", TVar "T4"))
  ] in
  let var_to_substitute = "T1" in
  let replacement = TVar "T5" in
  let expected_equa = [
    (TVar "T5", TVar "T2");
    (TArr (TVar "T5", TVar "T2"), TArr (TVar "T3", TVar "T4"))
  ] in
  let substituted_equa = subtitute_type_into_equa var_to_substitute replacement original_equa in
  assert_equal ~msg:"Substitution dans l'équation incorrecte" expected_equa substituted_equa

(* ----------------- Tests d'inférence de type ----------------- *)

(* Fonction de test pour l'inférence de type de l'identité *)
let test_infer_identity _ =
  let term = Abs ("x", Var "x") in (* λx.x *) (* type attendu : TVar "T1" -> TVar "T1" *)
  let inferred_type = infer_type term [] in
  match inferred_type with
  | TArr (t1, t2) ->
      print_endline (print_type t1);
      print_endline (print_type t2);
      let expected_type = TArr (TVar "T1", TVar "T1") in
      assert_bool "Type inféré incorrect" (types_alpha_equal (TArr (t1, t2)) expected_type)
  | _ -> assert_failure "Le type inféré n'est pas une fonction"

(* Fonction de test pour l'inférence de type de la fonction constante *)
let test_infer_constant _ =
  let term = Abs ("x", Abs ("y", Var "x")) in (* λx.λy.x *) (* type attendu : TVar "T1" -> TVar "T2" -> TVar "T1" *)
  let inferred_type = infer_type term [] in
  match inferred_type with
  | TArr (t1, TArr (t2, t3)) ->
      (* Vérifier que t1 = t3 *)
      assert_equal t1 t3 ~msg:"Le type de la fonction constante devrait être T1 -> T2 -> T1"
  | _ ->
      assert_failure "Le type de la fonction constante n'est pas correctement inféré"

(* Fonction de test pour l'inférence de type de la fonction composée *)
let test_infer_nested_abs _ =
  let term = Abs ("x", Abs ("y", App (Var "x", Var "y"))) in (* λx. λy. x y *)
  let inferred_type = infer_type term [] in
  let expected_type = TArr (TArr (TVar "T1", TVar "T2"), TArr (TVar "T1", TVar "T2")) in
  print_endline ("Type inféré : " ^ (print_type inferred_type));
  print_endline ("Type attendu : " ^ (print_type expected_type));
  assert_bool "Type inféré incorrect" (types_alpha_equal inferred_type expected_type)

(* Fonction de test pour l'inférence de type de l'application *)
let test_infer_application _ =
  let env = [("f", TArr (TVar "T1", TVar "T2")); ("x", TVar "T1")] in (*  f : T1 -> T2, x : T1 *)
  let term = App (Var "f", Var "x") in (* f x *)
  let inferred_type = infer_type term env in
  let expected_type = TVar "T2" in
  print_endline ("Type inféré : " ^ (print_type inferred_type));
  print_endline ("Type attendu : " ^ (print_type expected_type));
  assert_bool "Type inféré incorrect" (types_alpha_equal inferred_type expected_type)

(* Fonction de test pour l'application complexe *)
let test_infer_application_complex _ =
  let env = [
    ("f", TArr (TVar "T1", TVar "T2")); (* f : T1 -> T2 *)
    ("g", TArr (TVar "T3", TVar "T1")); (* g : T3 -> T1 *)
    ("x", TVar "T3") (* x : T3 *)
  ] in
  let term = App (Var "f", App (Var "g", Var "x")) in (* f (g x) *)
  let inferred_type = infer_type term env in
  let expected_type = TVar "T2" in
  print_endline ("Type inféré test_comp : " ^ (print_type inferred_type));
  print_endline ("Type attendu test_comp: " ^ (print_type expected_type));
  assert_bool "Type inféré incorrect" (types_alpha_equal inferred_type expected_type)

(* Fonction de test pour l'inférence de type de delta_term (λx. x x) *)
let test_infer_delta _ =
  let term = delta_term in (* λx. x x *)
  assert_raises
    (Unification_Failed "Unification failed")
    (fun () -> infer_type term [])

(* Fonction de test pour l'inférence de type d'omega_term ((λx. x x) (λx. x x)) *)
let test_infer_omega _ =
  let term = omega_term in (* (λx. x x) (λx. x x) *)
  assert_raises
    (Unification_Failed "Unification failed")
    (fun () -> infer_type term [])


(* Ajout des nouveaux tests à la suite de tests *)
let substitution_tests =
  "Substitution Tests" >::: [
    "test_substitution" >:: test_substitution;
    "test_substitution_complex" >:: test_substitution_complex;
    "test_substitution_into_equa" >:: test_substitution_into_equa;
    ]


(* Suite de tests pour l'inférence de type *)
let inference_tests =
  "Typing Inference Tests" >::: [
    "test_infer_identity" >:: test_infer_identity;
    "test_infer_constant" >:: test_infer_constant;
    "test_infer_nested_abs" >:: test_infer_nested_abs;
    "test_infer_application" >:: test_infer_application;
    "test_infer_application_complex" >:: test_infer_application_complex;
    "test_infer_delta" >:: test_infer_delta;
    "test_infer_omega" >:: test_infer_omega;
    ] 

let unification_tests =
  "Unification Tests" >::: [
    "test_unification_equation_simple" >:: test_unification_equation_simple;
    "test_unification_equation_complex" >:: test_unification_equation_complex;
    "test_unification_failure" >:: test_unification_failure;
    "test_unification_occurs_check" >:: test_unification_occurs_check;
    "test_unification_multiple_substitutions" >:: test_unification_multiple_substitutions;
    ]

(* Combine les suites de tests *)
let combined_suites =
  "All Tests" >::: [
    inference_tests;
    unification_tests;
    substitution_tests;
  ]

(* Exécution des tests *)
let () =
  run_test_tt_main combined_suites
