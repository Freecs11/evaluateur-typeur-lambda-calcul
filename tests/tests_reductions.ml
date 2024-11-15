open OUnit2
open Lambda_ast
open Reduction
open Terms

(* Test ltr_cbv_step function *)
let test_ltr_cbv_step _ =
  let term1 = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in (* (λx. x) (λy. y) *)
  let term2 = App (Abs ("x", Var "x"), Var "z") in (* (λx. x) z *)
  let term3 = App (Abs ("x", Var "x"), Abs ("y", App (Var "x", Var "y"))) in (* (λx. x) (λy. x y) *)
  let term4 = Var "x" in (* x *) 
  let result1 = ltr_cbv_step term1 in
  let result2 = ltr_cbv_step term2 in
  let result3 = ltr_cbv_step term3 in
  let result4 = ltr_cbv_step term4 in

  (*print_endline ("Term1 res in test_ltr_cbv_step: " ^ print_term (Option.get result1));
  print_endline ("Term2 res in test_ltr_cbv_step: " ^ print_term (Option.get result2));
  print_endline ("Term3 res in test_ltr_cbv_step: " ^ print_term (Option.get result3));
   *)

  assert_equal (Some (Abs ("y", Var "y"))) result1;
  assert_equal (Some (Var "z")) result2;
  assert_equal (Some (Abs ("y", App (Var "x", Var "y"))) ) result3;
  assert_equal None result4  (* variable, should return None *)

(* Test ltr_cbv_norm function *)
let test_ltr_cbv_norm _ =
  let term = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", Var "y")) in
  let result = ltr_cbv_norm term in
  print_endline ("Term: " ^ print_term term);
  assert_equal (Abs ("y", Var "y")) result

(* Test function *)
let test_ltr_cbv_norm_with_timeout _ctxt =
  let term_divergent = omega_term in
  let term_convergent = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in

  let result_divergent = ltr_cbv_norm_with_timeout term_divergent 5 in
  let result_convergent = ltr_cbv_norm_with_timeout term_convergent 5 in
  
  (* the divergent term should return None *)
  print_endline ("Divergent term: " ^ print_term term_divergent); 
  assert_equal None result_divergent;
  
  print_endline ("Convergent term: " ^ print_term term_convergent);
  print_endline ("Result of convergent term: " ^ print_term (Option.get result_convergent));
  assert_equal (Some (Abs ("y", Var "y"))) result_convergent


(* Test combinators *)

(* Test I normalisation *)
let test_i_term_normalization _ctxt =
  let result = ltr_cbv_norm i_term in
  let expected = i_term in
  print_endline ("I: " ^ print_term i_term ^ "  ::: =>  " ^ print_term result);
  assert_bool "I did not normalize to itself" (alpha_equal result expected)


(* Test S I I under CbV *)
let test_s_i_i_normalization_cbv _ctxt =
  let s_i_i_term_normalisation = ltr_cbv_norm s_i_i_term in
  let s_i_i_term_norm_result =  Abs ("z", App (Var "z", Var "z")) in
  print_endline ("S I I: " ^ print_term s_i_i_term ^ "  ::: =>  " ^ print_term s_i_i_term_normalisation);
  print_endline ("S I I Expected: " ^ print_term s_i_i_term_norm_result);
  assert_bool
    "S I I did not normalize to the expected result under Call-by-Value"
    (alpha_equal s_i_i_term_normalisation s_i_i_term_norm_result)
  
(* Test of delta normalisation *)
let test_delta_term_normalization _ctxt =
  let result = ltr_cbv_norm delta_term in
  let expected = delta_term in
  print_endline ("δ: " ^ print_term delta_term);
  assert_bool "δ did not normalize to itself" (alpha_equal result expected)

(* Test of omega divergence , infinitely loops with ltr_cbv_norm *)
let test_omega_term_divergence _ctxt =
  let result = ltr_cbv_norm_with_timeout omega_term 100 in  
  print_endline ("Ω: " ^ print_term omega_term);
  assert_bool "Ω should not normalize (diverges)" (result = None)

(* Test of S K K normalisation *)
let test_s_k_k_term_normalization _ctxt =
  let result = ltr_cbv_norm s_k_k_term in
  (* TODO: check if cbv is expected to be strict like this *)
  let expected = i_term in
  print_endline ("S K K: " ^ print_term s_k_k_term ^ "  ::: =>  " ^ print_term result);
  assert_bool "S K K did not normalize to I" (alpha_equal result expected)


(* Test of zero normalisation , remains zero *)
let test_zero_term_normalization _ctxt =
  let result = ltr_cbv_norm zero in
  let expected = zero in
  print_endline ("0: " ^ print_term zero ^ "  ::: =>  " ^ print_term result);
  assert_bool "0 did not normalize to itself" (alpha_equal result expected)


(* Test for succ 0 normalization *)
let test_succ_zero_normalization _ctxt =
  let succ_zero_term = App (succ_term, zero) in
  let result = ltr_cbv_norm succ_zero_term in
  let expected = one in
  print_endline ("LOOOG succ 0: " ^ print_term succ_zero_term ^ "  ::: =>  " ^ print_term result);
  let result_int = church_to_int result in
  let expected_int = church_to_int expected in
  
  assert_equal expected_int result_int ~msg:"succ 0 did not normalize to 1"


(* Test for addition 1 1 normalization *)
let test_addition_1_1_normalization _ctxt =
  let addition_1_1_term = App (App (add_term, one), one) in
  let result = ltr_cbv_norm addition_1_1_term in
  let expected = two in
  let result_int = church_to_int result in
  let expected_int = church_to_int expected in
  assert_equal expected_int result_int ~msg:"addition 1 1 did not normalize to 2"

(* Test for succ 3 normalization *)
let test_succ_3_normalization _ctxt =
  let succ_3_term = App (succ_term, three) in
  let result = ltr_cbv_norm succ_3_term in
  let expected = four in
  let result_int = church_to_int result in
  let expected_int = church_to_int expected in
  assert_equal expected_int result_int ~msg:"succ 3 did not normalize to 4"
  
(* Test for S normalisation *)
let test_s_term_normalization _ctxt =
  let result = ltr_cbv_norm s_term in
  let expected = s_term in
  print_endline ("S: " ^ print_term s_term ^ "  ::: =>  " ^ print_term result);
  assert_bool "S did not normalize to itself" (alpha_equal result expected)



(* Test de la création de référence *)
let test_ref_term_normalization _ =
  Hashtbl.reset region_state;  (* Réinitialisation de l'état *)
  let term = Ref (Int 42) in
  (* Après réduction, on devrait obtenir une région *)
  let result = ltr_cbv_norm term in
  match result with
  | Region id ->
      (* Vérifier que la région contient bien la valeur 42 *)
      let value_in_region = Hashtbl.find region_state id in
      Printf.printf "Ref test result: %s\n" (print_term value_in_region);
      assert_equal ~printer:print_term (Int 42) value_in_region
  | _ -> assert_failure "Ref n'a pas été réduit en une région"

let test_ref_complex_term_normalization _ =
  Hashtbl.reset region_state; 
  let term =
    Let ("r", Ref (Int 42),
      Var "r"
    ) in
  let result = ltr_cbv_norm term in
  match result with
  | Region id ->
      (* Vérifier que la région contient bien la valeur 42 *)
      let value_in_region = Hashtbl.find region_state id in
      Printf.printf "Ref complex test result: %s\n" (print_term value_in_region);
      assert_equal ~printer:print_term (Int 42) value_in_region
  | _ -> assert_failure "Ref complex n'a pas été réduit en une région"

(* Test du déréférencement *)
let test_deref_term_normalization _ =
  Hashtbl.reset region_state;  
  let term = Deref (Ref (Int 42)) in (* !ref 42 *)
  let result = ltr_cbv_norm term in
  Printf.printf "Deref test result: %s\n" (print_term result);
  assert_equal ~printer:print_term (Int 42) result

(* Test du déréférencement d'un terme complexe, on fait un let r = ref 42 in !r *)
let test_deref_complex_term_normalization _ =
  Hashtbl.reset region_state;  
  let term =
    Let ("r", Ref (Int 42),
      Deref (Var "r")
    ) in
  let result = ltr_cbv_norm term in
  Printf.printf "Deref complex test result: %s\n" (print_term result);
  assert_equal ~printer:print_term (Int 42) result

(* Test de l'assignation *)
let test_assign_term_normalization _ =
  Hashtbl.reset region_state;  
  let term =
    Let ("r", Ref (Int 0),
      Let ("_", Assign (Var "r", Int 42),
        Deref (Var "r")
      )
    ) in
  let result = ltr_cbv_norm term in
  Printf.printf "Assign test result: %s\n" (print_term result);
  assert_equal ~printer:print_term (Int 42) result

(* Test de l'assignation, on fait 2 assignations du même emplacement *)
let test_assign_complex_term_normalization _ =
  Hashtbl.reset region_state;  
  let term =
    Let ("r", Ref (Int 0),
      Let ("_", Assign (Var "r", Int 42),
        Let ("_", Assign (Var "r", Int 43),
          Deref (Var "r")
        )
      )
    ) in
  let result = ltr_cbv_norm term in
  Printf.printf "Assign complex test result: %s\n" (print_term result);
  assert_equal ~printer:print_term (Int 43) result

(* Test suite *)

let imperative_traits_tests =
  "Imperative Traits Tests" >::: [
    "test_ref_term_normalization" >:: test_ref_term_normalization;
    "test_ref_complex_term_normalization" >:: test_ref_complex_term_normalization;
    "test_deref_term_normalization" >:: test_deref_term_normalization;
    "test_deref_complex_term_normalization" >:: test_deref_complex_term_normalization;
    "test_assign_term_normalization" >:: test_assign_term_normalization;
    "test_assign_complex_term_normalization" >:: test_assign_complex_term_normalization;
  ]

(* Test suite *)
let normalization_tests =
  "Normalization Tests" >::: [
    "test_s_i_i_normalization" >:: test_s_i_i_normalization_cbv;
    "test_delta_term_normalization" >:: test_delta_term_normalization;
    "test_i_term_normalization" >:: test_i_term_normalization;
    "test_omega_term_divergence" >:: test_omega_term_divergence;
    "test_s_k_k_term_normalization" >:: test_s_k_k_term_normalization;
    "test_zero_term_normalization" >:: test_zero_term_normalization;
    "test_succ_zero_normalization" >:: test_succ_zero_normalization;
    "test_addition_1_1_normalization" >:: test_addition_1_1_normalization;
    "test_succ_3_normalization" >:: test_succ_3_normalization;
    "test_s_term_normalization" >:: test_s_term_normalization;
  ]


(* Test suite *)

(* LtR-CbV Tests *)
let ltr_cbv_tests =
  "LtR-CbV Tests" >::: [
    "test_ltr_cbv_step" >:: test_ltr_cbv_step;
    "test_ltr_cbv_norm" >:: test_ltr_cbv_norm;
    "test_ltr_cbv_norm_with_timeout" >:: test_ltr_cbv_norm_with_timeout;
  ]

(* Combine the test suites *)
let combined_suites =
  "All Tests" >::: [
    ltr_cbv_tests;
    normalization_tests;
    imperative_traits_tests;
  ]

(* Run the combined test suite *)
let () =
  run_test_tt_main combined_suites