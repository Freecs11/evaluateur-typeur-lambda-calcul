open OUnit2
open Lambda_ast
open Reduction

(* Lambda Terms *)
let i_term = Abs ("x", Var "x")  (* I: λx. x *)

let delta_term = Abs ("x", App (Var "x", Var "x"))  (* δ: λx. x x *)

let omega_term = App (delta_term, delta_term)  (* Ω: (λx. x x) (λx. x x), diverges *)

let k_term = Abs ("x", Abs ("y", Var "x"))  (* K: λx. λy. x *)

let s_term = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))  (* S: λx. λy. λz. x z (y z) *)

let s_k_k_term = App (App (s_term, k_term), k_term)  (* S K K simplifies to I *)

let s_i_i_term = App (App (s_term, i_term), i_term)  (* S I I simplifies to I *)


(* Church Numbers *)
let zero = Abs ("f", Abs ("x", Var "x"))  (* 0: λf. λx. x *)

let one = Abs ("f", Abs ("x", App (Var "f", Var "x")))  (* 1: λf. λx. f x *)

let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))))  (* 2: λf. λx. f (f x) *)

let three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))  (* 3: λf. λx. f (f (f x)) *)

(* Arithmetic operations *)

(* successor *)
let succ_term = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x"))))) (* succ: λn. λf. λx. f (n f x) *)

(* addition  add = λnmfe.n f (m f e) (addition) *)
let add_term =  Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))



(* Test ltr_cbv_step function *)
let test_ltr_cbv_step _ =
  let term1 = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in (* (λx. x) (λy. y) *)
  let term2 = App (Abs ("x", Var "x"), Var "z") in (* (λx. x) z *)
  let term3 = App (Abs ("x", Var "x"), Abs ("y", App (Var "x", Var "y"))) in (* (λx. x) (λy. x y) *)
  let result1 = ltr_cbv_step term1 in
  let result2 = ltr_cbv_step term2 in
  let result3 = ltr_cbv_step term3 in
  assert_equal (Some (Abs ("y", Var "y"))) result1;
  assert_equal None result2; (* z is a variable *)
  assert_equal (Some (Abs ("y", App (Var "x", Var "y"))) ) result3

(* Test ltr_cbv_norm function *)
let test_ltr_cbv_norm _ =
  let term = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", Var "y")) in
  let result = ltr_cbv_norm term in
  print_endline ("Term: " ^ print_term term);
  assert_equal (Abs ("y", Var "y")) result

(* Test function *)
let test_ltr_cbv_norm_with_timeout _ctxt =
  let term_divergent = App (Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x"))) in
  let term_convergent = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in

  let result_divergent = ltr_cbv_norm_with_timeout term_divergent 5 in
  let result_convergent = ltr_cbv_norm_with_timeout term_convergent 5 in
  
  (* the divergent term should return None *)
  print_endline ("Divergent term: " ^ print_term term_divergent); 
  assert_equal None result_divergent;
  
  print_endline ("Convergent term: " ^ print_term term_convergent);
  print_endline ("Result: " ^ print_term (Option.get result_convergent));

  match result_convergent with
  | Some term ->
      assert_bool
        "Convergent term did not normalize to expected value"
        (alpha_equal term (Abs ("y", Var "y")))
  | None -> assert_failure "Convergent term should not return None"



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

(* Test of S K K normalisation , cbv so it results in fun z -> (K z) (K z) *)
let test_s_k_k_term_normalization _ctxt =
  let result = ltr_cbv_norm s_k_k_term in
  (* TODO: check if cbv is expected to be strict like this *)
  let expected =  Abs ("z", App (App (k_term, Var "z"), App (k_term, Var "z"))) in
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
  (* Simplify the term *)
  let simplified_result = ltr_cbv_norm succ_zero_term in
  let expected = one in
  (* Debugging output *)
  print_endline ("Expected (1): " ^ print_term expected);
  print_endline ("Result after simplification: " ^ print_term simplified_result);
  
  (* Assert the result matches expected (Church numeral for 1) *)
  assert_bool "succ 0 did not normalize to 1" (alpha_equal simplified_result expected)


(* A revoir et corriger 
TODO: Fix the simplification of normalization ( and check if it's even necessary to simplify the result) *)

(* Test for addition 1 1 normalization *)
let test_addition_1_1_normalization _ctxt =
  let addition_1_1_term = App (App (add_term, one), one) in
  (* Simplify the term *)
  let simplified_result = ltr_cbv_norm addition_1_1_term in
  let expected = two in
  (* Debugging output *)
  print_endline ("Expected (2): " ^ print_term expected ^ "Result after simplification: " ^ print_term simplified_result);
  
  (* Assert the result matches expected (Church numeral for 2) *)
  assert_bool "addition 1 1 did not normalize to 2" (alpha_equal simplified_result expected)



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
    
    (* "test_addition_1_1_normalization" >:: test_addition_1_1_normalization; *)
  ]


(* Test suite *)

(* LtR-CbV Tests *)
let ltr_cbv_tests =
  "LtR-CbV Tests" >::: [
    "test_ltr_cbv_step" >:: test_ltr_cbv_step;
    "test_ltr_cbv_norm" >:: test_ltr_cbv_norm;
    (* Uncomment if you want to test with timeout *)
    "test_ltr_cbv_norm_with_timeout" >:: test_ltr_cbv_norm_with_timeout;
  ]

(* Combine the test suites *)
let combined_suites =
  "All Tests" >::: [
    ltr_cbv_tests;
    normalization_tests;
  ]

(* Run the combined test suite *)
let () =
  run_test_tt_main combined_suites