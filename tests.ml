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
let succ_term = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))  (* Successor function *)

let add_term = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))  (* Addition function *)

let mul_term = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))  (* Multiplication function *)


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
  assert_equal (Abs ("y", Var "y")) result

(* Test function *)
let test_ltr_cbv_norm_with_timeout _ctxt =
  let term_divergent = App (Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x"))) in
  let term_convergent = App (Abs ("x", Var "x"), Abs ("y", Var "y")) in

  let result_divergent = ltr_cbv_norm_with_timeout term_divergent 5 in
  let result_convergent = ltr_cbv_norm_with_timeout term_convergent 5 in

  assert_equal None result_divergent;
  match result_convergent with
  | Some term ->
      assert_bool
        "Convergent term did not normalize to expected value"
        (alpha_equal term (Abs ("y", Var "y")))
  | None -> assert_failure "Convergent term should not return None"


(* Test combinators *)


(* Test S I I under CbV *)
let test_s_i_i_normalization_cbv _ctxt =
  let s_i_i_term_normalisation = ltr_cbv_norm s_i_i_term in
  let s_i_i_term_norm_result = Abs ("z", App (App (i_term, Var "z"), App (i_term, Var "z"))) in
  print_endline ("S I I: " ^ print_term s_i_i_term);
  print_endline ("Result: " ^ print_term s_i_i_term_normalisation);
  assert_bool
    "S I I did not normalize to the expected result under Call-by-Value"
    (alpha_equal s_i_i_term_normalisation s_i_i_term_norm_result)
  


(* Test suite *)
let normalization_tests =
  "Normalization Tests" >::: [
    "test_s_i_i_normalization" >:: test_s_i_i_normalization_cbv;
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
    (* ltr_cbv_tests; *)
    normalization_tests;
  ]

(* Run the combined test suite *)
let () =
  run_test_tt_main combined_suites