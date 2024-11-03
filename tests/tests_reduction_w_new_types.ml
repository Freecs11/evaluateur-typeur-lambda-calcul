open OUnit2
open Lambda_ast
open Reduction


(* Test addition simple *)
let test_addition_simple _ctxt =
  let term = Add (Int 2, Int 3) in
  let result = ltr_cbv_norm term in
  let expected = Int 5 in
  print_endline ("Add (2, 3): " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Addition did not normalize to 5" (alpha_equal result expected)

(* Test addition avec variable *)
let test_addition_with_variable _ctxt =
  let term = Add (Int 2, Var "x") in
  let result = ltr_cbv_norm term in
  let expected = Add (Int 2, Var "x") in
  print_endline ("Add (2, x): " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool  "Addition did not normalize to itself" (alpha_equal result expected)

(* Test soustraction simple *)
let test_subtraction_simple _ctxt =
  let term = Sub (Int 10, Int 4) in
  let result = ltr_cbv_norm term in
  let expected = Int 6 in
  print_endline ("Sub (10, 4): " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Subtraction did not normalize to 6" (alpha_equal result expected)

(* Test soustraction avec variable *)
let test_subtraction_with_variable _ctxt =
  let term = Sub (Var "x", Int 3) in
  let result = ltr_cbv_norm term in
  let expected = Sub (Var "x", Int 3) in
  print_endline ("Sub (x, 3): " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Subtraction did not normalize to itself" (alpha_equal result expected)

(* Test application avec addition *)
let test_application_add _ctxt =
  let term = App (Abs ("x", Add (Var "x", Int 2)), Int 3) in
  let result = ltr_cbv_norm term in
  let expected = Int 5 in
  print_endline ("(fun x -> x + 2) 3: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Application did not normalize to 5" (alpha_equal result expected)

(* Test application non totalement réductible *)
let test_application_non_reductible _ctxt =
  let term = App (Abs ("x", Add (Var "x", Int 2)), Var "y") in
  let result = ltr_cbv_norm term in
  let expected = Add (Var "y", Int 2) in
  print_endline ("(fun x -> x + 2) y: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Application did not normalize to itself" (alpha_equal result expected)

(* Test substitution dans une abstraction *)
let test_substitution_in_abs _ctxt =
  let term = Abs ("x", Add (Var "x", Var "y")) in
  let substituted = substitution "y" (Int 5) term in
  let reduced = alphaconv substituted |> ltr_cbv_norm in
  let expected = Abs ("x", Add (Var "x", Int 5)) in
  print_endline ("Substituting y with 5 in (fun x -> x + y): " ^ print_term substituted ^ " ::: => " ^ print_term reduced);
  assert_bool "Substitution did not normalize to (fun x -> x + 5)" (alpha_equal reduced expected)

  (* Test composition de fonctions 
  compose = λf. λg. λx. f (g x)
  increment = λx. x + 1
  double = λx. x + x
  term = compose increment double 2
  ==> (2 * 2) + 1 = 5
  *)
  let test_function_composition _ctxt =
    let compose = Abs ("f", Abs ("g", Abs ("x", 
      App (Var "f", App (Var "g", Var "x"))
    ))) in
    let increment = Abs ("x", Add (Var "x", Int 1)) in
    let double = Abs ("x", Add (Var "x", Var "x")) in
    let term = App (App (App (compose, increment), double), Int 2) in
    let result = ltr_cbv_norm term in
    let expected = Int 5 in 
    assert_bool "Function composition failed" (alpha_equal result expected)

(* Tests pour les Bindings Let *)

(* Test d'un binding let simple *)
let test_let_simple _ctxt =
  let term = Let ("x", Int 5, Add (Var "x", Int 3)) in
  let result = ltr_cbv_norm term in
  let expected = Int 8 in
  print_endline ("Let x = 5 in x + 3: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Let did not normalize to 8" (alpha_equal result expected)


(* Test d'un binding let avec une variable *)
let test_let_with_variable _ctxt =
  let term = Let ("x", Int 10, Sub (Var "x", Int 4)) in
  let result = ltr_cbv_norm term in
  let expected = Int 6 in
  print_endline ("Let x = 10 in x - 4: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Let did not normalize to 6" (alpha_equal result expected)

(* Test d'un binding let imbriqué *)
let test_let_nested _ctxt =
  let term = Let ("x", Int 2, Let ("y", Add (Var "x", Int 3), Sub (Var "y", Int 1))) in
  let result = ltr_cbv_norm term in
  let expected = Int 4 in
  print_endline ("Let x = 2 in Let y = x + 3 in y - 1: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Let did not normalize to 4" (alpha_equal result expected)

(* Tests pour les Listes *)

(* Test de création d'une liste simple *)
let test_create_list _ctxt =
  let term = Cons (Int 1, Cons (Int 2, Cons (Int 3, Nil))) in
  let result = ltr_cbv_norm term in
  let expected = Cons (Int 1, Cons (Int 2, Cons (Int 3, Nil))) in
  print_endline ("Cons(1, Cons(2, Cons(3, Nil))): " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "List did not normalize to itself" (alpha_equal result expected)
  

(* Test d'accès au head d'une liste *)
let test_head_of_list _ctxt =
  let list = Cons (Int 10, Cons (Int 20, Nil)) in
  let term = Head list in
  let result = ltr_cbv_norm term in
  let expected = Int 10 in
  print_endline ("Head of " ^ print_term list ^ ": " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Head did not normalize to 10" (alpha_equal result expected)

(* Test d'accès au tail d'une liste *)
let test_tail_of_list _ctxt =
  let list = Cons (Int 5, Cons (Int 15, Nil)) in
  let term = Tail list in
  let result = ltr_cbv_norm term in
  let expected = Cons (Int 15, Nil) in
  print_endline ("Tail of " ^ print_term list ^ ": " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Tail did not normalize to Cons(15, Nil)" (alpha_equal result expected)

(* Test de Head sur une liste vide (Nil) *)
let test_head_of_nil _ctxt =
  let term = Head Nil in
  let result = ltr_cbv_norm term in
  let expected = Head Nil in
  print_endline ("Head of Nil: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Head did not normalize to itself" (alpha_equal result expected)

(* Test de Tail sur une liste vide (Nil) *)
let test_tail_of_nil _ctxt =
  let term = Tail Nil in
  let result = ltr_cbv_norm term in
  let expected = Tail Nil in
  print_endline ("Tail of Nil: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Tail did not normalize to itself" (alpha_equal result expected)

let test_let_binding_with_list _ctxt =
  let term = Let ("x", Cons (Int 1, Cons (Int 2, Nil)), Head (Var "x")) in
  let result = ltr_cbv_norm term in
  let expected = Int 1 in
  print_endline ("Let x = Cons(1, Cons(2, Nil)) in Head x: " ^ print_term term ^ "  ::: =>  " ^ print_term result);
  assert_bool "Let did not normalize to 1" (alpha_equal result expected)


  (* Tests pour Fix *)
let test_fix_simple _ctxt =
  let term = Fix ("f", Abs ("x", Var "x")) in
  let result = ltr_cbv_norm term in
  let expected = Abs ("x", Var "x") in
  print_endline ("Fix f = (fun x -> x): " ^ print_term term ^ " ::: => " ^ print_term result);
  assert_bool "Fix did not normalize correctly" (alpha_equal result expected)

(* Test pour le calcul de la factorielle avec fix*)
let test_fix_recursive_factorial _ctxt =
  (* Définition de la multiplication par additions répétées 
    mult = λx. λy. if y = 0 then 0 else x + mult x (y - 1)
  *)
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
  
  (* Définition du factoriel utilisant multiply 
    factorial = fix f. λn. if n = 0 then 1 else n * f (n - 1)
  *)
  let term = Fix ("factorial", Abs ("n",
    IfZero (Var "n",
      Int 1,
      App (
        App (multiply, Var "n"),
        App (Var "factorial", Sub (Var "n", Int 1))
      )
    )
  )) in
  
  let applied = App (term, Int 5) in
  let result = ltr_cbv_norm applied in
  let expected = Int 120 in
  print_endline ("Factorial 5: " ^ print_term applied ^ " ::: => " ^ print_term result);
  assert_bool "Fix factorial did not compute correctly" (alpha_equal result expected)

(* Tests supplémentaires pour IfZero 
  if zero (5-5) then 1 else 0
*)
let test_ifzero_complex _ctxt =
  let term = IfZero (Sub (Int 5, Int 5), Int 1, Int 0) in
  let result = ltr_cbv_norm term in
  let expected = Int 1 in
  print_endline ("IfZero (5-5) then 1 else 0: " ^ print_term term ^ " ::: => " ^ print_term result);
  assert_bool "IfZero with computation did not evaluate correctly" (alpha_equal result expected)

(* Tests supplémentaires pour IfEmpty
  if empty (tail [1]) then 0 else 1
  ==> empty (tail [1]) = empty [] = true
  donc le résultat doit être 0
*)
let test_ifempty_complex _ctxt =
  let term = IfEmpty (
    Tail (Cons (Int 1, Nil)),
    Int 0,
    Int 1
  ) in
  let result = ltr_cbv_norm term in
  let expected = Int 0 in
  print_endline ("IfEmpty (tail [1]) then 0 else 1: " ^ print_term term ^ " ::: => " ^ print_term result);
  assert_bool "IfEmpty with tail computation did not evaluate correctly" (alpha_equal result expected)

(* Tests pour les combinaisons de constructeurs 
  let x = IfZero 0 then Cons 1 Nil else Nil in IfEmpty x then 0 else Head x
  ==> ce qui doit donner 1
*)
let test_complex_combination _ctxt =
  let term = Let ("x",
    IfZero (Int 0,
      Cons (Int 1, Nil),
      Nil
    ),
    IfEmpty (Var "x",
      Int 0,
      Head (Var "x")
    )
  ) in
  let result = ltr_cbv_norm term in
  let expected = Int 1 in
  print_endline ("Complex combination: " ^ print_term term ^ " ::: => " ^ print_term result);
  assert_bool "Complex combination did not evaluate correctly" (alpha_equal result expected)

(* Test fonction polymorphe : map 
  map = fix map. λf. λlst. if empty lst then [] else f (head lst) :: map f (tail lst)
*)
let test_polymorphic_map _ctxt =
  let map = Fix ("map", Abs ("f", Abs ("lst",
    IfEmpty (Var "lst",
      Nil,
      Cons (
        App (Var "f", Head (Var "lst")),
        App (App (Var "map", Var "f"), Tail (Var "lst"))
      )
    )
  ))) in
  (* Test avec +1 sur une liste *)
  let increment = Abs ("x", Add (Var "x", Int 1)) in
  let input_list = Cons (Int 1, Cons (Int 2, Nil)) in
  let term = App (App (map, increment), input_list) in
  let result = ltr_cbv_norm term in
  let expected = Cons (Int 2, Cons (Int 3, Nil)) in
  print_endline ("Map (+1) [1, 2]: " ^ print_term term ^ " ::: => " ^ print_term result);
  assert_bool "Map didn't work correctly" (alpha_equal result expected)

(* Test fonction polymorphe : fold 
  fold = fix fold. λf. λacc. λlst. if empty lst then acc else f (head lst) (fold f acc (tail lst))
*)
let test_polymorphic_fold _ctxt =
  let fold = Fix ("fold", Abs ("f", Abs ("acc", Abs ("lst",
    IfEmpty (Var "lst",
      Var "acc",
      App (
        App (Var "f", Head (Var "lst")),
        App (App (App (Var "fold", Var "f"), Var "acc"), Tail (Var "lst"))
      )
    )
  )))) in
  (* Test avec sum sur une liste *)
  let add = Abs ("x", Abs ("y", Add (Var "x", Var "y"))) in
  let input_list = Cons (Int 1, Cons (Int 2, Cons (Int 3, Nil))) in
  let term = App (App (App (fold, add), Int 0), input_list) in
  let result = ltr_cbv_norm term in
  let expected = Int 6 in
  print_endline ("Fold (+) 0 [1, 2, 3]: " ^ print_term term ^ " ::: => " ^ print_term result);
  assert_bool "Fold didn't sum correctly" (alpha_equal result expected)




(* Ajout des nouveaux tests à la suite *)
let new_operator_tests =
  "New Operator Tests" >::: [
    "test_fix_simple" >:: test_fix_simple;
    "test_fix_recursive" >:: test_fix_recursive_factorial;
    "test_ifzero_complex" >:: test_ifzero_complex;
    "test_ifempty_complex" >:: test_ifempty_complex;
    "test_complex_combination" >:: test_complex_combination;
    "test_polymorphic_map" >:: test_polymorphic_map;
    "test_polymorphic_fold" >:: test_polymorphic_fold;
  ]

let reduction_let_tests =
  "Let Binding Tests" >::: [
    "test_let_simple" >:: test_let_simple;
    "test_let_with_variable" >:: test_let_with_variable;
    "test_let_nested" >:: test_let_nested;
    "test_let_binding_with_list" >:: test_let_binding_with_list;
  ]

let reduction_list_tests =
  "List Reduction Tests" >::: [
    "test_create_list" >:: test_create_list;
    "test_head_of_list" >:: test_head_of_list;
    "test_tail_of_list" >:: test_tail_of_list;
    "test_head_of_nil" >:: test_head_of_nil;
    "test_tail_of_nil" >:: test_tail_of_nil;
  ]

let reduction_tests =
  "Reduction Tests" >::: [
    "test_addition_simple" >:: test_addition_simple;
    "test_addition_with_variable" >:: test_addition_with_variable;
    "test_subtraction_simple" >:: test_subtraction_simple;
    "test_subtraction_with_variable" >:: test_subtraction_with_variable;
    "test_application_add" >:: test_application_add;
    "test_application_non_reductible" >:: test_application_non_reductible;
    "test_substitution_in_abs" >:: test_substitution_in_abs;
    "test_function_composition" >:: test_function_composition;
  ]

  (* Combine the test suites *)
let combined_suites =
  "All Tests" >::: [
    reduction_tests;
    reduction_list_tests;
    reduction_let_tests;
    new_operator_tests;
  ]


  (* Exécution des tests *)
let () =
  run_test_tt_main combined_suites