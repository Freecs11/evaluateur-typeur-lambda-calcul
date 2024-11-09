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
    (Unification_Failed "Unification failed: different constructors")
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
  (* je ne comprend pas si le type de Head est Forall("alpha", TArr(TList(TVar("alpha")), TVar("alpha"))) 
    ou alors le c'est simplement le type de la liste , j'ai donc juste pris le type de la liste*)
  (* let expected =   TForall ("alpha", TArr (TList (TNat), TNat)) in *)
  let expected = TNat in
  print_endline ("Type inféré pour Head: " ^ print_type infer_typered);
  print_endline ("Type attendu pour Head: " ^ print_type expected);
  assert_bool "Type inféré incorrect pour Head" 
    (types_alpha_equal infer_typered expected)

let test_tail_type _ctxt =
  let term = Tail (Cons (Int 1, Nil)) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TList TNat in
  assert_bool "Type inféré incorrect pour Tail" 
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

let test_LET _ctxt =
  let term = Let ("id", Abs ("x", Var "x"), App (Var "id", Int 5)) in 
  let env = [] in
  let infer_typered = infer_type term env in
  (* let expected = TList TNat in *)
  let expected = TNat in
  Printf.printf "Type inféré: %s\n" (print_type infer_typered);
  assert_bool "Type inféré incorrect pour map"
    (types_alpha_equal infer_typered expected)

let test_map_polymorphic _ctxt = 
  let term = Let ("map",
    Fix ("map",
      Abs ("f",
        Abs ("l",
          IfEmpty (Var "l",
            Nil,
            Cons (
              App (Var "f", Head (Var "l")),
              App (App (Var "map", Var "f"), Tail (Var "l"))
            )
          )
        )
      )
    ),
    App (
      App (Var "map", Abs ("x", Add (Var "x", Int 1))),
      Cons (Int 1, Nil)
    )
  ) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TList TNat in
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


let test_let_imbriq _ctxt = 
  let term = Let ("f", Abs ("x", Add (Var "x", Int 1)),  (* f : Nat -> Nat *)
  Let ("y", Int 10,  App (Var "f", Var "y"))) 
  in
  let env = [] in
  let infer_typered = infer_type term env in
  print_endline ("Type inféré pour let imbriqué: " ^ print_type infer_typered);
  let expected = TNat in
  assert_bool "Type inféré incorrect pour let imbriqué" 
    (types_alpha_equal infer_typered expected)

let test_polymorphic_fibonacci _ctxt = 
  let term = Let ("fib",
    Fix ("fib",
      Abs ("n",
        IfZero (Var "n",
          Int 0,
          IfZero (Sub (Var "n", Int 1),
            Int 1,
            Add (
              App (Var "fib", Sub (Var "n", Int 1)),
              App (Var "fib", Sub (Var "n", Int 2))
            )
          )
        )
      )
    ),
    App (Var "fib", Int 5)
  ) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect pour fibonacci"
    (types_alpha_equal infer_typered expected)


  (* Test plus complexe qui defini filter/map et les utilise is_even qui filtre
   les nombres pairs et retourne une liste d'entiers (1 pour pair, 0 pour impair) *)
let test_complex_filter_map _ctxt = 
  let term = Let ("filter",
    Fix ("filter",
      Abs ("f",
        Abs ("l",
          IfEmpty (Var "l",
            Nil,
            IfZero (App (Var "f", Head (Var "l")),
              App (App (Var "filter", Var "f"), Tail (Var "l")),
              Cons (
                Head (Var "l"), 
                App (App (Var "filter", Var "f"), Tail (Var "l"))
              )
            )
          )
        )
      )
    ),
    Let ("map",
      Fix ("map",
        Abs ("f",
          Abs ("l",
            IfEmpty (Var "l",
              Nil,
              Cons (
                App (Var "f", Head (Var "l")), 
                App (App (Var "map", Var "f"), Tail (Var "l"))
              )
            )
          )
        )
      ),
      Let ("is_even", 
        Fix ("is_even",  
          Abs ("x", 
            IfZero (Var "x", 
              Int 1,  (* Even *)
              IfZero (Sub (Var "x", Int 1),
                Int 0,  (* Odd *)
                App (Var "is_even", Sub (Var "x", Int 2))
              )
            )
          )
        ),
        App (
          App (Var "map", Var "is_even"),
          App (
            App (Var "filter", Var "is_even"),
            Cons (Int 1, Cons (Int 2, Cons (Int 3, Cons (Int 4, Nil))))
          )
        )
      )
    )
  ) in
  let env = [] in
  let infer_typered = infer_type term env in
  print_endline ("Type inféré pour complex filter map: " ^ print_type infer_typered);
  let expected = TList TNat in
  assert_bool "Type inféré incorrect pour filter et map"
    (types_alpha_equal infer_typered expected)

(* -------- PARTIE 5 : Traits impératifs -------- *)

(* Test de l'inférence de type pour Unit *)
let test_unit_type _ctxt =
  let term = Unit in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TUnit in
  assert_bool "Type inféré incorrect pour Unit" 
    (types_alpha_equal infer_typered expected)

let test_unit_complex_type _ctxt =
  let term = Let ("f", Abs ("x", Unit), Unit) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TUnit in
  assert_bool "Type inféré incorrect pour Unit" 
    (types_alpha_equal infer_typered expected)


(* Test de l'inférence de type pour Ref T *)
let test_ref_type _ctxt =
  let term = Ref (Int 42) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TRef TNat in
  assert_bool "Type inféré incorrect pour Ref TNat" 
    (types_alpha_equal infer_typered expected)

let test_ref_complex_type _ctxt =
  let term = Let ("x", Ref (Int 42), Deref (Var "x")) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect pour Ref TNat" 
    (types_alpha_equal infer_typered expected)


(* Test de l'inférence de type pour Deref *)
let test_deref_type _ctxt =
  let term = Deref (Ref (Int 42)) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect pour Deref" 
    (types_alpha_equal infer_typered expected)

let test_deref_complex_type _ctxt =
  let term = Let ("x", Ref (Int 42), Deref (Var "x")) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TNat in
  assert_bool "Type inféré incorrect pour Deref" 
    (types_alpha_equal infer_typered expected)

(* un test encore plus compelexe utilisant les listes *)
let test_deref_more_complex_type _ctxt =
  let term = Let ("x", Ref (Cons (Int 1, Nil)), Deref (Var "x")) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TList TNat in
  assert_bool "Type inféré incorrect pour Deref" 
    (types_alpha_equal infer_typered expected)


(* Test de l'inférence de type pour Assign *)
let test_assign_type _ctxt =
  let term = Assign (Ref (Int 0), Int 42) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TUnit in
  assert_bool "Type inféré incorrect pour Assign" 
    (types_alpha_equal infer_typered expected)

let test_assign_complex_type _ctxt =
  let term = Let ("x", Ref (Int 0), Assign (Var "x", Int 42)) in
  let env = [] in
  let infer_typered = infer_type term env in
  let expected = TUnit in
  assert_bool "Type inféré incorrect pour Assign" 
    (types_alpha_equal infer_typered expected)



(* Suite de tests *)
let typing_tests =
  "Typing Tests" >::: [
    "test_let_imbriq" >:: test_let_imbriq;
    "test_var_type" >:: test_var_type;
    "test_abs_type" >:: test_abs_type;
    "test_app_type" >:: test_app_type;
    "test_add_with_variable_type" >:: test_add_with_variable_type;
    "test_nested_abs_type" >:: test_nested_abs_type;
    "test_malformed_term_type" >:: test_malformed_term_type;
    "test_list_type" >:: test_list_type;
    "test_polymorphic_identity" >:: test_polymorphic_identity;
    "test_LET" >:: test_LET;
    "test_head_type" >:: test_head_type;
    "test_map_polymorphic" >:: test_map_polymorphic;
    "test_factorial_type" >:: test_factorial_type;
    "test_polymorphic_fibonacci" >:: test_polymorphic_fibonacci;
    "test_complex_filter_map" >:: test_complex_filter_map;
]


(* Suite de tests pour les traits impératifs *)
let tests_imperative_traits = 
  "imperative traits tests" >::: [
    "test_ref_type" >:: test_ref_type;
    "test_deref_type" >:: test_deref_type;
    "test_assign_type" >:: test_assign_type;
    "test_unit_type" >:: test_unit_type;
    "test_unit_complex_type" >:: test_unit_complex_type;
    "test_ref_complex_type" >:: test_ref_complex_type;
    "test_deref_complex_type" >:: test_deref_complex_type;
    "test_assign_complex_type" >:: test_assign_complex_type;
    "test_deref_more_complex_type" >:: test_deref_more_complex_type;
  ]

(* Combine the test suites *)
let combined_suites =
  "All Tests" >::: [
    typing_tests;
    tests_imperative_traits;
  ]

(* Exécution des tests *)
let () =
  run_test_tt_main combined_suites
