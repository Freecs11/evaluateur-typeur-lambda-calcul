open Lambda_ast
open Reduction

(* testes de qq termes lambda  *)
let term1 = Var "x"
let term2 = Abs ("x", Var "x")
let term3 = App (Abs ("x", Var "x"), Var "y")

let term_alpha_conv_test = App (Abs ("x", Var "x"), Abs ("y", App (Var "x", Var "y")))

let term1_alpha = Abs ("x", Abs ("x", Var "x"))
let term2_alpha = Abs ("x", App (Var "y", Var "x"))
let term3_alpha = Abs ("x", Abs ("y", App (Var "x", Abs ("x", Var "x"))))
let term4_alpha = App (Abs ("x", App (Var "x", Var "y")), Abs ("y", Var "x"))

(* main *)
let () =
    print_endline ("Term 1: " ^ print_term term1);
    print_endline ("Term 2: " ^ print_term term2);
    print_endline ("Term 3: " ^ print_term term3);

    (* print_endline ("Term alpha conv test: " ^ print_term term_alpha_conv_test); *)
    (* print_endline ("Term alpha conv test after alphaconv: " ^ print_term (alphaconv term_alpha_conv_test)); *)

    print_endline ("Term 1 alpha: " ^ print_term term1_alpha);
    print_endline ("Term 1 alpha after alphaconv: " ^ print_term (alphaconv term1_alpha));
    print_endline ("Term 2 alpha: " ^ print_term term2_alpha);
    print_endline ("Term 2 alpha after alphaconv: " ^ print_term (alphaconv term2_alpha));
    print_endline ("Term 3 alpha: " ^ print_term term3_alpha);
    print_endline ("Term 3 alpha after alphaconv: " ^ print_term (alphaconv term3_alpha));
    print_endline ("Term 4 alpha: " ^ print_term term4_alpha);
    print_endline ("Term 4 alpha after alphaconv: " ^ print_term (alphaconv term4_alpha));


(* 
term1:
    Before: λx. λx. x
    After: λx. λy. y
term2:
    Before: λx. (y x)
    After: λz. (y z) (or another fresh variable)
term3:
    Before: λx. λy. (x λx. x)
    After: λx. λy. (x λz. z)
term4:
    Before: (λx. (x y)) (λy. x)
    After: (λz. (z y)) (λw. x) 
*)
