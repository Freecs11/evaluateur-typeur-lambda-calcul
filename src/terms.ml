(* Lambda Terms *)
open Lambda_ast


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
