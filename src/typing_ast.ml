type ptype = 
  | TVar of string
  | TArr of ptype * ptype
  | TNat
  | TList of ptype  (* [T] *)
  | TForall of string * ptype  (* ∀X.T*)

let compteur_var_t : int ref = ref 0
let nouvelle_var_t () : string = compteur_var_t := !compteur_var_t + 1; "T" ^ (string_of_int !compteur_var_t)

type equa = (ptype * ptype) list
type env = (string * ptype) list

exception Type_Inconnu of string
exception Unification_Failed of string

(* Fonctions d'affichage *)
let rec print_type (t : ptype) : string =
  match t with
  | TVar x -> x
  | TArr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | TNat -> "Nat"
  | TList t -> "[" ^ (print_type t) ^ "]"
  | TForall (x, t) -> "(∀" ^ x ^ "." ^ (print_type t) ^ ")"

let rec print_env (env : env) : string =
  match env with
  | [] -> ""
  | (x, t) :: q -> x ^ " : " ^ (print_type t) ^ "\n" ^ (print_env q)
let rec print_equa (equa : equa) : string =
  match equa with
  | [] -> ""
  | (t1, t2) :: q -> (print_type t1) ^ " = " ^ (print_type t2) ^ "\n" ^ (print_equa q)

