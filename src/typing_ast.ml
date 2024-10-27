type ptype = 
  | TVar of string
  | TArr of ptype * ptype
  | TNat

let rec print_type (t : ptype) : string =
  match t with
  | TVar x -> x
  | TArr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | TNat -> "Nat"


exception Type_Inconnu of string

exception Unification_Failed of string