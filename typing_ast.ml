type ptype = 
  | Var of string
  | Arr of ptype * ptype
  | Nat

let rec print_type (t : ptype) : string =
  match t with
  | Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | Nat -> "Nat"
