type pterm = 
  | Var of string
  | App of pterm * pterm
  | Abs of string * pterm

let rec print_term (t : pterm) : string =
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"


let compteur_var : int ref = ref 0

let nouvelle_var () : string =
  compteur_var := !compteur_var + 1;
  "X" ^ (string_of_int !compteur_var)


(* récupère les variables libres d'un terme *)
let rec free_vars (t: pterm) : string list =
  match t with
  | Var x -> [x]
  | App (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Abs (x, t) -> List.filter (fun y -> y <> x) (free_vars t)


(* Substitution & logging pour les tests*)
let rec substitution (x : string) (n: pterm) (t : pterm) : pterm =
  (* print_endline ("Substituting " ^ x ^ " in term " ^ (print_term t) ^ " with " ^ (print_term n)); *)
  match t with
  | Var y -> 
      if x = y then 
        (* (print_endline ("Substituted: " ^ (print_term n)); *)  
      n
      else Var y
  | App (t1, t2) -> App (substitution x n t1, substitution x n t2)
  | Abs (y, t) ->
      if x = y then Abs (y, t) 
      else if List.mem y (free_vars n) then 
        let new_var = nouvelle_var () in  
        let t' = substitution y (Var new_var) t in  (* Alpha-conversion *)
        Abs (new_var, substitution x n t')
      else Abs (y, substitution x n t)




(* Alpha-conversion: renomme les variables liées pour éviter les conflits *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x
  | App (t1, t2) -> App (alphaconv t1, alphaconv t2)
  | Abs (x, t) ->
      let new_var = nouvelle_var () in
      Abs (new_var, substitution x (Var new_var) (alphaconv t))


(* Alpha-equivalence Equivalence structurelle des termes lambda *)
(* partagé avec fall wally*)
let alpha_equal t1 t2 =
  let rec alpha_eq env t1 t2 =
    match t1, t2 with
    | Var x1, Var x2 ->
        (try List.assoc x1 env = x2
          with Not_found -> x1 = x2)
    | Abs (x1, t1'), Abs (x2, t2') ->
        let new_env = (x1, x2) :: env in
        alpha_eq new_env t1' t2'
    | App (t1a, t1b), App (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | _ -> false
  in
  alpha_eq [] t1 t2