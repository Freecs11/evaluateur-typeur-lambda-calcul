type pterm = 
  | Var of string
  | App of pterm * pterm
  | Abs of string * pterm
  (* Partie 4 *)
  | Int of int
  | Add of pterm * pterm
  | Sub of pterm * pterm
  | Nil
  | Cons of pterm * pterm
  | Head of pterm
  | Tail of pterm
  | IfZero of pterm * pterm * pterm
  | IfEmpty of pterm * pterm * pterm
  | Fix of string * pterm
  | Let of string * pterm * pterm


let rec print_term (t : pterm) : string =
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"
  | Int n -> string_of_int n
  | Add (t1, t2) -> "(" ^ (print_term t1) ^ " + " ^ (print_term t2) ^ ")"
  | Sub (t1, t2) -> "(" ^ (print_term t1) ^ " - " ^ (print_term t2) ^ ")"
  | Nil -> "Nil"
  | Cons (t1, t2) -> "Cons(" ^ (print_term t1) ^ ", " ^ (print_term t2) ^ ")"
  | Head t1 -> "Head(" ^ (print_term t1) ^ ")"
  | Tail t1 -> "Tail(" ^ (print_term t1) ^ ")"
  | IfZero (t1, t2, t3) -> "IfZero(" ^ (print_term t1) ^ ", " ^ (print_term t2) ^ ", " ^ (print_term t3) ^ ")"
  | IfEmpty (t1, t2, t3) -> "IfEmpty(" ^ (print_term t1) ^ ", " ^ (print_term t2) ^ ", " ^ (print_term t3) ^ ")"
  | Fix (phi, t1) -> "Fix(" ^ phi ^ ", " ^ (print_term t1) ^ ")"
  | Let (x, e1, e2) -> "Let(" ^ x ^ ", " ^ (print_term e1) ^ ", " ^ (print_term e2) ^ ")"


let compteur_var : int ref = ref 0

(* test pushing to both gh and gl *)

let nouvelle_var () : string =
  compteur_var := !compteur_var + 1;
  "X" ^ (string_of_int !compteur_var)


(* récupère les variables libres d'un terme *)
let rec free_vars (t: pterm) : string list =
  match t with
  | Var x -> [x]
  | App (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Abs (x, t) -> List.filter (fun y -> y <> x) (free_vars t)
  | Int _ -> []
  | Add (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Sub (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Nil -> []
  | Cons (t1, t2) -> (free_vars t1) @ (free_vars t2)
  | Head t1 -> free_vars t1
  | Tail t1 -> free_vars t1
  | IfZero (t1, t2, t3) -> (free_vars t1) @ (free_vars t2) @ (free_vars t3)
  | IfEmpty (t1, t2, t3) -> (free_vars t1) @ (free_vars t2) @ (free_vars t3)
  | Fix (phi, t1) -> List.filter (fun y -> y <> phi) (free_vars t1)
  | Let (x, e1, e2) -> (free_vars e1) @ (List.filter (fun y -> y <> x) (free_vars e2))


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
  | Int _ -> t
  | Add (t1, t2) -> Add (substitution x n t1, substitution x n t2)
  | Sub (t1, t2) -> Sub (substitution x n t1, substitution x n t2)
  | Nil -> Nil
  | Cons (t1, t2) -> Cons (substitution x n t1, substitution x n t2)
  | Head t1 -> Head (substitution x n t1)
  | Tail t1 -> Tail (substitution x n t1)
  | IfZero (t1, t2, t3) -> IfZero (substitution x n t1, substitution x n t2, substitution x n t3)
  | IfEmpty (t1, t2, t3) -> IfEmpty (substitution x n t1, substitution x n t2, substitution x n t3)
  | Fix (phi, t1) ->
      if x = phi then Fix (phi, t1)
      else if List.mem phi (free_vars n) then
        let new_var = nouvelle_var () in
        let t' = substitution phi (Var new_var) t1 in
        Fix (new_var, substitution x n t')
      else Fix (phi, substitution x n t1)
  | Let (y, e1, e2) ->
      if x = y then Let (y, substitution x n e1, e2)
      else if List.mem y (free_vars n) then
        let new_var = nouvelle_var () in
        let e2' = substitution y (Var new_var) e2 in
        Let (new_var, substitution x n e1, substitution x n e2')
      else Let (y, substitution x n e1, substitution x n e2)


(* Alpha-conversion: renomme les variables liées pour éviter les conflits *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x
  | App (t1, t2) -> App (alphaconv t1, alphaconv t2)
  | Abs (x, t) ->
      let new_var = nouvelle_var () in
      Abs (new_var, substitution x (Var new_var) (alphaconv t))
  | Int _ -> t
  | Add (t1, t2) -> Add (alphaconv t1, alphaconv t2)
  | Sub (t1, t2) -> Sub (alphaconv t1, alphaconv t2)
  | Nil -> Nil
  | Cons (t1, t2) -> Cons (alphaconv t1, alphaconv t2)
  | Head t1 -> Head (alphaconv t1)
  | Tail t1 -> Tail (alphaconv t1)
  | IfZero (t1, t2, t3) -> IfZero (alphaconv t1, alphaconv t2, alphaconv t3)
  | IfEmpty (t1, t2, t3) -> IfEmpty (alphaconv t1, alphaconv t2, alphaconv t3)
  | Fix (phi, t1) ->
      let new_phi = nouvelle_var () in
      Fix (new_phi, substitution phi (Fix (new_phi, t1)) (alphaconv t1))
  | Let (x, e1, e2) ->
      let e1' = alphaconv e1 in
      if List.mem x (free_vars e1') then
        let new_var = nouvelle_var () in
        Let (new_var, e1', substitution x (Var new_var) (alphaconv e2))
      else
        Let (x, e1', alphaconv e2)


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
    | Int n1, Int n2 -> n1 = n2
    | Add (t1a, t1b), Add (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Sub (t1a, t1b), Sub (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Nil, Nil -> true
    | Cons (t1a, t1b), Cons (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Head t1a, Head t2a ->
        alpha_eq env t1a t2a
    | Tail t1a, Tail t2a ->
        alpha_eq env t1a t2a
    | IfZero (t1a, t1b, t1c), IfZero (t2a, t2b, t2c) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b && alpha_eq env t1c t2c
    | IfEmpty (t1a, t1b, t1c), IfEmpty (t2a, t2b, t2c) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b && alpha_eq env t1c t2c
    | Fix (phi1, t1'), Fix (phi2, t2') ->
        let new_env = (phi1, phi2) :: env in
        alpha_eq new_env t1' t2'
    | Let (x1, e1, e2), Let (x2, e1', e2') ->
        let new_env = (x1, x2) :: env in
        alpha_eq env e1 e1' && alpha_eq new_env e2 e2'
    | _ -> false
  in
  alpha_eq [] t1 t2