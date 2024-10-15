open Lambda_ast

(* Fonction pour vérifier si un terme est une valeur *)
let is_value (t : pterm) : bool =
  match t with
  | Abs (_, _) -> true 
  | _ -> false  


(* Simplification des termes *)
let rec simplify (t : pterm) : pterm =
  match t with
  (* Simplification des applications d'identité: (fun x -> x) arg -> arg *)
  | App (Abs (x, Var y), arg) when x = y -> simplify arg

  (* Simplification des applications imbriquées d'identité *)
  | App (Abs (f, Abs (x, Var y)), arg) when x = y -> simplify (Abs (x, Var y))

  (* Simplification des applications d'identité imbriquées *)
  | App (Abs (x, body), arg) -> 
      let simplified_body = simplify body in
      let simplified_arg = simplify arg in
      (match simplified_body with
      (* Si le corps de la fonction est une fonction identité, on simplifie l'application *)
      | Var x' when x = x' -> simplified_arg
      | _ -> App (Abs (x, simplified_body), simplified_arg))

  (* Abstractions redondantes *)
  | Abs (x, Abs (y, t1)) -> 
      if x = y then simplify (Abs (x, t1))
      else Abs (x, simplify (Abs (y, t1)))

  (* Simplification des applications *)
  | App (t1, t2) -> 
      let simplified_t1 = simplify t1 in
      let simplified_t2 = simplify t2 in
      App (simplified_t1, simplified_t2)

  (* Simplification des abstractions *)
  | Abs (x, t1) -> Abs (x, simplify t1)

  (* Variables *)
  | Var x -> Var x


(* etape de Call-by-Value (LtR-CbV) avec simplification *)
let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None  (* variable *)
  | Abs (x, t1) -> 
      (match ltr_cbv_step t1 with
        | Some t1' -> Some (Abs (x, t1'))
        | None -> None)
  | App (Abs (x, t1), v2) when is_value v2 ->  
      Some (simplify (substitution x v2 t1))  (* on substitue l'argument dans le corps et on simplifie *)
  | App (v1, t2) when is_value v1 ->  (* reduction de l'argument *)
      (match ltr_cbv_step t2 with
      | Some t2' -> Some (App (v1, t2'))
      | None -> None)
  | App (t1, t2) ->  (* la partie de la fonction est réduite *)
      (match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None -> None)

(* Normalisation LtR-CbV *)
let rec ltr_cbv_norm (t : pterm) : pterm =
  (* print_endline ("Normalizing: " ^ print_term t); *)
  match ltr_cbv_step t with
  | Some t' -> ltr_cbv_norm t'
  | None -> simplify t  (* simplifie la normalization *)

(* Normalisation LtR-CbV avec timeout *)
let rec ltr_cbv_norm_with_timeout (t : pterm) (timeout : int) : pterm option =
  if timeout = 0 then None
  else 
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm_with_timeout t' (timeout - 1)
    | None -> Some (simplify t)