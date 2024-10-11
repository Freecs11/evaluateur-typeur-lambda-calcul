open Lambda_ast

(* Fonction pour vérifier si un terme est une valeur *)
let is_value (t : pterm) : bool =
  match t with
  | Abs (_, _) -> true 
  | _ -> false  

(* Fonction de réduction Call-by-Value (LtR-CbV) *)
let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None  (* Les variables ne se réduisent pas *)
  | Abs (_, _) -> None  
  | App (Abs (x, t1), v2) when is_value v2 ->  
      Some (substitution x v2 t1)  (* On substitue l'argument dans le corps de l'abstraction *)
  | App (v1, t2) when is_value v1 ->  (* on réduit l'argument 
  , c'est que l'argument est une application *)
      (match ltr_cbv_step t2 with
      | Some t2' -> Some (App (v1, t2'))
      | None -> None)  (* l'argument ne peut pas être réduit *)
  | App (t1, t2) ->  (* Réduction du terme en position de fonction *)
      (match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None -> None)


(* Normalisation *)
let rec ltr_cbv_norm (t : pterm) : pterm =
  match ltr_cbv_step t with
  | Some t' -> ltr_cbv_norm t'  
  | None -> t  


(* Normalization with timeout to limit reduction steps *)
let rec ltr_cbv_norm_with_timeout (t : pterm) (timeout : int) : pterm option =
  if timeout = 0 then None
  else match ltr_cbv_step t with
  | Some t' -> ltr_cbv_norm_with_timeout t' (timeout - 1)
  | None -> Some t

