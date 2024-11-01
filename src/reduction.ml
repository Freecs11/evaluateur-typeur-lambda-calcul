open Lambda_ast

(* Fonction pour vérifier si un terme est une valeur *)
let rec is_value (t : pterm) : bool =
  match t with
  | Abs (_, _) -> true
  | Var _ -> true
  | App (Var _, t2) -> is_value t2
  (* | Int _ -> true  *)
  | _ -> false  


(* Fonction de simplification avec limite d'Ã©tapes *)
(* Faite avec Alseiny *)
let rec simplify (t : pterm) (limit : int) : pterm =
  if limit <= 0 then
    (* Si la limite est atteinte, renvoyer simplement le terme sans lever d'exception *)
    t
  else
    match t with
    | App (Abs (x, t1), t2) -> simplify (substitution x t2 t1) (limit - 1)
    | App (t1, t2) -> App (simplify t1 (limit - 1), simplify t2 (limit - 1))
    | Abs (x, t1) -> Abs (x, simplify t1 (limit - 1))
    | Var _ -> t


(* etape de Call-by-Value (LtR-CbV) avec simplification *)
let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None  (* variable *)
  | Abs (x, t1) -> 
      (match ltr_cbv_step t1 with
        | Some t1' -> Some (Abs (x, t1'))
        | None -> None)
  | App (Abs (x, t1), v2) when is_value v2 ->  
      Some (simplify (substitution x v2 t1) 100)  (* on substitue l'argument dans le corps et on simplifie *)
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
  | None -> (simplify t  100) (* simplifie la normalization *)

(* Normalisation LtR-CbV avec timeout *)
let rec ltr_cbv_norm_with_timeout (t : pterm) (timeout : int) : pterm option =
  if timeout = 0 then None
  else 
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm_with_timeout t' (timeout - 1)
    | None -> Some (simplify t 100)


(* Fonction qui convertit un entier en notation Church en un entier OCaml 
  Fait avec Alseiny
*)
let churchto_int (church_num: pterm) : int =
  match church_num with
  | Abs ( _ , Abs ( _ , body)) -> (
      let rec eval n = match n with
        | Var "x" -> 0
        | App (Var "f", t) -> 1 + eval t
        |  _ -> failwith "Invalid Church numeral"
      in eval body
    )
  | _ -> failwith "Not a valid Church numeral"
;;