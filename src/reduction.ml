open Lambda_ast

(* Gestion des régions (mémoire) *)
let region_state : (int, pterm) Hashtbl.t = Hashtbl.create 16 (* id -> value *)

(* Compteur pour générer des identifiants de régions uniques *)
let region_counter : int ref = ref 0
let fresh_region_id () : int =
    let id = !region_counter in
    incr region_counter;
    id

(* Fonction pour vérifier si un terme est une valeur *)
let rec is_value (t : pterm) : bool =
    match t with
        | Abs (_, _) -> true
        | Var _ -> true
        | App (Var _, t2) -> is_value t2
        | Int _ -> true 
        | Nil -> true
        | Cons (t1, t2) when is_value t1 && is_value t2 -> true
        | Region _ -> true
        | Unit -> true
        | _ -> false  


(* etape de Call-by-Value (LtR-CbV) avec simplification *)
let rec ltr_cbv_step (t : pterm) : pterm option =
    match t with
    (* Cas de base *)
    | Var _ -> None
    | Int _ -> None
    | Nil -> None
    | Abs (_, _) when is_value t -> None

    (* Application *)
    | App (Abs (x, t1), v2) when is_value v2 ->  
        Some (substitution x v2 t1)
    | App (v1, t2) when is_value v1 ->  
        (match ltr_cbv_step t2 with
        | Some t2' -> Some (App (v1, t2'))
        | None -> None)
    | App (t1, t2) ->  
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (App (t1', t2))
        | None -> None)

    (* Opérateurs arithmétiques *)
    | Add (Int n1, Int n2) -> 
        Some (Int (n1 + n2))
    | Add (v1, t2) when is_value v1 -> 
        (match ltr_cbv_step t2 with
        | Some t2' -> Some (Add (v1, t2'))
        | None -> None)
    | Add (t1, t2) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (Add (t1', t2))
        | None -> None)
    
    | Sub (Int n1, Int n2) -> 
        Some (Int (n1 - n2))
    | Sub (v1, t2) when is_value v1 -> 
        (match ltr_cbv_step t2 with
        | Some t2' -> Some (Sub (v1, t2'))
        | None -> None)
    | Sub (t1, t2) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (Sub (t1', t2))
        | None -> None)

    (* Listes *)
    | Cons (v1, v2) when is_value v1 && is_value v2 -> None
    | Cons (v1, t2) when is_value v1 -> 
        (match ltr_cbv_step t2 with
        | Some t2' -> Some (Cons (v1, t2'))
        | None -> None)
    | Cons (t1, t2) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (Cons (t1', t2))
        | None -> None)

    | Head (Cons (h, _)) when is_value h -> Some h
    | Head (t1) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (Head t1')
        | None -> None)

    | Tail (Cons (_, t)) when is_value t -> Some t
    | Tail (t1) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (Tail t1')
        | None -> None)

    (* Let binding *)
    | Let (x, v1, e2) when is_value v1 -> 
        Some (substitution x v1 e2)
    | Let (x, e1, e2) -> 
        (match ltr_cbv_step e1 with
        | Some e1' -> Some (Let (x, e1', e2))
        | None -> None)

    (* Fix *)
    | Fix (phi, t) -> 
        Some (substitution phi (Fix (phi, t)) t)

    (* Conditionnels *)
    | IfZero (Int 0, t2, _) -> Some t2
    | IfZero (Int _, _, t3) -> Some t3
    | IfZero (t1, t2, t3) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (IfZero (t1', t2, t3))
        | None -> None)

    | IfEmpty (Nil, t2, _) -> Some t2
    | IfEmpty (Cons (h, t), _, t3) when is_value h && is_value t -> Some t3
    | IfEmpty (t1, t2, t3) -> 
        (match ltr_cbv_step t1 with
        | Some t1' -> Some (IfEmpty (t1', t2, t3))
        | None -> None)

    (* Gestion de Deref *)
    | Deref e ->
        if is_value e then (* Si 'e' est une valeur, on vérifie si c'est une région *)
            (match e with
            | Region id ->
                (try           (* Si c'est une région, on récupère la valeur associée dans l'état *)
                let v = Hashtbl.find region_state id in
                Some v (* On réduit 'Deref (Region id)' en la valeur 'v' *)
                with Not_found ->
                failwith ("Région inconnue : " ^ string_of_int id))
            | _ -> failwith "Déréférencement d'une valeur non région")
        else
            (match ltr_cbv_step e with (* Si 'e' n'est pas une valeur, on tente de le réduire *)
            | Some e' -> Some (Deref e') (* jusqu'à ce qu'il devienne une valeur (région) *)
            | None -> None)

    (* Références, mm raisonement que Deref *)
    | Ref e ->
        if is_value e then
            let id = fresh_region_id () in
            Hashtbl.add region_state id e;
            Some (Region id)
        else
            (match ltr_cbv_step e with
            | Some e' -> Some (Ref e')
            | None -> None)

    | Assign (e1, e2) ->
        if is_value e1 then
            if is_value e2 then
            (match e1 with
            | Region id ->
                Hashtbl.replace region_state id e2;
                Some Unit
            | _ -> failwith "Assignation sur une valeur non région")
            else
            (match ltr_cbv_step e2 with
            | Some e2' -> Some (Assign (e1, e2'))
            | None -> None)
        else
            (match ltr_cbv_step e1 with
            | Some e1' -> Some (Assign (e1', e2))
            | None -> None)

    | _ -> None

(* Normalisation LtR-CbV *)
let rec ltr_cbv_norm (t : pterm) : pterm =
    match ltr_cbv_step t with
    | Some t' -> ltr_cbv_norm t'
    | None -> 
        (* on continue la normalisation sous les abstractions *)
        match t with
        | App (t1, t2) when not (is_value t1) || not (is_value t2) -> 
            let t1' = ltr_cbv_norm t1 in
            let t2' = ltr_cbv_norm t2 in
            App (t1', t2')
        | Abs (x, body) -> 
            Abs (x, ltr_cbv_norm body)
        | _ -> t

(* Normalisation LtR-CbV avec timeout *)
let rec ltr_cbv_norm_with_timeout (t : pterm) (timeout : int) : pterm option =
    if timeout = 0 then None
    else 
        match ltr_cbv_step t with
        | Some t' -> ltr_cbv_norm_with_timeout t' (timeout - 1)
        | None ->  
            match t with
            | App (t1, t2) when not (is_value t1) || not (is_value t2) -> 
                let t1' = ltr_cbv_norm_with_timeout t1 (timeout - 1) in
                let t2' = ltr_cbv_norm_with_timeout t2 (timeout - 1) in
                (match t1', t2' with
                | Some t1'', Some t2'' -> Some (App (t1'', t2''))
                | _ -> None)
            | Abs (x, body) -> 
                (match ltr_cbv_norm_with_timeout body (timeout - 1) with
                | Some body' -> Some (Abs (x, body'))
                | None -> None)
            | _ -> Some t

