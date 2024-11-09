open Typing_ast
open Lambda_ast


(* Récupère les variables libres d'un type *)
let rec free_vars_type (ty : ptype) : string list =
    match ty with
    | TVar x -> [x]
    | TArr (t1, t2) -> 
        List.sort_uniq String.compare (free_vars_type t1 @ free_vars_type t2)
    | TNat -> []
    | TList t -> free_vars_type t
    | TForall (x, t) -> List.filter (fun y -> y <> x) (free_vars_type t)
    | TUnit -> []   
    | TRef t -> free_vars_type t



(* Récupère les variables libres d'un environnement,
    c'est-à-dire les variables libres des types de l'environnement *)
let free_vars_env (env : env) : string list =
    List.concat (List.map (fun (_, t) -> free_vars_type t) env)

(* Fonction d'application de substitution sur un type *)
let rec apply_subst_env (subst : env) (ty : ptype) : ptype =
    match ty with
    | TVar x -> (try apply_subst_env subst (List.assoc x subst) with Not_found -> TVar x)
    | TArr (t1, t2) -> TArr (apply_subst_env subst t1, apply_subst_env subst t2)
    | TNat -> TNat
    | TList t -> TList (apply_subst_env subst t)
    | TForall (x, t) -> TForall (x, apply_subst_env subst t)
    | TUnit -> TUnit
    | TRef t -> TRef (apply_subst_env subst t)



(* Généralisation d'un type *)
let generalize (env : env) (ty : ptype) : ptype =
    let env_vars = free_vars_env env in
    let type_vars = free_vars_type ty in
    let free_vars = List.filter (fun x -> not (List.mem x env_vars)) type_vars in
    List.fold_right (fun x acc -> TForall (x, acc)) free_vars ty

(* Fonction d'unification , prend une équation et un environnement et retourne l'équation résolue et l'environnement *)
let rec unification (eqs : equa) (sub : env) : (equa * env) =
    Printf.printf "Unification de %s\n" (print_equa eqs);
    match eqs with  
    | [] -> (eqs, sub)
    | (s, t) :: q when s = t -> 
        Printf.printf "Équation triviale supprimée: %s = %s\n" (print_type s) (print_type t);
        unification q sub
    | (TVar x, t) :: q | (t, TVar x) :: q -> 
        if occurs_check x t then
            raise (Unification_Failed (Printf.sprintf "Unification failed"))
        else
            let q' = subtitute_type_into_equa x t q in
            let sub' = (x, t) :: sub in
            unification q' sub'
    | (s, t) :: q when different_constructors s t -> 
        Printf.printf "Unification échouée : %s != %s\n" (print_type s) (print_type t);
        raise (Unification_Failed "Unification failed: different constructors")
    | (TArr (s1, s2), TArr (t1, t2)) :: q ->
        (* Printf.printf "Unification TArr\n"; *)
        unification ((s1, t1) :: (s2, t2) :: q) sub  
    | (TList t1, TList t2) :: q ->
        (* Printf.printf "Unification TList\n"; *)
        unification ((t1, t2) :: q) sub
    | (TForall (x, t1), t2) :: rest | (t2, TForall (x, t1)) :: rest ->
        let fresh = TVar (nouvelle_var_t ()) in
        let t1' = subtitute_type_into_type x fresh t1 in
        unification ((t1', t2) :: rest) sub
    | (TUnit, TUnit) :: q -> unification q sub
    | (TRef t1, TRef t2) :: q -> unification ((t1, t2) :: q) sub
    | (s, t) :: q -> 
        raise (Unification_Failed "Unification failed: no matching case")


(* Fonction d'unification avec timeout *)
let rec unify (equa : equa) (sub : env) (timeout : int) : env option =
    if timeout = 0 then None
    else
    match unification equa sub with
    | (equa', sub') -> 
        if equa' = [] then Some sub'
        else unify equa' sub' (timeout - 1)


(* Génération des équations de typage *)
let rec genere_equa (te : pterm) (ty : ptype) (env : env) : equa =
    match te with
    | Var x -> 
        let t_x = cherche_type x env in 
        [(ty, t_x)]

    | Abs (x, t) -> 
        let nv1 = nouvelle_var_t () in
        let nv2 = nouvelle_var_t () in
        let ty1 = TVar nv1 in
        let ty2 = TVar nv2 in
        (ty, TArr (ty1, ty2)) :: (genere_equa t ty2 ((x, ty1) :: env))

    | App (t1, t2) ->
        let nv1 = nouvelle_var_t () in
        let ty1 = TVar nv1 in 
        let eq1 = genere_equa t1 (TArr (ty1, ty)) env in
        let eq2 = genere_equa t2 ty1 env in
        eq1 @ eq2

    | Int _ -> 
        [(ty, TNat)]

    | Add (t1, t2) | Sub (t1, t2) -> 
        (ty, TNat) :: (genere_equa t1 TNat env) @ (genere_equa t2 TNat env)

    | Cons (t1, t2) ->
        let elem_type = TVar (nouvelle_var_t ()) in
        (ty, TList elem_type) ::
        (genere_equa t1 elem_type env) @
        (genere_equa t2 (TList elem_type) env)

    | Head t -> 
        (* je ne suis pas sur de comprendre le sens que doit prendre le head/tail, je sais pas si on doit 
        renvoyé TForall x. (|x| -> x) ou juste le type de la liste  *)
        let alpha = nouvelle_var_t () in
        let t1 = TList (TVar alpha) in
        [(ty, TVar alpha)] @
        (genere_equa t t1 env)

    | Tail t ->
        let alpha = nouvelle_var_t () in
        let t1 = TList (TVar alpha) in
        [(ty, t1)] @
        (genere_equa t t1 env)

    | IfZero (cond, then_branch, else_branch) ->
        (genere_equa cond TNat env) @
        (genere_equa then_branch ty env) @
        (genere_equa else_branch ty env)

    | IfEmpty (cond, then_branch, else_branch) ->
        let alpha = nouvelle_var_t () in
        (genere_equa cond (TForall (alpha, TList (TVar alpha))) env) @
        (genere_equa then_branch ty env) @
        (genere_equa else_branch ty env)
    
    | Fix (f, t) -> 
        let rec_type = TVar (nouvelle_var_t ()) in
        let env_with_f = (f, rec_type) :: env in
        let body_eqns = genere_equa t rec_type env_with_f in
        [(ty, rec_type)] @ body_eqns

    |Let (x, e1, e2) ->
        let ty_e1 = infer_type e1 env in
        let gen_t0 = generalize env ty_e1 in
        let env2 = (x, gen_t0) :: env in
        genere_equa e2 ty env2
    
    | Nil -> [(ty, TList (TVar (nouvelle_var_t())))]
    | Unit -> [(ty, TUnit)]

    | Ref e ->
        let inner_ty = TVar (nouvelle_var_t ()) in   
        (ty, TRef inner_ty) :: (genere_equa e inner_ty env)
    
    | Deref e ->
        let inner_ty = TVar (nouvelle_var_t ()) in
        (* DEREF e : ty = inner_ty et e : Ref inner_ty *)
        (ty, inner_ty) :: (genere_equa e (TRef inner_ty) env)
    
    | Assign (e1, e2) ->
        let inner_ty = TVar (nouvelle_var_t ()) in
        (ty, TUnit) :: 
        (genere_equa e1 (TRef inner_ty) env) @ (genere_equa e2 inner_ty env)
    | _ -> []

and  infer_type (te : pterm) (env : env) : ptype =
    compteur_var_t := 0;
    let ty_cible = TVar (nouvelle_var_t ()) in
    let equa = genere_equa te ty_cible env in
    Printf.printf "Équations de typage générées: %s\n" (print_equa equa);
    (* Solve the equations with a timeout *)
    let subst_option = unify equa [] 500 in
    match subst_option with
        | Some sub -> apply_subst_env sub ty_cible
        | None -> raise (Type_Inconnu "Type inference failed")


