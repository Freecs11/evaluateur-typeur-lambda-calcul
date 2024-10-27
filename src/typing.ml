open Typing_ast
open Lambda_ast


let compteur_var_t : int ref = ref 0
let nouvelle_var_t () : string = compteur_var_t := !compteur_var_t + 1; "T" ^ (string_of_int !compteur_var_t)

type equa = (ptype * ptype) list
type env = (string * ptype) list


(* chercher une variable dans l'environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
  | [] -> raise (Type_Inconnu v)
  | (v1, t1)::q when v1 = v -> t1
  | (_, _)::q -> (cherche_type v q) 


(* Génération des équations de typage *)
let rec genere_equa (te : pterm) (ty : ptype) (env : env) : equa =
  match te with
  | Var x -> let t_x = cherche_type x env in [(t_x, ty)] (* Récupérer le type de x dans l'environnement *)
  | Abs (x, t) -> 
      let nv1 : string = nouvelle_var_t () in
      let nv2 : string = nouvelle_var_t () in
      let ty1 = TVar nv1 in
      let ty2 = TVar nv2 in
      (ty, TArr (ty1, ty2)) :: (genere_equa t ty2 ((x, ty1) :: env)) 
  | App (t1, t2) ->
      let nv1 : string = nouvelle_var_t () in
      let ty1 = TVar nv1 in 
      let eq1 = genere_equa t1 (TArr (ty1, ty)) env in
      let eq2 = genere_equa t2 ty1 env in
      eq1 @ eq2


(* Occur check , vérifie si une variable de type apparaît dans un type *)
let rec occurs_check (v : string) (ty : ptype) : bool =
  match ty with
  | TVar x -> x = v
  | TArr (t1, t2) -> occurs_check v t1 || occurs_check v t2
  | TNat -> false


(* Substitution d'une variable de type par un type dans un type *)
let rec subtitute_type_into_type (var : string) (replacement : ptype) (ty : ptype) : ptype =
  match ty with
  | TVar x  -> if x = var then replacement else TVar x
  | TArr (t1, t2) -> TArr (subtitute_type_into_type var replacement t1, subtitute_type_into_type var replacement t2)
  | TNat -> TNat

(* Substitution d'une variable de type par un type dans une équation *)
let rec subtitute_type_into_equa (var : string) (replacement : ptype) (equa : equa) : equa =
  let fun_to_apply (t1, t2) = (subtitute_type_into_type var replacement t1, subtitute_type_into_type var replacement t2) in
  List.map fun_to_apply equa 


(* Unification *)
let rec unification (equa : equa) (sub : env) : (equa * env) =
  match equa with
  | [] -> (equa, sub) 
  | (s, t) :: q when s = t -> unification q sub (* Supprimer l'équation triviale *)
  | (TVar x, t) :: q when not (occurs_check x t) ->
      let q' = subtitute_type_into_equa x t q in (* Remplacer x par t dans les équations *)
      unification q' ((x, t) :: sub) (* Ajouter la substitution sans modifier l'environnement existant *)
  | (t, TVar x) :: q when not (occurs_check x t) ->
      let q' = subtitute_type_into_equa x t q in
      unification q' ((x, t) :: sub)
  | (TArr (s1, s2), TArr (t1, t2)) :: q ->
      unification ((s1, t1) :: (s2, t2) :: q) sub  
  | _ ->
      (* Non unifiable *)
      raise (Unification_Failed "Unification failed")


(* Fonction d'unification avec timeout *)
let rec unify (equa : equa) (sub : env) (timeout : int) : env option =
  if timeout = 0 then None
  else
    match unification equa sub with
    | (equa', sub') -> 
        if equa' = [] then Some sub'
        else unify equa' sub' (timeout - 1)

(* Fonction d'application de substitution sur un type *)
let rec apply_subst_env (subst : env) (ty : ptype) : ptype =
  match ty with
  | TVar x ->
      (try apply_subst_env subst (List.assoc x subst) with Not_found -> TVar x)
  | TArr (t1, t2) -> TArr (apply_subst_env subst t1, apply_subst_env subst t2)
  | TNat -> TNat


(* Fonction d'inférence de type corrigée *)
let infer_type (te : pterm) (env : env) : ptype =
  compteur_var_t := 0;

  (* Générer une variable de type fraîche pour le type cible *)
  let ty_cible = TVar (nouvelle_var_t ()) in

  (* Générer les équations de typage en utilisant l'environnement donnée *)
  let equa = genere_equa te ty_cible env in

  (* Résoudre les équations avec un timeout, par exemple 200 étapes *)
  let subst_option = unify equa [] 200 in

  (* Appliquer la substitution au type cible pour obtenir le type final *)
  match subst_option with
  | Some subst ->
      apply_subst_env subst ty_cible
  | None ->
      raise (Unification_Failed "Unification failed")



(* Fonctions d'affichage *)

let rec print_env (env : env) : string =
  match env with
  | [] -> ""
  | (x, t) :: q -> x ^ " : " ^ (print_type t) ^ "\n" ^ (print_env q)

let rec print_equa (equa : equa) : string =
  match equa with
  | [] -> ""
  | (t1, t2) :: q -> (print_type t1) ^ " = " ^ (print_type t2) ^ "\n" ^ (print_equa q)