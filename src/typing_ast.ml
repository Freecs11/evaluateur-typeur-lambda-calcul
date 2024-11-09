type ptype = 
  | TVar of string
  | TArr of ptype * ptype
  | TNat
  | TList of ptype  (* [T] *)
  | TForall of string * ptype  (* ∀X.T*)
  | TUnit 
  | TRef of ptype (* Ref(T) *)

let compteur_var_t : int ref = ref 0
let nouvelle_var_t () : string = compteur_var_t := !compteur_var_t + 1; "T" ^ (string_of_int !compteur_var_t)

type equa = (ptype * ptype) list
type env = (string * ptype) list

(* Exceptions *)
exception Type_Inconnu of string
exception Unification_Failed of string

(* Fonctions d'affichage *)
let rec print_type (t : ptype) : string =
  match t with
  | TVar x -> x
  | TArr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"
  | TNat -> "Nat"
  | TList t -> "[" ^ (print_type t) ^ "]"
  | TForall (x, t) -> "(∀" ^ x ^ "." ^ (print_type t) ^ ")"
  | TUnit -> "Unit"
  | TRef t -> "Ref(" ^ (print_type t) ^ ")"

let rec print_env (env : env) : string =
  match env with
  | [] -> ""
  | (x, t) :: q -> x ^ " : " ^ (print_type t) ^ "\n" ^ (print_env q)
let rec print_equa (equa : equa) : string =
  match equa with
  | [] -> ""
  | (t1, t2) :: q -> (print_type t1) ^ " = " ^ (print_type t2) ^ "\n" ^ (print_equa q)


(*------ fonctions basiques ------*)

(* chercher une variable dans l'environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
  | [] -> raise (Type_Inconnu v)
  | (v1, t1)::q when v1 = v -> t1
  | (_, _)::q -> cherche_type v q

(* Occur check , vérifie si une variable de type apparaît dans un type *)
let rec occurs_check (v : string) (ty : ptype) : bool =
  match ty with
  | TVar x -> x = v
  | TArr (t1, t2) -> occurs_check v t1 || occurs_check v t2
  | TNat -> false
  | TList t -> occurs_check v t
  | TForall (x, t) -> if x = v then false else occurs_check v t
  | TUnit -> false
  | TRef t -> occurs_check v t


(* Substitution d'une variable de type par un type dans un type *)
let rec subtitute_type_into_type (var : string) (replacement : ptype) (ty : ptype) : ptype =
  match ty with
  | TVar x  -> if x = var then replacement else TVar x
  | TArr (t1, t2) -> TArr (subtitute_type_into_type var replacement t1, subtitute_type_into_type var replacement t2)
  | TNat -> TNat
  | TList t -> TList (subtitute_type_into_type var replacement t)
  | TForall (x, t) -> if x = var then TForall (x, t) else TForall (x, subtitute_type_into_type var replacement t)
  | TUnit -> TUnit
  | TRef t -> TRef (subtitute_type_into_type var replacement t)

(* Substitution d'une variable de type par un type dans une équation 
  utilisé pour la fonction de test que 2 types sont équivalents 'types_alpha_equal' *)
let rec subtitute_type_into_equa (var : string) (replacement : ptype) (equa : equa) : equa =
  let fun_to_apply (t1, t2) = (subtitute_type_into_type var replacement t1, subtitute_type_into_type var replacement t2) in
  List.map fun_to_apply equa 


(* Fonction qui vérifie si deux types ont des constructeurs différents 
  une exception est le Forall *)
let different_constructors t1 t2 =
  match t1, t2 with
  | TVar _, TVar _ -> false
  | TArr _, TArr _ -> false
  | TNat, TNat -> false
  | TList _, TList _ -> false
  | TForall _, TForall _ -> false
  | TForall _, _ -> false
  | _, TForall _ -> false
  | TUnit, TUnit -> false
  | TRef _, TRef _ -> false
  | _, _ -> true



(* Fonction d'équivalence alpha entre deux types *)
let rec types_alpha_equal t1 t2 =
  let mapping = ref [] in
  let rec aux t1 t2 =
    match t1, t2 with
    | TVar x, TVar y ->
        (try
          let y_mapped = List.assoc x !mapping in
          y = y_mapped
        with Not_found ->
          mapping := (x, y) :: !mapping; 
          true)
    | TArr (a1, a2), TArr (b1, b2) ->
        aux a1 b1 && aux a2 b2
    | TNat, TNat -> true
    | TList t1', TList t2' -> aux t1' t2'  
    | TForall (x1, t1'), TForall (x2, t2') ->
        (* Renommage pour éviter les conflits *)
        let t2'' = subtitute_type_into_type x2 (TVar x1) t2' in
        aux t1' t2'' 
    | TUnit, TUnit -> true
    | TRef t1', TRef t2' -> aux t1' t2'
    | _, _ -> false
  in
  aux t1 t2