open Typing_ast
open Lambda_ast

type equa = (ptype * ptype) list
type env = (string * ptype) list

(* Génération des équations de typage *)
let rec genere_equa (te : pterm) (ty : ptype) (env : env) : equa =
  (* à compléter *)
  []

(* Unification *)
let rec unification (equa : equa) : unit =
  (* à compléter *)
  ()
