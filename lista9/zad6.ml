module QBF = struct
  type var = string
  type formula =
  | Var of var (* zmienne zdaniowe *)
  | Bot (* spójnik fałszu (⊥) *)
  | Not of formula (* negacja (¬φ) *)
  | And of formula * formula (* koniunkcja (φ ∧ ψ) *)
  | All of var * formula (* kwantyfikacja uniwersalna (∀p.φ) *)
end

type 'v inc = Z | S of 'v

module NestedQBF = struct
  type 'v formula =
  | Var of 'v
  | Bot
  | Not of 'v formula
  | And of 'v formula * 'v formula
  | All of 'v inc formula
end

type 'v env = QBF.var -> 'v

let rec scope_check : type v. v env -> QBF.formula -> v NestedQBF.formula =
  fun env formula ->
    match formula with
    | Var p -> NestedQBF.Var (env p)
    | Bot -> NestedQBF.Bot
    | Not f -> NestedQBF.Not (scope_check env f)
    | And (f1, f2) -> NestedQBF.And (scope_check env f1, scope_check env f2)
    | All (p, f) -> 
      let new_env x = 
        if x = p then Z else S (env x)
      in NestedQBF.All (scope_check new_env f) 

let sc = scope_check (fun x -> failwith "unbounded variable")

open QBF
let formula1 = All ("p", Var "p")  (* (∀p.p) *)
let formula2 =
  All ("p",
    Not (
      All ("q",
        Not (
          And (
            Not (And (Var "p", Var "q")),
            Not (And (Not (Var "p"), Not (Var "q")))
          )
        )
      )
    )
  )  (* (∀p.¬∀q.¬(¬(p ∧ q) ∧ ¬(¬p ∧ ¬q))) *)