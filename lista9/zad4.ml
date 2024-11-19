module QBF = struct
  type var = string
  type formula =
  | Var of var (* zmienne zdaniowe *)
  | Bot (* spójnik fałszu (⊥) *)
  | Not of formula (* negacja (¬φ) *)
  | And of formula * formula (* koniunkcja (φ ∧ ψ) *)
  | All of var * formula (* kwantyfikacja uniwersalna (∀p.φ) *)
end

type env = QBF.var -> bool

let rec eval (env : env) (formula : QBF.formula) : bool =
  match formula with
  | Var p -> env p
  | Bot -> false
  | Not f -> not (eval env f)
  | And (f1, f2) -> eval env f1 && eval env f2
  | All (p, f) -> 
    let env_true q = if p = q then true else env q in
    let env_false q = if p = q then false else env q in
    (eval env_true f) && (eval env_false f)

let is_true (formula : QBF.formula) =
  let env _ = failwith "free variable" in
  eval env formula
  

open QBF
let formula1 = All ("p", Var "p")  (* (∀p.p), fałszywe *)
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
  )  (* (∀p.¬∀q.¬(¬(p ∧ q) ∧ ¬(¬p ∧ ¬q))), prawdziwe *)