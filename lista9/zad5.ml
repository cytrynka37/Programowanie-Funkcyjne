type empty = |

type 'v inc = Z | S of 'v

module NestedQBF = struct
  type 'v formula =
  | Var of 'v
  | Bot
  | Not of 'v formula
  | And of 'v formula * 'v formula
  | All of 'v inc formula
end

let absurd (x : empty) = match x with _ -> .

type 'v env = 'v -> bool

let rec eval : type a. a env -> a NestedQBF.formula -> bool = 
  fun env formula ->
  match formula with
  | Var p -> env p
  | Bot -> false
  | Not f -> not (eval env f)
  | And (f1, f2) -> (eval env f1) && (eval env f2)
  | All f ->
    let env_true = function
      | Z -> true
      | S p -> env p
    in
    let env_false = function
      | Z -> false
      | S p -> env p
    in (eval env_true f) && (eval env_false f)

let is_true (formula : empty NestedQBF.formula) =
  let env x = absurd x in
  eval env formula

open NestedQBF
let formula1 = All (Var Z)  (* (∀p.p), fałszywe *)
let formula2 =
  All (
    Not ( 
      All (
        Not (
          And ( 
            Not ( And ( Var (S Z), Var Z ) ),
            Not ( And ( Not (Var (S Z)), Not (Var Z) ) )
          )
        )
      )
    )
  )
 (* (∀p.¬∀q.¬(¬(p ∧ q) ∧ ¬(¬p ∧ ¬q))), prawdziwe *)