type cbool = { cbool : 'a. 'a -> 'a -> 'a }
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let ctrue = { cbool = fun a b -> a }
let cfalse = { cbool = fun a b -> b }
let cand (a :cbool) (b : cbool) =
  { cbool = fun x y -> a.cbool (b.cbool x y) y }
let cor (a :cbool) (b : cbool) =
  { cbool = fun x y -> a.cbool x (b.cbool x y) }
let cbool_of_bool (b : bool) = if b then ctrue else cfalse
let bool_of_cbool (cb : cbool) = cb.cbool true false

let zero = { cnum = fun f x -> x }
let succ (n : cnum) = { cnum = fun f x -> f (n.cnum f x) }
let add (num1 : cnum) (num2 : cnum) = 
  { cnum = fun f x -> num1.cnum f (num2.cnum f x) }
let mul (num1 : cnum) (num2 : cnum) = 
  { cnum = fun f x -> num1.cnum (num2.cnum f) x }
let is_zero (num : cnum) = if num == zero then ctrue else cfalse
let rec cnum_of_int n = 
  if n = 0 then zero else succ (cnum_of_int (n - 1))
let int_of_cnum (num: cnum) = num.cnum (fun x -> x + 1) 0
