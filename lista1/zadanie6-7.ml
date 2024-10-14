(* zadanie 6 *)
let ctrue (a : 'a) (b : 'a) = a
let cfalse (a : 'a) (b : 'a) = b
let cand (a : 'a -> 'a -> 'a) (b : 'a -> 'a -> 'a) =
  fun (x : 'a) (y : 'a) -> a (b x y) y
let cor (a : 'a -> 'a -> 'a) (b : 'a -> 'a -> 'a) =
  fun (x : 'a) (y : 'a) -> a x (b x y)
let cbool_of_bool (b : bool) = if b then ctrue else cfalse
let bool_of_cbool (cbool : 'a -> 'a -> 'a) = cbool true false

(* zadanie 7 *)
let f x = x + 1
let zero (f : 'a -> 'a) (x : 'a) = x
let succ n (f : 'a -> 'a) (x : 'a) = f (n f x)
let add (num1 : (('a -> 'a) -> 'a -> 'a)) (num2 : (('a -> 'a) -> 'a -> 'a)) = 
  fun f x -> num1 f (num2 f x)
let mul (num1 : (('a -> 'a) -> 'a -> 'a)) (num2 : (('a -> 'a) -> 'a -> 'a)) =
  fun f x -> num1 (num2 f) x
let is_zero (num : ('a -> 'a) -> 'a -> 'a) = if num == zero then ctrue else cfalse
let rec cnum_of_int n = if n = 0 then zero else succ (cnum_of_int (n - 1))
let int_of_cnum (num: (int -> int) -> int -> int) = num (fun x -> x + 1) 0
