(*zadanie 1*)
let z1 = fun x -> x
let z2 = fun x -> x * 1
let z3 = fun a b c -> a (b c)
let z4 = fun a b -> a
let z5 a b = if true then a else b
let z6 = raise Not_found

(*zadanie 2*)
let rec f x = f x
