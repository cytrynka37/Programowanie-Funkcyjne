(*zadanie3*)
let hd (s : int -> 't) = s 0

let tl (s : int -> 't) = fun n -> s (n + 1)

let add (s : int -> 't) x = fun n -> s n + x

let map (s : int -> 't) f = fun n -> f (s n)

let map2 (s1 : int -> 't) (s2 : int -> 't) f = fun n -> f (s1 n) (s2 n)

let replace (s : int -> 't) n a = fun i -> if i = n then a else s i

let take_every (s : int -> 't) n = fun i -> s (n * i)


let s1 = fun n -> n

let s2 = fun n -> n * n

(*zadanie4*)
let scan (f : 'a -> 'b -> 'a) (a : 'a) (s : int -> 'b) =
let rec helper = fun i -> 
    if i == 0 then f a (s 0)
    else f (helper (i - 1)) (s i)
in helper 