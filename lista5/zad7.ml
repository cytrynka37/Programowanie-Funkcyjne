type 'a mylazy = ('a mylazy_state) ref
and 'a mylazy_state =
  | Unevaluated of (unit -> 'a)
  | Evaluating
  | Evaluated of 'a

let force (x : 'a mylazy) : 'a =
  match !x with 
  | Evaluating -> failwith "err"
  | Evaluated v -> v
  | Unevaluated f ->
    x := Evaluating;
    let v = f() in x := Evaluated v; v

let fix (f : 'a mylazy -> 'a) : 'a mylazy =
  let rec i = ref (Unevaluated (fun () -> f i)) in i

(* Typ leniwej listy *)
type 'a lazy_list = 
  | Nil
  | Cons of 'a * 'a lazy_list mylazy

  (* Funkcja do pobrania głowy leniwej listy *)
let head = function
| Nil -> failwith "Empty list"
| Cons (h, _) -> h

(* Funkcja do pobrania ogona leniwej listy *)
let tail = function
| Nil -> failwith "Empty list"
| Cons (_, t) -> force t

(* Funkcja generująca nieskończoną leniwą listę liczb od zadanego punktu *)
let rec from n =
Cons (n, ref (Unevaluated (fun () -> from (n + 1))))


(* Funkcja filtrująca elementy leniwej listy zgodnie z predykatem *)
let rec filter p = function
  | Nil -> Nil
  | Cons (h, t) ->
      if p h then Cons (h, ref (Unevaluated (fun () -> filter p (force t))))
      else filter p (force t)

(* Funkcja tworząca leniwą listę liczb pierwszych *)
let rec sieve = function
  | Nil -> Nil
  | Cons (p, t) ->
      Cons (p, ref (Unevaluated (fun () ->
        sieve (filter (fun x -> x mod p <> 0) (force t)))))
        
(* Lista wszystkich liczb pierwszych, zaczynająca się od 2 *)
let primes = sieve (from 2)



(* Funkcja pobierająca pierwsze n elementów z leniwej listy *)
let rec take n = function
  | Nil -> []
  | Cons (h, t) ->
      if n <= 0 then []
      else h :: take (n - 1) (force t)

(* Wyświetlenie pierwszych 10 liczb pierwszych *)
let () =
  let first_10_primes = take 10 primes in
  List.iter (Printf.printf "%d ") first_10_primes
