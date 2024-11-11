type 'a mylazy = ('a mylazy_state) ref
and 'a mylazy_state =
  | Unevaluated of (unit -> 'a)
  | Evaluating
  | Evaluated of 'a

let force (x : 'a mylazy) : 'a =
  match !x with 
  | Evaluating -> failwith "evaluating"
  | Evaluated v -> v
  | Unevaluated f ->
    x := Evaluating;
    let v = f() in x := Evaluated v; v

let fix (f : 'a mylazy -> 'a) : 'a mylazy =
  let rec i = ref (Unevaluated (fun () -> f i)) in i

type 'a llist = 
  | Nil
  | Cons of 'a * 'a llist mylazy

let rec nats n = fix (fun _ -> Cons (n, nats (n + 1)))

let rec filter p xs =
  fix (fun _ ->
    match force xs with
    | Nil -> Nil
    | Cons (x, xs') when p x -> Cons (x, filter p xs')
    | Cons (_, xs') -> force (filter p xs'))

let rec take_while p xs =
  fix (fun _ ->
  match force xs with
  | Cons (x, ys) when p x -> Cons(x, take_while p ys)
  | _ -> Nil)
let rec for_all p xs =
  force (fix (fun _ ->
  match force xs with
  | Nil -> true
  | Cons (x, xs') -> p x && for_all p xs'))

let primes =
  fix (fun primes ->
    let is_prime n =
      primes
      |> take_while (fun p -> p * p <= n)
      |> for_all (fun p -> n mod p <> 0)
    in
    Cons(2, filter is_prime (nats 3)))
  
let rec to_list ls =
  match force ls with
  | Nil -> []
  | Cons(x, xs) -> x :: to_list xs
  

let res = to_list (take_while ((>) 100) primes);;