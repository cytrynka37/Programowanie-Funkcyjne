exception RecursionLimitExceeded

let rec fix f x = f (fix f) x

let rec fix_with_limit n f x =
  if n <= 0 then raise RecursionLimitExceeded
  else f (fix_with_limit (n - 1) f) x

let fix_memo f x =
  let table = Hashtbl.create 100 in
  let rec helper x =
    if Hashtbl.mem table x then
      Hashtbl.find table x
    else
      let result = f helper x in
      Hashtbl.add table x result; 
      result
  in helper x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fib_limit = fix_with_limit 2 fib_f

let fib_memo = fix_memo fib_f