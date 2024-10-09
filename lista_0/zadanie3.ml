let hd (s : int -> 't) = s 0
let tl (s : int -> 't) = fun n -> s (n + 1)