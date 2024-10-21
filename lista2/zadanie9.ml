type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = { clist = fun f z -> z }

let ccons x xs = { clist = fun f z -> f x (xs.clist f z) } 