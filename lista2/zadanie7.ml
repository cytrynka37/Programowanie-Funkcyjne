type 'a heap =
| Leaf 
| Node of 'a * 'a heap * 'a heap * int

let smart_constructor h1 h2 e =
  match h1, h2 with
  | Leaf, _ -> Node (e, h2, h1, 0)
  | _, Leaf -> Node (e, h1, h2, 0)
  | Node (_, _, _, z), Node (_, _, _, y) -> if y >= z then Node (e, h2, h1, z + 1) else Node (e, h1, h2, y + 1)

let rec merge h1 h2 =
  match h1, h2 with
  | Leaf, _ -> h2
  | _, Leaf -> h1
  | Node (v1, l1, r1, s1), Node (v2, l2, r2, s2) ->
    if v1 <= v2 then
      smart_constructor l1 (merge r1 h2) v1
    else merge h2 h1 

let rec insert x h = merge (Node (x, Leaf, Leaf, 0)) h

let remove h = 
  match h with
  | Leaf -> Leaf
  | Node (_, l, r, _) -> merge l r

let h1 = Node (4, Node (19, Leaf, Leaf, 0), Node (8, Leaf, Leaf, 0), 1)
let h2 = Node (6, Node (8, Leaf, Leaf, 0), Node (7, Leaf, Leaf, 0), 1)
let h3 = Node (5, Node (12, Node (13, Leaf, Leaf, 0), Node (19, Leaf, Leaf, 0), 1), Leaf, 0)
let h4 = Node (6, Node (7, Node (8, Leaf, Leaf, 0), Node (9, Leaf, Leaf, 0), 1), Node (10, Leaf, Leaf, 0), 1)