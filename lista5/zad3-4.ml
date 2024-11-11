type fraction = int * int

type rational_tree = Node of fraction * rational_tree lazy_t * rational_tree lazy_t

let rec build_tree (a, b) (c, d) = 
  let root = (a + c, b + d) in
  Node (root, lazy (build_tree (a, b) root), lazy (build_tree root (c, d)))

let rational_tree = build_tree (0, 1) (1, 0)

let rec seq_of_tree tree : fraction Seq.t = fun () ->
  let Node(root, left, right) = tree in
  let left_seq = seq_of_tree (Lazy.force left) in
  let right_seq = seq_of_tree (Lazy.force right) in
  Cons (root, (Seq.interleave left_seq right_seq))

let seq_rt = seq_of_tree rational_tree