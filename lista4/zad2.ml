type 'a zlist = 'a list * 'a list

let of_list lst = ([], lst)

let elem lst =
  match lst with
  | (_, y :: _) -> Some y
  | (_, _) -> None

let to_list lst =
  let (xs, ys) = lst in
  List.rev_append xs ys

let move_left lst =
  match lst with
  | (x :: xs, ys) -> (xs, x :: ys)
  | (_, _)-> lst

let move_right lst =
  match lst with
  | (xs, y :: ys) -> (y :: xs, ys)
  | (_, _) -> lst

let insert a lst =
  let (xs, ys) = lst in
  (a :: xs, ys)

let remove lst =
  match lst with
  | (x :: xs, ys) -> (xs, ys)
  | ([], _) -> failwith "Nothing to remove"