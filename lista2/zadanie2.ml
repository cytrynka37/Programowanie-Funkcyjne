let sublists xs =
  let rec helper xs acc =
    match xs with
    | [] -> acc
    | hd :: tl -> 
      helper tl (List.fold_left (fun acc x -> (hd :: x) :: acc) acc acc)
  in
  helper xs [[]]
