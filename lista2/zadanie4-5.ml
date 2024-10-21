let rec merge cmp ls1 ls2 = 
  match ls1, ls2 with
  | [], ls2 -> ls2
  | ls1, [] -> ls1
  | hd1 :: tl1, hd2 :: tl2 ->
    if cmp hd1 hd2 then hd1 :: merge cmp tl1 ls2 else hd2 :: merge cmp ls1 tl2

let merge_tail cmp ls1 ls2 =
  let rec helper ls1 ls2 acc =
    match ls1, ls2 with
    | [], ls2 -> List.rev_append acc ls2
    | ls1, [] -> List.rev_append acc ls1
    | hd1 :: tl1, hd2 :: tl2 ->
      if cmp hd1 hd2 then helper tl1 ls2 (hd1 :: acc) else helper ls1 tl2 (hd2 :: acc)
    in
    helper ls1 ls2 []

let halve lst =
  let rec helper rest first_half second_half =
    match rest, second_half with
    | [], _ | [_], _ -> (first_half, second_half)
    | _ :: _ :: tl, hd :: sh -> helper tl (first_half @ [hd]) sh
    | _ -> failwith "error"
  in 
  helper lst [] lst

let rec merge_sort xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | _ -> 
    let (left, right) = halve xs in
    let right_sorted = merge_sort right in
    let left_sorted = merge_sort left in
    merge (<) left_sorted right_sorted
    

let merge_tail2 cmp ls1 ls2 =
  let rec helper ls1 ls2 acc =
    match ls1, ls2 with
    | [], [] -> acc
    | [], hd :: tl -> helper [] tl (hd :: acc)
    | hd :: tl, [] -> helper tl [] (hd :: acc)
    | hd1 :: tl1, hd2 :: tl2 ->
      if cmp hd1 hd2
        then helper tl1 ls2 (hd1 :: acc)
        else helper ls1 tl2 (hd2 :: acc)
    in
    helper ls1 ls2 []

let rec merge_sort2 cmp xs =
  match xs with
  | []  -> []
  | [x] -> [x]
  | _ ->
      let left, right = halve xs
      in let ncmp = fun a b -> not ((cmp) a b)
      in (merge_tail2 ncmp (merge_sort2 ncmp left) (merge_sort2 ncmp right))