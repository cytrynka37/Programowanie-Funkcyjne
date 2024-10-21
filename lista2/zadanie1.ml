let length ls = List.fold_left (fun acc _ -> acc + 1) 0 ls

let rev ls = List.fold_left (fun acc x -> x :: acc) [] ls

let map f ls = List.fold_right (fun x acc -> f x :: acc) ls []

let append ls1 ls2 = List.fold_right (fun x acc -> x :: acc) ls1 ls2

let rev_append ls1 ls2 = List.fold_left (fun acc x -> x :: acc) ls2 ls1

let filter ls f = List.fold_right (fun x acc -> if f x then x :: acc else acc) ls []

let rev_map f ls = List.fold_left (fun acc x -> (f x) :: acc) [] ls