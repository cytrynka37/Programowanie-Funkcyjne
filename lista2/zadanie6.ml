let rec insert x xs = 
    match xs with
    | [] -> [[x]]
    | hd :: tl -> (x :: xs) :: (List.map (fun l -> hd :: l) (insert x tl))

let permute_insert xs =
    let rec helper acc xs =
        match xs with
        | [] -> acc
        | hd :: tl -> helper (List.concat (List.map (insert hd) acc)) tl
    in 
    helper [[]]

let rec remove x lst =
    match lst with
    | [] -> []
    | hd :: tl -> if hd = x then tl else hd :: (remove x tl)

let permute_select xs =
    let rec helper acc rest =
        match rest with
        | [] -> [acc]
        | _ ->
            List.concat (
                List.map (fun x ->
                    let rest_without_x = remove x rest in
                    let new_acc = x :: acc in
                    helper new_acc rest_without_x) 
                    rest)
    in
    helper [] xs