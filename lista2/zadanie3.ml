let rec suffixes xs =
  match xs with
  | [] -> [[]]
  | hd :: tl -> xs :: suffixes tl

let prefixes xs =
  let rec helper xs acc =
    match xs with
    | [] -> [[]]
    | hd :: tl ->
      let new_acc = List.rev (hd :: List.rev acc) in
      new_acc :: helper tl new_acc
    in helper xs []