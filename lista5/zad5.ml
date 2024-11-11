type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data = {
  prev : 'a dllist;
  elem : 'a;
  next : 'a dllist;
}

let prev (ls : 'a dllist) = (Lazy.force ls).prev

let elem (ls : 'a dllist) = (Lazy.force ls).elem

let next (ls : 'a dllist) = (Lazy.force ls).next
  
let of_list (ls : 'a list) : 'a dllist =
  match ls with
  | [] -> failwith "empty list"
  | x :: [] -> let rec first = lazy { prev = first; elem = x; next = first } in first
  | hd :: tl ->
    let rec first = lazy { prev = last; elem = hd; next = create_node first tl }
    and last = lazy { prev = get_prev first; elem = List.hd (List.rev ls); next = first }
    and create_node prev ls =
      match ls with
      | [] -> prev
      | [x] -> last
      | x :: xs -> 
        let rec node = lazy { prev = prev; elem = x; next = create_node node xs } 
        in node
    and get_prev prev_node =
      if next prev_node == last then prev_node else get_prev (next prev_node)
    in first

let d = of_list [1;2;3;4]
let t1 = d == prev (next d)
let t2 = d == next (prev d)
let t3 = next (next (next d)) == next (next (next (next (prev d))))


let integers : 'a dllist =
  let rec start = lazy { prev = make_node start (-1); elem = 0; next = make_node start 1 }
  and make_node prev n =
    if n > 0 then 
      let rec node = lazy { prev = prev; elem = n; next = make_node node (n + 1) } 
      in node
    else 
      let rec node = lazy { prev = make_node node (n - 1); elem = n; next = prev }
      in node
  in start

let i1 = (prev (prev (prev (next integers)))) == prev (prev integers)
let i2 = (next (prev integers))  == (prev (next integers))
let i3 = integers == (prev (next integers))