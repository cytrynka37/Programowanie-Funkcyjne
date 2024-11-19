type empty = |

type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Empty : empty fin_type
| Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type 

let rec all_values : type a. a fin_type -> a Seq.t = function
  | Unit -> Seq.return ()
  | Bool -> List.to_seq [true; false]
  | Pair (x, y) -> 
        Seq.concat_map
          (fun b -> Seq.map (fun a  -> (a, b)) (all_values x)) 
          (all_values y)
  | Empty -> Seq.empty 
  | Either (x, y) ->
        Seq.append
          (Seq.map (fun a -> Either.Left a) (all_values x))
          (Seq.map (fun a -> Either.Right a) (all_values y))

let t1 = List.of_seq (all_values (Pair(Bool, Unit)))
let t2 = List.of_seq (all_values (Pair(Unit, (Pair(Bool, Bool)))))