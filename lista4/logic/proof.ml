open Logic

type goal = (string * formula) list * formula

type proof_tree = 
  | PTGoal of goal
  | PTComplete of theorem
  | PTImpI of formula * proof_tree
  | PTImpE of formula * proof_tree * proof_tree
  | PTBotE of formula * proof_tree

type context =
  | Root
  | CImpI of context * proof_tree
  | CImpE of context * proof_tree * proof_tree
  | CBotE of context * proof_tree

type proof =
  | PComplete of theorem
  | PActive of proof_tree * context

let proof g f = PActive (PTGoal (g, f), Root)

let goal pf =
  match pf with
  | PComplete _ -> None
  | PActive (PTGoal (assumptions, formula), _) -> Some (assumptions, formula)
  | _ -> failwith "There is no goal"


let qed pf =
  match pf with
  | PComplete thm -> thm
  | _ -> failwith "Incomplete proof" 

let next pf = failwith "not implemented" 
              
let intro (name: string) (p: proof) : proof =
  match p with
  | PComplete th -> PComplete th 
  | PActive (pt, ctx) ->
      match pt with
      | PTGoal (assump, Impl (f, g)) -> 
          let new_subtree = PTGoal ((name, f) :: assump, g)
          in PActive (new_subtree, CImpI (ctx, PTImpI (Impl (f, g), new_subtree))) 
      | _ -> failwith "Not an implication" 

let apply f pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_thm thm pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_assm name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"


let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()
