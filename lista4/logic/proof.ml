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
  | _ -> failwith "Unexpected proof structure: the proof should only have one active goal."


let qed pf =
  match pf with
  | PComplete thm -> thm
  | _ -> failwith "Incomplete proof"

(* let rec move_down pt =
  match pt with
  | PTGoal g -> Some g
  | PTImpI (_, st) | PTBotE (_, st) -> move_down st
  | PTImpE (_, left, right) ->
    (match move_down left with
    | Some goal -> Some goal
    | None -> move_down right)
  | PTComplete _ -> None

let rec move_up ctx =
  match ctx with
  | Root -> None
  | CImpI (pctx, pt) | CBotE (pctx, pt) -> Some (pctx, pt)
  | CImpE (pctx, left, right) -> 
    (match move_down right with
    | Some goal -> Some (CImpE (pctx, left, right), PTGoal goal)
    | None -> move_up pctx)*)

let next pf = failwith "not im"
  (* match pf with
  | PComplete _ -> pf
  | PActive (goal, ctx) ->
    (match move_down goal with
    | Some new_goal -> PActive (PTGoal new_goal, ctx)
    | None -> (match move_up ctx with
              | Some (new_ctx, new_pt) -> PActive (goal, new_ctx)
              | None -> pf)) *) 

exception NotAnImplication
let intro (name: string) (p: proof) : proof =
  match p with
  | PComplete th -> PComplete th 
  | PActive (pt, ctx) ->
      match pt with
      | PTGoal (assump, Impl (f, g)) -> 
          let new_subtree = PTGoal ((name, f) :: assump, g)
          in PActive (new_subtree, CImpI (ctx, PTImpI (Impl (f, g), new_subtree))) 
      | _ -> raise NotAnImplication 

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