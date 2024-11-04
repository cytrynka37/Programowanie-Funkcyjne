type formula = False | Var of string | Impl of formula * formula

let rec string_of_formula f =
  match f with
  | False -> "F"
  | Var v -> v
  | Impl (f1, f2) ->
    let sf1 = string_of_formula f1 in
    let sf2 = string_of_formula f2 in
    (match f1, f2 with
      | Impl(_, _), Impl(_, _) -> "(" ^ sf1 ^ ") -> (" ^ sf2 ^ ")"
      | Impl(_, _), _ -> "(" ^ sf1 ^ ") -> " ^ sf2
      | _ -> sf1 ^ " -> " ^ sf2)

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem =
  | ByAssumption of formula list * formula
  | ImpI of formula list * formula * theorem
  | ImpE of formula list * formula * theorem * theorem
  | BotE of formula list * formula * theorem

let assumptions thm =
  match thm with
  | ByAssumption (assump, _) | ImpI (assump, _, _) | ImpE (assump, _, _, _) | BotE (assump, _, _) -> assump

let consequence thm =
  match thm with
  | ByAssumption (_, consq) | ImpI (_, consq, _) | ImpE (_, consq, _, _) | BotE (_, consq, _) -> consq

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = ByAssumption ([f], f)

let imp_i f thm =
  ImpI (List.filter ((<>) f) (assumptions thm), Impl(f, consequence thm), thm)

let imp_e th1 th2 =
  match consequence th1 with
  | Impl(f1, f2) ->
    if f1 <> consequence th2 then failwith "th1 and th2 have different conclusions"
    else ImpE (assumptions th1 @ assumptions th2, f2, th1, th2)
  | _ -> failwith "th1 is not an implication"

let bot_e f thm =
  match consequence thm with
  | False -> BotE (assumptions thm, f, thm)
  | _ -> failwith "conclusion is not False"