open Liveness_analysis
open Ltltree
open Ops
open Format

(* Interference graph types *)
type arcs = { prefs: Register.set; intfs: Register.set }
type igraph = arcs Register.map

(* Coloring types *)
type color = Ltltree.operand
type coloring = color Register.map

let get_arcs r graph =
  try
    Register.M.find r graph
  with
  | Not_found -> { prefs = Register.S.empty; intfs = Register.S.empty }

let handle_instr_pref_edges _ li graph = match li.instr with
| Ertltree.Embinop(Mmov, r1, r2, _) when not (r1 = r2) ->
  begin
    let a1 = get_arcs r1 graph in
    let a2 = get_arcs r2 graph in
    let g1 = Register.M.add r1 { a1 with prefs = Register.S.add r2 a1.prefs } graph in
    Register.M.add r2 { a2 with prefs = Register.S.add r1 a2.prefs } g1
  end
| _ -> graph

let add_interference_edge graph r1 r2 =
  let a1 = get_arcs r1 graph in
  let a2 = get_arcs r2 graph in
  let g1 = Register.M.add r1 { a1 with intfs = Register.S.add r2 a1.intfs } graph in
  Register.M.add r2 { a2 with intfs = Register.S.add r1 a2.intfs } g1

let handle_instr_interference_edges _ li graph = match li.instr with
| Ertltree.Embinop(Mmov, r1, r2, _) ->
  begin
    let handle_out_vars r g =
      if not (r = r1) && not (r = r2) then add_interference_edge g r r2 else g in
    Register.S.fold handle_out_vars li.outs graph
  end
| _ ->
  begin
    let handle_def_vars v g =
      let handle_out_vars r g1 =
        if not (r = v) then add_interference_edge g1 r v else g1 in
        Register.S.fold handle_out_vars li.outs g in
    Register.S.fold handle_def_vars li.defs graph
  end

let print_ig ig =
Register.M.iter (fun r arcs ->
  Format.printf "%s: prefs=@[%a@] intfs=@[%a@]@." (r :> string)
    Register.print_set arcs.prefs Register.print_set arcs.intfs) ig

let print_color_aux fmt = function
  | Reg hr    -> fprintf fmt "%a" Register.print hr
  | Spilled n -> fprintf fmt "stack %d" n
let print_color cm =
  Register.M.iter
    (fun r cr -> printf "%a -> %a@\n" Register.print r print_color_aux cr) cm

let make liveness_info =
  let g1 = Label.M.fold handle_instr_pref_edges liveness_info Register.M.empty in
  Label.M.fold handle_instr_interference_edges liveness_info g1

let choose_register_color todo_map coloring ig =
  let get_pref1 reg colors =
    if not ((Register.S.cardinal colors) = 1) then None
    else
      let color = Register.S.choose colors in
      let prefs = (Register.M.find reg ig).prefs in
      try
        let r_color = Register.S.find color prefs in Some(reg, r_color)
      with
      | Not_found -> None in
  let get_pref2 reg colors =
    if not ((Register.S.cardinal colors) = 1) then None
    else Some(reg, Register.S.choose colors) in
  let get_pref3 reg colors =
    let get_pref3_aux pref = function
    | Some(r, c) -> Some(r, c)
    | None ->
      try
        let c_pref = Register.S.find pref colors in
        if not (Register.is_pseudo c_pref) then
          Some(reg, c_pref)
        else
          let c = Register.M.find c_pref coloring in
          match c with
          | Reg color -> Some(reg, color)
          | _ -> None
      with
      | Not_found -> None in
    let prefs = (Register.M.find reg ig).prefs in
    Register.S.fold get_pref3_aux prefs None in
  let get_pref4 reg colors =
    if not (Register.S.is_empty colors) then
      Some(reg, Register.S.choose colors)
    else None in
  let rec choose_register_color_aux pref2 pref3 pref4 todo_map =
    begin
      if (Register.M.is_empty todo_map) then
        match pref2, pref3, pref4 with
        | Some(_), _, _ -> pref2
        | _, Some(_), _ -> pref3
        | _, _, _ -> pref4
      else
        let (reg, colors) = Register.M.choose todo_map in
        match get_pref1 reg colors with
        | None ->
          begin
            let new_pref2 = match pref2 with
            | None -> get_pref2 reg colors
            | _ -> pref2 in
            let new_pref3 = match pref3 with
            | None -> get_pref3 reg colors
            | _ -> pref3 in
            let new_pref4 = match pref4 with
            | None -> get_pref4 reg colors
            | _ -> pref4 in
            choose_register_color_aux
              new_pref2 new_pref3 new_pref4
              (Register.M.remove reg todo_map)
          end
        | Some(r, c) -> Some(r, c)
    end in
  choose_register_color_aux None None None todo_map


(* val color: igraph -> coloring * int *)
let color ig =
  let build_init_todo ig =
    let map1 = Register.M.filter (fun k _ -> Register.is_pseudo k) ig in
    Register.M.map (fun a -> Register.S.diff Register.allocatable a.intfs) map1 in
  let todo = ref (build_init_todo ig) in
  let coloring = ref Register.M.empty in
  let nb_to_spill = ref 0 in
  let add_to_coloring r c =
    coloring := Register.M.add r c !coloring in
  while (not (Register.M.is_empty !todo)) do
    match choose_register_color !todo !coloring ig with
    | Some(r, c) ->
      let remove_color_from_todo r map =
        begin
          try
            let possible_colors = Register.M.find r map in
            Register.M.add r (Register.S.remove c possible_colors) map
          with
          | Not_found -> map
        end in
      let new_todo =
        Register.S.fold remove_color_from_todo (Register.M.find r ig).intfs !todo in
      add_to_coloring r (Reg c);
      todo := Register.M.remove r new_todo
    | None ->
      begin
        let (r_to_spill, _) = Register.M.choose !todo in
        add_to_coloring r_to_spill (Spilled !nb_to_spill);
        todo := Register.M.remove r_to_spill !todo;
        nb_to_spill := !nb_to_spill + 1;
      end
  done;
  (!coloring, !nb_to_spill)

let test func =
  let fmt = Format.std_formatter in
  let li = Liveness_analysis.liveness func.Ertltree.fun_body in
  let ig = make li in
  let cm = color ig in
  (fprintf fmt "---\nIn func %a\n---\n" Format.pp_print_string func.Ertltree.fun_name;
  print_color (fst cm))
