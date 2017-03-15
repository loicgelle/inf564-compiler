open Ertltree
open Format

type live_info = {
         instr: instr;
          succ: Label.t list;    (* successeurs *)
  mutable pred: Label.set;       (* prédécesseurs *)
          defs: Register.set;    (* définitions *)
          uses: Register.set;    (* utilisations *)
  mutable  ins: Register.set;    (* variables vivantes en entrée *)
  mutable outs: Register.set;    (* variables vivantes en sortie *)
}

(* Global variables *)
let info_tbl = Hashtbl.create 100
let ws = ref Label.S.empty

let fill_tbl_init cfg =
  let handle_instr key instr =
    begin
      ws := Label.S.add key !ws;
      let defs_uses = def_use instr in
      let info = {
        instr = instr;
        succ = succ instr;
        pred = Label.S.empty;
        defs = Register.S.of_list (fst defs_uses);
        uses = Register.S.of_list (snd defs_uses);
        ins = Register.S.empty;
        outs = Register.S.empty} in
      Hashtbl.add info_tbl key info
    end in
  Label.M.iter handle_instr cfg

let fill_tbl_pred () =
  let add_as_pred pred succ =
    let succ_info = Hashtbl.find info_tbl succ in
    succ_info.pred <- Label.S.add pred succ_info.pred in
  let handle_instr key info =
    List.iter (add_as_pred key) info.succ in
  Hashtbl.iter handle_instr info_tbl

let compute_new_out label =
  let union_label_ins set l =
    let il = Hashtbl.find info_tbl l in Register.S.union set il.ins in
  let info = Hashtbl.find info_tbl label in
  let succ = info.succ in
  List.fold_left union_label_ins Register.S.empty succ

let fill_tbl_kildall () =
  while (not (Label.S.is_empty !ws)) do
    begin
      let l = Label.S.choose !ws in
      let info_l = Hashtbl.find info_tbl l in
      let old_in = info_l.ins in
      let new_out = compute_new_out l in
      let new_in = Register.S.union info_l.uses (Register.S.diff new_out info_l.defs) in
      info_l.ins <- new_in;
      info_l.outs <- new_out;
      if (not (Register.S.equal old_in new_in)) then ws := Label.S.union !ws info_l.pred;
      ws := Label.S.remove l !ws
    end
  done

let build_info_map () =
  let info_map = ref Label.M.empty in
  let handle_instr key info =
    info_map := Label.M.add key info !info_map in
  Hashtbl.iter handle_instr info_tbl;
  !info_map

let print_set = Register.print_set

let print_live_info fmt l i li =
  fprintf fmt "%a: %a@ --> d={%a}@ u={%a}@ i={%a}@ o={%a}\n"
    Label.print l print_instr i
    print_set li.defs print_set li.uses print_set li.ins print_set li.outs

let liveness cfg =
  fill_tbl_init cfg;
  fill_tbl_pred ();
  fill_tbl_kildall ();
  build_info_map ()
