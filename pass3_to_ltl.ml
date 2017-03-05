open Ltltree
open Ops

type frame = {
  f_params: int; (* taille pour les paramètres + adresse retour *)
  f_locals: int; (* taille pour les variables locales *)
}

(* Global variables *)
let graph = ref (Label.M.empty)

let lookup c r =
  if Register.is_hw r then Reg r else Register.M.find r c

let is_reg c r = match lookup c r with
| Reg _ -> true
| _ -> false

let have_same_color c r1 r2 = match lookup c r1 with
| Reg c1 ->
  begin
    match lookup c r2 with
    | Reg c2 when c1 = c2 -> true
    | _ -> false
  end
| _ -> false

let push_instr label instr =
  graph := Label.M.add label instr !graph

let handle_instr c frame inpl instr =
  let spill_index_to_shift i = 8 * (frame.f_locals - i) in
  let param_index_to_shift i = 8 * (frame.f_locals + 1 + i) in
  let compute_shift = function
  | Reg r -> Reg r
  | Spilled n -> Spilled (spill_index_to_shift n) in
  match instr with
  |	Ertltree.Egoto(l) -> push_instr inpl (Egoto(l))
  |	Ertltree.Ereturn -> push_instr inpl Ereturn
  |	Ertltree.Ecall(id, i, l) -> push_instr inpl (Ecall(id, l))
  | Ertltree.Econst(n, r, l) ->
    push_instr inpl (Econst (n, compute_shift (lookup c r), l))
  | Ertltree.Eload(r1, i, r2, l) ->
    begin
      match lookup c r1, lookup c r2 with
      | Reg reg1, Reg reg2 -> push_instr inpl (Eload(reg1, i, reg2, l))
      | Reg reg1, Spilled index2 ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(reg1, i, Register.tmp1, nextl));
        push_instr nextl (Estore(Register.tmp1, Register.rsp, spill_index_to_shift index2, l));)
      | Spilled index1, Reg reg2 ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl));
        push_instr nextl (Eload(Register.tmp1, i, reg2, l)))
      | Spilled index1, Spilled index2 ->
        let nextl1 = Label.fresh () in
        let nextl2 = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl1));
        push_instr nextl1 (Eload(Register.tmp1, i, Register.tmp2, nextl2));
        push_instr nextl2 (Estore(Register.tmp2, Register.rsp, spill_index_to_shift index2, l));)
    end
  | Ertltree.Estore(r1, r2, i, l) ->
    begin
      match lookup c r1, lookup c r2 with
      | Reg reg1, Reg reg2 -> push_instr inpl (Estore(reg1, reg2, i, l))
      | Reg reg1, Spilled index2 ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index2, Register.tmp1, nextl));
        push_instr nextl (Estore(reg1, Register.tmp1, i, l));)
      | Spilled index1, Reg reg2 ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl));
        push_instr nextl (Estore(Register.tmp1, reg2, i, l));)
      | Spilled index1, Spilled index2 ->
        let nextl1 = Label.fresh () in
        let nextl2 = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl1));
        push_instr nextl1 (Eload(Register.rsp, spill_index_to_shift index2, Register.tmp2, nextl2));
        push_instr nextl2 (Estore(Register.tmp1, Register.tmp2, i, l));)
    end
  | Ertltree.Embinop(Mmov, r1, r2, l) when have_same_color c r1 r2 ->
    push_instr inpl (Egoto l)
  | Ertltree.Embinop(Mmul, r1, r2, l) when not (is_reg c r2) ->
    begin
      match lookup c r2 with
      | Spilled index2 ->
          let shift_index2 = spill_index_to_shift index2 in
          let nextl1 = Label.fresh () in
          let nextl2 = Label.fresh () in
          (push_instr inpl (Eload(Register.rsp, shift_index2, Register.tmp1, nextl1));
          push_instr nextl1 (Embinop(Mmul, lookup c r1, Reg Register.tmp1, nextl2));
          push_instr nextl2 (Estore(Register.tmp1, Register.rsp, shift_index2, l)))
      | _ -> failwith "r2 should not be a register"
    end
  | Ertltree.Embinop(op, r1, r2, l) ->
    begin
      let c_reg2 = lookup c r2 in
      match lookup c r1 with
      | Reg reg1 -> push_instr inpl (Embinop(op, Reg reg1, c_reg2, l))
      | Spilled index1 ->
          let nextl = Label.fresh () in
          (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl));
          push_instr nextl (Embinop(op, Reg Register.tmp1, c_reg2, l)))
    end
  | Ertltree.Ealloc_frame l | Ertltree.Edelete_frame l ->
    let m = frame.f_locals in
    if m = 0 then push_instr inpl (Egoto l)
    else
      begin
        let nextl = Label.fresh () in
        let shift =
          match instr with
          | Ertltree.Ealloc_frame _ -> Int32.of_int (-8 * (m+1))
          | Ertltree.Edelete_frame _ -> Int32.of_int (8 * (m+1))
          | _ -> Int32.zero in
        (push_instr inpl (Econst(shift, Reg Register.tmp1, nextl));
        push_instr nextl (Embinop(Madd, Reg Register.tmp1, Reg Register.rsp, l)))
      end
  | Ertltree.Eget_param(i, r, l) ->
    begin
      match lookup c r with
      | Reg reg -> push_instr inpl (Eload(Register.rsp, 8 * i, reg, l))
      | Spilled index ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, param_index_to_shift i, Register.tmp1, nextl));
        push_instr nextl (Estore(Register.tmp1, Register.rsp, spill_index_to_shift index, l)))
    end
  | Ertltree.Epush_param(r, l) -> push_instr inpl (Epush_param(lookup c r, l))
  | Ertltree.Emunop(op, r, l) -> push_instr inpl (Emunop(op, lookup c r, l))
  | Ertltree.Emubranch(op, r, l1, l2) ->
    begin
      match lookup c r with
      | Reg reg -> push_instr inpl (Emubranch(op, reg, l1, l2))
      | Spilled index ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index, Register.tmp1, nextl));
        push_instr nextl (Emubranch(op, Register.tmp1, l1, l2)))
    end
  | Ertltree.Embbranch(op, r1, r2, l1, l2) ->
    begin
      match lookup c r1, lookup c r2 with
      | Reg reg1, Reg reg2 -> push_instr inpl (Embbranch(op, reg1, reg2, l1, l2))
      | Reg reg1, Spilled index2 ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index2, Register.tmp1, nextl));
        push_instr nextl (Embbranch(op, reg1, Register.tmp1, l1, l2)))
      | Spilled index1, Reg reg2 ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl));
        push_instr nextl (Embbranch(op, Register.tmp1, reg2, l1, l2)))
      | Spilled index1, Spilled index2 ->
        let nextl1 = Label.fresh () in
        let nextl2 = Label.fresh () in
        (push_instr inpl (Eload(Register.rsp, spill_index_to_shift index1, Register.tmp1, nextl1));
        push_instr nextl1 (Eload(Register.rsp, spill_index_to_shift index2, Register.tmp2, nextl2));
        push_instr nextl2 (Embbranch(op, Register.tmp1, Register.tmp2, l1, l2)))
    end
  | Ertltree.Eaccess_global(id, r, l) ->
    begin
      match lookup c r with
      | Reg reg -> push_instr inpl (Eaccess_global(id, reg, l))
      | Spilled index ->
        let nextl = Label.fresh () in
        (push_instr inpl (Eaccess_global(id, Register.tmp1, nextl));
        push_instr nextl (Estore(Register.tmp1, Register.rsp, spill_index_to_shift index, l)))
    end
  | Ertltree.Eassign_global(r, id, l) ->
    begin
      match lookup c r with
      | Reg reg -> push_instr inpl (Eassign_global(reg, id, l))
      | Spilled index ->
        let nextl = Label.fresh () in
        (push_instr nextl (Eload(Register.rsp, spill_index_to_shift index, Register.tmp1, l));
        push_instr inpl (Eassign_global(Register.tmp1, id, nextl)))
    end

let handle_fun_graph c frame g =
  Label.M.iter (handle_instr c frame) g

let handle_deffun df =
  graph := Label.M.empty;
  let liveness_info = Liveness_analysis.liveness df.Ertltree.fun_body in
  let interference_graph = Interference_graph.make liveness_info in
  let (coloring, nb_to_spill) = Interference_graph.color interference_graph in
  handle_fun_graph coloring { f_params = df.Ertltree.fun_formals + 1; f_locals = nb_to_spill } df.Ertltree.fun_body;
  { fun_name = df.Ertltree.fun_name;
    fun_entry = df.Ertltree.fun_entry;
    fun_body = !graph }

let transform_to_ltl ertl_file =
  { gvars = ertl_file.Ertltree.gvars;
    funs = List.map handle_deffun ertl_file.Ertltree.funs }
