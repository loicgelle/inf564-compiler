open Ertltree
open Ops

(* Global variables *)
let graph = ref (Label.M.empty)

let push_instr label instr =
  graph := Label.M.add label instr !graph

let rec save_infos_from_label l lst1 lst2 = match lst1, lst2 with
| [], _ -> l
| h::t, [] ->
  let nextl = Label.fresh () in
  (push_instr l (Epush_param(h, nextl));
  save_infos_from_label nextl t [])
| h1::t1, h2::t2 ->
  let nextl = Label.fresh () in
  (push_instr l (Embinop(Mmov, h1, h2, nextl));
  save_infos_from_label nextl t1 t2)

let rec save_infos_then_goto nextl lst1 lst2 = match lst1, lst2 with
| [], _ -> nextl
| h::t, [] ->
  let l = Label.fresh () in
  (push_instr l (Epush_param(h, nextl));
  save_infos_then_goto l t [])
| h1::t1, h2::t2 ->
  let l = Label.fresh () in
  (push_instr l (Embinop(Mmov, h1, h2, nextl));
  save_infos_then_goto l t1 t2)

let rec get_infos_then_goto nextl lst1 lst2 = match lst1, lst2 with
| _, [] -> nextl
| [], h::t ->
  let l = Label.fresh () in
  (push_instr l (Eget_param(List.length lst2, h, nextl));
  get_infos_then_goto l [] t)
  (* IMPORTANT note: if n (nb of args) > 6, then the given
  index to Egetparam is n - i + 1
  if we denote by i the index of the param in x1, ..., xn *)
| h1::t1, h2::t2 ->
  let l = Label.fresh () in
  (push_instr l (Embinop(Mmov, h1, h2, nextl));
  get_infos_then_goto l t1 t2)



let handle_instr inp_l = function
| Rtltree.Econst(n, r, l) ->
  push_instr inp_l (Econst(n, r, l))
| Rtltree.Eaccess_global(i, r, l) ->
  push_instr inp_l (Eaccess_global(i, r, l))
| Rtltree.Eassign_global(r, i, l) ->
  push_instr inp_l (Eassign_global(r, i, l))
| Rtltree.Eload(r1, i, r2, l) ->
  push_instr inp_l (Eload(r1, i, r2, l))
| Rtltree.Estore(r1, r2, i, l) ->
  push_instr inp_l (Estore(r1, r2, i, l))
| Rtltree.Emunop(op, r, l) ->
  push_instr inp_l (Emunop(op, r, l))
| Rtltree.Embinop(Mdiv, r1, r2, l) ->
  let l1 = Label.fresh () in
  let l2 = Label.fresh () in
  (push_instr inp_l (Embinop(Mmov, r1, Register.rax, l1));
  push_instr l1 (Embinop(Mdiv, Register.rax, r2, l2));
  push_instr l2 (Embinop(Mmov, Register.rax, r2, l)))
| Rtltree.Embinop(op, r1, r2, l) ->
  push_instr inp_l (Embinop(op, r1, r2, l))
| Rtltree.Emubranch(op, r, l1, l2) ->
  push_instr inp_l (Emubranch(op, r, l1, l2))
| Rtltree.Embbranch(op, r1, r2, l1, l2) ->
  push_instr inp_l (Embbranch(op, r1, r2, l1, l2))
| Rtltree.Egoto l ->
  push_instr inp_l (Egoto l)
| Rtltree.Ecall(r, f, rl, l) ->
  let n = List.length rl in
  let nb_params = min n 6 in
  let nextl1 = save_infos_from_label inp_l rl Register.parameters in
  let nextl2 = Label.fresh () in
  let nextl3 = (if n > 6 then Label.fresh () else l) in
  begin
    push_instr nextl1 (Ecall(f, nb_params, nextl2));
    push_instr nextl2 (Embinop(Mmov, Register.result, r, nextl3));
    if (n > 6) then
      begin
        let nb_oct = 8 * (n - 6) in
        let nextl4 = Label.fresh () in
        let rc = Register.fresh () in
        push_instr nextl3 (Econst(Int32.of_int nb_oct, rc, nextl4));
        push_instr nextl4 (Embinop(Msub, rc, Register.rsp, l))
      end
  end
let handle_fun_graph g =
  Label.M.iter handle_instr g

let handle_deffun df =
  let newentryl = Label.fresh () in
  let saved_registers = List.map (fun _ -> Register.fresh ()) Register.callee_saved in
  (graph := Label.M.empty;
  (let nextl2 = get_infos_then_goto df.Rtltree.fun_entry Register.parameters df.Rtltree.fun_formals in
  let nextl1 = save_infos_then_goto nextl2 Register.callee_saved saved_registers in
  push_instr newentryl (Ealloc_frame(nextl1)));
  handle_fun_graph df.Rtltree.fun_body;
  (let nextl2 = Label.fresh () in
  let nextl3 = Label.fresh () in
  let nextl1 = save_infos_then_goto nextl2 saved_registers Register.callee_saved in
  (push_instr nextl3 Ereturn;
  push_instr nextl2 (Edelete_frame(nextl3)));
  push_instr df.Rtltree.fun_exit (Embinop(Mmov, df.Rtltree.fun_result, Register.rax, nextl1))));
  { fun_name = df.Rtltree.fun_name;
    fun_formals = List.length df.Rtltree.fun_formals;
    fun_locals = df.Rtltree.fun_locals;
    fun_entry = newentryl;
    fun_body = !graph }

let transform_to_ertl rtl_file =
  { gvars = rtl_file.Rtltree.gvars;
    funs = List.map handle_deffun rtl_file.Rtltree.funs }
