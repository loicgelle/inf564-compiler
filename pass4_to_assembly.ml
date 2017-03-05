open X86_64
open Format
open Ops

let visited = Hashtbl.create 17
type instr = Code of X86_64.text | Label of Label.t
let code = ref []
let emit l i = code := Code i :: Label l :: !code
let emit_wl i = code := Code i :: !code
let labels = Hashtbl.create 17
let need_label l = Hashtbl.add labels l ()

let rec lin g l =
  if not (Hashtbl.mem visited l) then begin
    Hashtbl.add visited l ();
    instr g l (Label.M.find l g)
  end else begin
    need_label l;
    emit_wl (jmp (l :> string))
  end
and instr g l = function
| Ltltree.Econst (n, r, l1) ->
  emit l (movq (imm32 n) (operand r)); lin g l1
| Ltltree.Emunop(op, r, l1) ->
  begin
    match op with
    | Maddi(i) -> emit l (addq (imm32 i) (operand r)); lin g l1
    | Msetei(i) ->
      emit l (subq (imm32 i) (operand r));
      emit (Label.fresh ()) (sete (operand r)); lin g l1
    | Msetnei(i) ->
      emit l (subq (imm32 i) (operand r));
      emit (Label.fresh ()) (setne (operand r)); lin g l1
  end
|	Ltltree.Embinop(op, r1, r2, l1) ->
  begin
    match op with
    | Mmov -> emit l (movq (operand r1) (operand r2)); lin g l1
    | Madd -> emit l (addq (operand r1) (operand r2)); lin g l1
    | Msub -> emit l (subq (operand r1) (operand r2)); lin g l1
    | Mmul -> emit l (imulq (operand r1) (operand r2)); lin g l1
    | Mdiv -> emit l (idivq (operand r2)); lin g l1
    | Msete ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (sete (operand r2)); lin g l1
    | Msetne ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setne (operand r2)); lin g l1
    | Msetl ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setl (operand r2)); lin g l1
    | Msetle ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setle (operand r2)); lin g l1
    | Msetg ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setg (operand r2)); lin g l1
    | Msetge ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setge (operand r2)); lin g l1
  end
| Ltltree.Epush_param(r, l1) ->
  emit l (pushq (operand r)); lin g l1
| Ltltree.Ereturn ->
  emit l ret
| Ltltree.Ecall(id, l1) ->
  emit l (call ("_" ^ id)); lin g l1
| Ltltree.Egoto(l1) ->
  if Hashtbl.mem visited l1 then emit l (jmp (l1 :> string))
  else lin g l1
| Ltltree.Emubranch(op, r, l1_1, l1_2) ->
  let reg = Ltltree.Reg r in
  if not (Hashtbl.mem visited l1_2) then begin
    let newl = Label.fresh () in
    match op with
    | Mjz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (je (l1_1 :> string)))
    |	Mjnz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (jne (l1_1 :> string)))
    |	Mjlei(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jle (l1_1 :> string)))
    |	Mjgi(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jg (l1_1 :> string)));
    lin g l1_2; lin g l1_1
  end
  else if not (Hashtbl.mem visited l1_1) then begin
    let newl = Label.fresh () in
    match op with
    | Mjz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (jne (l1_2 :> string)))
    |	Mjnz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (je (l1_2 :> string)))
    |	Mjlei(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jg (l1_2 :> string)))
    |	Mjgi(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jle (l1_2 :> string)));
    lin g l1_1; lin g l1_2
  end
  else begin
    let newl = Label.fresh () in
    match op with
    | Mjz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (je (l1_1 :> string)))
    |	Mjnz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (jne (l1_1 :> string)))
    |	Mjlei(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jle (l1_1 :> string)))
    |	Mjgi(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jg (l1_1 :> string)));
    emit (Label.fresh ()) (jmp (l1_2 :> string))
  end
| Ltltree.Embbranch(op, r1, r2, l1_1, l1_2) ->
  let reg1 = Ltltree.Reg r1 in
  let reg2 = Ltltree.Reg r2 in
  begin
    emit l (cmpq (operand reg2) (operand reg1));
    if not (Hashtbl.mem visited l1_2) then begin
      let newl = Label.fresh () in
      match op with
      | Mjl -> (emit newl (jl (l1_1 :> string)))
      | Mjle -> (emit newl (jle (l1_1 :> string)));
      lin g l1_2; lin g l1_1
    end
    else if not (Hashtbl.mem visited l1_1) then begin
      let newl = Label.fresh () in
      match op with
      | Mjl -> (emit newl (jge (l1_2 :> string)))
      | Mjle -> (emit newl (jg (l1_2 :> string)));
      lin g l1_1; lin g l1_2
    end
    else begin
      let newl = Label.fresh () in
      match op with
      | Mjl -> emit newl (jl (l1_1 :> string))
      |	Mjle -> emit newl (jle (l1_1 :> string));
      emit (Label.fresh ()) (jmp (l1_2 :> string))
    end
  end
| Ltltree.Eaccess_global(id, r, l1) ->
  emit l (movq (ilab id) (operand (Ltltree.Reg r))); lin g l1
| Ltltree.Eassign_global(r, id, l1) ->
  emit l (movq (operand (Ltltree.Reg r)) (ilab id)); lin g l1
| Ltltree.Eload(r1, i, r2, l1) ->
  emit l (movq (shifted_register r1 i) (operand (Ltltree.Reg r2))); lin g l1
| Ltltree.Estore(r1, r2, i, l1) ->
  emit l (movq (operand (Ltltree.Reg r1)) (shifted_register r2 i)); lin g l1

let handle_fun asm func =
  (*let filter_func = function
  | Code _ -> true
  | Label l when Hashtbl.mem labels l -> true
  | _ -> false in*)
  let map_func = function
  | Code t -> t
  | Label l -> label (l :> string) in
  begin
    code := [];
    lin func.Ltltree.fun_body func.Ltltree.fun_entry;
    let new_code = List.map map_func (List.rev !code) in
    (*let new_code = List.map map_func (List.rev (List.filter filter_func !code)) in*)
    List.fold_left (++) (label ("_" ^ func.Ltltree.fun_name)) new_code
  end

let build_data_segment ds =
  let data = ref [] in
  let add_global_var v =
    data := (dquad [0])::(label v)::(!data) in
  let rec concat acc = function
  | h1::h2::t when acc = None ->
    concat (Some ((++) h2 h1)) t
  | h::t ->
    begin match acc with
    | Some d -> concat (Some ((++) h d)) t
    | None -> failwith "data segment list length should be even"
    end
  | [] -> acc in
  begin
    List.iter add_global_var ds;
    match concat None !data with
    | None -> nop
    | Some d -> d
  end

let transform_to_assembly ltl_file =
  let func_text = List.fold_left handle_fun nop ltl_file.Ltltree.funs in
  let full_text = (++) (globl "_main") func_text in
  let data_segment = build_data_segment ltl_file.Ltltree.gvars in
  { text = full_text;
    data = data_segment }
