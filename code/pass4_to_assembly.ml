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

let rec replace_label g l =
  match Label.M.find l g with
  | Ltltree.Egoto(l1) -> replace_label g l1
  | _ -> need_label l; (l :> string)

let rec lin g l =
  if not (Hashtbl.mem visited l) then begin
    Hashtbl.add visited l ();
    instr g l (Label.M.find l g)
  end else begin
    emit_wl (jmp (replace_label g l))
  end
and instr g l = function
| Ltltree.Econst (n, r, l1) when (Int32.compare n Int32.zero) = 0 ->
  (match r with
  | Ltltree.Reg _ -> emit l (xorq (operand r) (operand r)); lin g l1
  | Ltltree.Spilled _ -> emit l (movq (imm32 n) (operand r)); lin g l1)
| Ltltree.Econst (n, r, l1) ->
  emit l (movq (imm32 n) (operand r)); lin g l1
| Ltltree.Emunop(op, r, l1) ->
  begin
    match op with
    | Maddi(i) -> emit l (addq (imm32 i) (operand r)); lin g l1
    | Msetei(i) ->
      emit l (cmpq (imm32 i) (operand r));
      emit (Label.fresh ()) (sete (operand_to_8bit r));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r) (operand r)); lin g l1
    | Msetnei(i) ->
      emit l (cmpq (imm32 i) (operand r));
      emit (Label.fresh ()) (setne (operand_to_8bit r));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r) (operand r)); lin g l1
  end
|	Ltltree.Embinop(op, r1, r2, l1) ->
  begin
    match op with
    | Mmov -> emit l (movq (operand r1) (operand r2)); lin g l1
    | Madd -> emit l (addq (operand r1) (operand r2)); lin g l1
    | Msub -> emit l (subq (operand r1) (operand r2)); lin g l1
    | Mmul -> emit l (imulq (operand r1) (operand r2)); lin g l1
    | Mdiv -> emit l cqto; emit (Label.fresh ()) (idivq (operand r1)); lin g l1
    | Mand | Mor ->
      emit l (cmpq (imm32 Int32.zero) (operand r1));
      emit (Label.fresh ()) (setne (operand_to_8bit r1));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r1) (operand r1));
      emit (Label.fresh ()) (cmpq (imm32 Int32.zero) (operand r2));
      emit (Label.fresh ()) (setne (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2));
      (match op with
        | Mand -> emit (Label.fresh ()) (andq (operand r1) (operand r2))
        | Mor -> emit (Label.fresh ()) (orq (operand r1) (operand r2))
        | _ -> failwith "not handled here");
      lin g l1
    | Msete ->
      emit l (cmpq (operand r1) (operand r2));
      emit (Label.fresh ()) (sete (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2)); lin g l1
    | Msetne ->
      emit l (cmpq (operand r1) (operand r2));
      emit (Label.fresh ()) (setne (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2)); lin g l1
    | Msetl ->
      emit l (cmpq (operand r1) (operand r2));
      emit (Label.fresh ()) (setl (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2)); lin g l1
    | Msetle ->
      emit l (cmpq (operand r1) (operand r2));
      emit (Label.fresh ()) (setle (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2)); lin g l1
    | Msetg ->
      emit l (cmpq (operand r1) (operand r2));
      emit (Label.fresh ()) (setg (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2)); lin g l1
    | Msetge ->
      emit l (cmpq (operand r1) (operand r2));
      emit (Label.fresh ()) (setge (operand_to_8bit r2));
      emit (Label.fresh ()) (movzbq (operand_to_8bit r2) (operand r2)); lin g l1
  end
| Ltltree.Epush_param(r, l1) ->
  emit l (pushq (operand r)); lin g l1
| Ltltree.Ereturn ->
  emit l ret
| Ltltree.Ecall(id, l1) ->
  emit l (call id); lin g l1
| Ltltree.Egoto(l1) ->
  lin g l1
| Ltltree.Emubranch(op, r, l1_1, l1_2) ->
  let reg = Ltltree.Reg r in
  if not (Hashtbl.mem visited l1_2) then begin
    let newl = Label.fresh () in
    (match op with
    | Mjz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (je (replace_label g l1_1)); need_label l1_1)
    |	Mjnz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (jne (replace_label g l1_1)); need_label l1_1)
    |	Mjlei(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jle (replace_label g l1_1)); need_label l1_1)
    |	Mjgi(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jg (replace_label g l1_1)); need_label l1_1));
    lin g l1_2; lin g l1_1
  end
  else if not (Hashtbl.mem visited l1_1) then begin
    let newl = Label.fresh () in
    (match op with
    | Mjz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (jne (replace_label g  l1_2)); need_label l1_2)
    |	Mjnz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (je (replace_label g  l1_2)); need_label l1_2)
    |	Mjlei(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jg (replace_label g  l1_2)); need_label l1_2)
    |	Mjgi(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jle (replace_label g  l1_2)); need_label l1_2));
    lin g l1_1; lin g l1_2
  end
  else begin
    let newl = Label.fresh () in
    (match op with
    | Mjz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (je (replace_label g l1_1)))
    |	Mjnz ->
      (emit l (cmpq (imm32 Int32.zero) (operand reg));
      emit newl (jne (replace_label g l1_1)))
    |	Mjlei(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jle (replace_label g l1_1)))
    |	Mjgi(i) ->
      (emit l (cmpq (imm32 i) (operand reg));
      emit newl (jg (replace_label g l1_1))));
    lin g l1_2; lin g l1_1; need_label l1_1; need_label l1_2
  end
| Ltltree.Embbranch(op, r1, r2, l1_1, l1_2) ->
  let reg1 = Ltltree.Reg r1 in
  let reg2 = Ltltree.Reg r2 in
  begin
    emit l (cmpq (operand reg2) (operand reg1));
    if not (Hashtbl.mem visited l1_2) then begin
      let newl = Label.fresh () in
      (match op with
      | Mjl -> (emit newl (jl (replace_label g l1_1)))
      | Mjle -> (emit newl (jle (replace_label g l1_1))));
      lin g l1_2; lin g l1_1; need_label l1_1
    end
    else if not (Hashtbl.mem visited l1_1) then begin
      let newl = Label.fresh () in
      (match op with
      | Mjl -> (emit newl (jge (replace_label g l1_2)))
      | Mjle -> (emit newl (jg (replace_label g l1_2))));
      lin g l1_1; lin g l1_2; need_label l1_2
    end
    else begin
      let newl = Label.fresh () in
      (match op with
      | Mjl -> emit newl (jl (replace_label g l1_1))
      |	Mjle -> emit newl (jle (replace_label g l1_1)));
      lin g l1_2; lin g l1_1; need_label l1_1; need_label l1_2
    end
  end
| Ltltree.Eaccess_global(id, r, l1) ->
  emit l (movq (lab id) (operand (Ltltree.Reg r))); lin g l1
| Ltltree.Eassign_global(r, id, l1) ->
  emit l (movq (operand (Ltltree.Reg r)) (lab id)); lin g l1
| Ltltree.Eload(r1, i, r2, l1) ->
  emit l (movq (shifted_register r1 i) (operand (Ltltree.Reg r2))); lin g l1
| Ltltree.Estore(r1, r2, i, l1) ->
  emit l (movq (operand (Ltltree.Reg r1)) (shifted_register r2 i)); lin g l1

let handle_fun asm func =
  let filter_func = function
  | Code _ -> true
  | Label l when Hashtbl.mem labels l -> true
  | _ -> false in
  let map_func = function
  | Code t -> t
  | Label l -> label (l :> string) in
  begin
    code := [];
    lin func.Ltltree.fun_body func.Ltltree.fun_entry;
    (*let new_code = List.map map_func (List.rev !code) in*)
    let new_code = List.map map_func (List.rev (List.filter filter_func !code)) in
    let new_asm = List.fold_left (++) (label func.Ltltree.fun_name) new_code in
    (++) asm new_asm
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
  let full_text = (++) (globl "main") func_text in
  let data_segment = build_data_segment ltl_file.Ltltree.gvars in
  { text = full_text;
    data = data_segment }
