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
      emit (Label.fresh ()) (sete (operand r2)); lin g l1
    | Msetg ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setg (operand r2)); lin g l1
    | Msetge ->
      emit l (subq (operand r1) (operand r2));
      emit (Label.fresh ()) (setge (operand r2)); lin g l1
  end
| Ltltree.Epush_param(r, l1) ->
  emit l (pushq (operand r)); lin g l1
| _ -> failwith "not implemented yet"

let handle_fun func =
  (* TODO *)
  (* Incorrect function; I wrote it to check compiling *)
  lin func.Ltltree.fun_body func.Ltltree.fun_entry

let transform_to_assembly ltl_file =
  (* TODO *)
  (* Incorrect function; I wrote it to check compiling *)
  handle_fun (List.hd ltl_file.Ltltree.funs)
