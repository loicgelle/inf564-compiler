open X86_64
open Format

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
  emit l (movq (imm32 n) (X86_64.operand r)); lin g l1
| _ -> failwith "not implemented yet"

let handle_fun func =
  (* TODO *)
  (* Incorrect function; I wrote it to check compiling*)
  lin func.Ltltree.fun_body func.Ltltree.fun_entry

let transform_to_assembly ltl_file =
  (* TODO *)
  (* Incorrect function; I wrote it to check compiling*)
  handle_fun (List.hd ltl_file.Ltltree.funs)
