open Ast_typed
open Rtltree
open Ops

let graph = ref Label.M.empty

let generate i =
    let l = Label.fresh () in
    graph := Label.M.add l i !graph;
    l

let rtlbinop_of_binop = function
| TBadd -> Madd
| TBsub -> Msub
| TBmul -> Mmul
| TBdiv -> Mdiv
| _ -> failwith "binop not yet supported"

let rec expr e destr destl = match fst e with
| TEint i ->
  let instr = Econst(Int32.of_int i, destr, destl) in
  generate instr
| TEunop(TUnot, e1) ->
  let instr = Emunop(Msetnei(Int32.of_int 0), destr, destl) in
  expr e1 destr (generate instr)
| TEunop(TUneg, e2) ->
  let r1 = Register.fresh () in
  let instr = Embinop(Msub, r1, destr, destl) in
  let next1 = expr e2 destr (generate instr) in
  expr (TEint 0, Type_int) r1 next1
| TEbinop(TBand, _, _) | TEbinop(TBor, _, _) ->
  failwith "not supported5"
| TEbinop(op, e1, e2) ->
  let r1 = Register.fresh () in
  let instr = Embinop(rtlbinop_of_binop op, r1, destr, destl) in
  let next1 = expr e2 destr (generate instr) in
  expr e1 r1 next1
| TEterm e ->
  expr e destr destl
| TEident _ | TEfetch(_, _) | TEcall(_, _) | TEsizeof _
  -> failwith "not supported4"

let rec stmt s destl retr exitl = match fst s with
| TIret e ->
  expr e retr exitl
| TIblock(blk) ->
  block blk destl retr exitl
| TIexpr e ->
  failwith "not supported3"
| TIvoid | TIif(_, _) | TIifelse(_, _, _) | TIwhile(_, _)
  -> failwith "not supported2"
and block b destl retr exitl = match fst b with
| TBlock(_, instr_lst) ->
  stmt (List.hd instr_lst) destl retr exitl

let deffun fun_decl =
  let retr = Register.fresh () in
  let exitl = Label.fresh () in
  match fun_decl with
  | TDFint(n, _, instr_blk)
  | TDFstruct(_, n, _, instr_blk) ->
    let entryl = block instr_blk exitl retr exitl in
    { fun_name = n;
      fun_formals = [];
      fun_result = retr;
      fun_locals = !(ref Register.S.empty);
      fun_entry = entryl;
      fun_exit = exitl;
      fun_body = !graph }

let defdecl decl = match decl with
| TDF df ->
  deffun df
| _ -> failwith "not supported1"

let transform_to_rtl t_ast =
  { gvars = [];
    funs = List.map defdecl t_ast }
