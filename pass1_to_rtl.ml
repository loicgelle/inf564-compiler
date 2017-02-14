open Ast_typed
open Rtltree
open Ops

(* Global tables *)
let graph = ref Label.M.empty
let local_env = Hashtbl.create 10
let struct_env = Hashtbl.create 10
let var_struct_type = Hashtbl.create 10

(* Helper functions *)
let generate i =
    let l = Label.fresh () in
    graph := Label.M.add l i !graph;
    l

let get_struct_shift ids idv =
  let rec aux i v = function
  | h::_ when h = v -> i
  | h::t -> aux (i+1) v t
  | [] -> failwith "var is not declared in struct" in
  let typ = Hashtbl.find var_struct_type ids in
  let lst = Hashtbl.find struct_env typ in
  8 * (aux 0 idv lst)

let rtlbinop_of_binop = function
| TBadd -> Madd
| TBsub -> Msub
| TBmul -> Mmul
| TBdiv -> Mdiv
| TBdbleq -> Msete
| TBneq -> Msetne
| TBlt -> Msetl
| TBlte -> Msetle
| TBgt -> Msetg
| TBgte -> Msetge
| _ -> failwith "binop or and and not supported in expression"

let add_var_to_env id =
  let r = Register.fresh () in
  Hashtbl.add local_env id r

let handle_decl_var = function
| TDVint l | TDVstruct(_, l) ->
  List.iter add_var_to_env l

let handle_var_struct_type = function
| TDVint _ -> ()
| TDVstruct(typ, l) ->
  List.iter (fun a -> Hashtbl.add var_struct_type a typ) l

let param_to_reg = function
| TPint(id) | TPstruct(_, id) ->
  let r = Register.fresh () in
  Hashtbl.add local_env id r;
  r

let rec decl_var_lst_to_lst acc = function
| [] -> List.rev acc
| (TDVint l)::h -> decl_var_lst_to_lst ((List.rev l)@acc) h
| (TDVstruct(_, l))::h -> decl_var_lst_to_lst ((List.rev l)@acc) h

let map_ignore_none mapfun lst =
  let rec aux acc = function
  | [] -> acc
  | h::t ->
    (match mapfun h with
      | None -> aux acc t
      | Some new_h -> aux (new_h::acc) t)
  in List.rev (aux [] lst)

(* Typed AST handlers *)
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
| TEbinop(TBeq, (TEfetch((TEident ids, _), idv), _), e2) ->
  begin
    try
      let r = Hashtbl.find local_env ids in
      let r_e = Register.fresh () in
      let instr = Estore(r_e, r, get_struct_shift ids idv, destl) in
      let nextl = generate instr in
      expr e2 r_e nextl
    with
    | Not_found ->
      let r = Register.fresh () in
      let r_e = Register.fresh () in
      let instr = Estore(r_e, r, get_struct_shift ids idv, destl) in
      let nextl = generate instr in
      let instrl = Eaccess_global(ids, r, nextl) in
      let fstl = generate instrl in
      expr e2 r_e fstl
  end
| TEbinop(TBeq, (TEident id, _), e2) ->
  begin
    try
      let r = Hashtbl.find local_env id in
      expr e2 r destl
    with
    | Not_found ->
      let instr = Eassign_global(destr, id, destl) in
      let nextl = generate instr in
      expr e2 destr nextl
  end
| TEbinop(op, e1, e2) ->
  let r1 = Register.fresh () in
  let instr = Embinop(rtlbinop_of_binop op, r1, destr, destl) in
  let next1 = expr e2 destr (generate instr) in
  expr e1 r1 next1
| TEterm e ->
  expr e destr destl
| TEident id ->
  begin
    try
      let r = Hashtbl.find local_env id in
      let instr = Embinop(Mmov, r, destr, destl) in
      generate instr
    with
    | Not_found ->
      let instr = Eaccess_global(id, destr, destl) in
      generate instr
  end
| TEcall(id, e_lst) ->
  let reglst = List.map (fun _ -> Register.fresh ()) e_lst in
  let instr = Ecall(destr, id, reglst, destl) in
  let calll = generate instr in
  args_to_instr calll reglst e_lst
| TEfetch((TEident ids, _), idv) ->
  begin
    try
      let r = Hashtbl.find local_env ids in
      let instr = Eload(r, get_struct_shift ids idv, destr, destl) in
      generate instr
    with
    | Not_found ->
      let r = Register.fresh () in
      let instrl = Eload(r, get_struct_shift ids idv, destr, destl) in
      let nextl = generate instrl in
      let instrg = Eaccess_global(ids, r, nextl) in
      generate instrg
  end
| TEfetch(_, _) ->
  failwith "left hanside is not a struct"
| TEsizeof ids ->
  let s = 8 * List.length (Hashtbl.find struct_env ids) in
  let instr = Econst(Int32.of_int s, destr, destl) in
  generate instr
and args_to_instr nextl regl = function
| [] -> nextl
| h::t -> args_to_instr (expr h (List.hd regl) nextl) (List.tl regl) t

let rec condition e truel falsel = match fst e with
| TEbinop(op, e1, e2) ->
  begin
    match op with
    | TBand ->
      condition e1 (condition e2 truel falsel) falsel
    | TBor ->
      condition e1 truel (condition e2 truel falsel)
    | TBlt ->
      let r1 = Register.fresh () in
      let r2 = Register.fresh () in
      expr e1 r1
      (expr e2 r2
      (generate (Embbranch (Mjl, r1, r2, truel, falsel))))
    | TBlte ->
      let r1 = Register.fresh () in
      let r2 = Register.fresh () in
      expr e1 r1
      (expr e2 r2
      (generate (Embbranch (Mjle, r1, r2, truel, falsel))))
    | TBgt ->
      let r1 = Register.fresh () in
      let r2 = Register.fresh () in
      expr e1 r1
      (expr e2 r2
      (generate (Embbranch (Mjle, r2, r1, truel, falsel))))
    | TBgte ->
      let r1 = Register.fresh () in
      let r2 = Register.fresh () in
      expr e1 r1
      (expr e2 r2
      (generate (Embbranch (Mjl, r2, r1, truel, falsel))))
    | _ ->
      let r = Register.fresh () in
      expr e r
      (generate (Emubranch (Mjnz, r, falsel, truel)))
  end
| _ ->
  let r = Register.fresh () in
  expr e r
  (generate (Emubranch (Mjnz, r, falsel, truel)))

let rec handle_instr destl retr exitl = function
| h::[] ->
  stmt h destl retr exitl
| h::t ->
  let nextl = stmt h destl retr exitl in
  handle_instr nextl retr exitl t
| [] ->
  destl
and stmt s destl retr exitl = match fst s with
| TIret e ->
  expr e retr exitl
| TIblock(blk) ->
  block blk destl retr exitl
| TIexpr e ->
  let r = Register.fresh () in
  expr e r destl
| TIvoid ->
  destl
| TIif(e, b1) ->
  condition e (stmt b1 destl retr exitl) destl
| TIifelse(e, b1, b2) ->
  condition e (stmt b1 destl retr exitl) (stmt b2 destl retr exitl)
| TIwhile(e, b1) ->
  begin
    let l = Label.fresh () in
    let testl = condition e (stmt b1 l retr exitl) destl in
    graph := Label.M.add l (Egoto testl) !graph ;
    testl
  end
and block b destl retr exitl = match fst b with
| TBlock(dv_lst, instr_lst) ->
  (List.iter handle_decl_var dv_lst;
  List.iter handle_var_struct_type dv_lst;
  handle_instr destl retr exitl (List.rev instr_lst))

let deffun fun_decl =
  Hashtbl.reset local_env;
  let retr = Register.fresh () in
  let exitl = Label.fresh () in
  match fun_decl with
  | TDFint(n, tpl, instr_blk)
  | TDFstruct(_, n, tpl, instr_blk) ->
    let entryl = block instr_blk exitl retr exitl in
    let reglst = List.map param_to_reg tpl in
    { fun_name = n;
      fun_formals = reglst;
      fun_result = retr;
      fun_locals = !(ref Register.S.empty);
      fun_entry = entryl;
      fun_exit = exitl;
      fun_body = !graph }

let handle_decl_struct = function
| TDTstruct(typ, dvl) ->
  Hashtbl.add struct_env typ (decl_var_lst_to_lst [] dvl)

let defdecl decl = match decl with
| TDF df ->
  Some(deffun df)
| TDV dv -> handle_var_struct_type dv; None
| TDT ds -> handle_decl_struct ds; None

let transform_to_rtl t_ast =
  { gvars = [];
    funs = map_ignore_none defdecl t_ast }
