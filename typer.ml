open Ast
open Ast_typed

exception Typing_error of (string * (Lexing.position * Lexing.position))

let error s loc = raise (Typing_error(s, loc))

let lvalue = function
| Eident _ | Efetch(_, _) -> true
| _ -> false

let str_of_typ = function
| Type_int -> "int"
| Type_null -> "null"
| Type_void -> "void"
| Type_struct id -> "struct(" ^ id ^ ")"

let str_of_op = function
| Beq -> "="
| Bdbleq -> "=="
| Bneq -> "!="
| Blt -> "<"
| Blte -> "<="
| Bgt -> ">"
| Bgte -> ">="
| Badd -> "+"
| Bsub -> "-"
| Bmul -> "*"
| Bdiv -> "/"
| Band -> "'and'"
| Bor -> "'or'"

let top_of_op = function
| Beq -> TBeq
| Bdbleq -> TBdbleq
| Bneq -> TBneq
| Blt -> TBlt
| Blte -> TBlte
| Bgt -> TBgt
| Bgte -> TBgte
| Badd -> TBadd
| Bsub -> TBsub
| Bmul -> TBmul
| Bdiv -> TBdiv
| Band -> TBand
| Bor -> TBor

(* Compatibility relation bewteen types *)
let are_compat typ1 typ2 = match typ1, typ2 with
| Type_int, Type_int | Type_null, Type_int
| Type_int, Type_null | Type_null, Type_null
    -> true
| Type_null, Type_struct _ | Type_struct _, Type_null
    -> true
| Type_void, Type_struct _ | Type_struct _, Type_void
    -> true
| Type_struct s, Type_struct t when s = t -> true
| _, _ -> false

let check_well_formed gamma tau loc = match tau with
| Type_struct s ->
  begin
    try
      match Hashtbl.find gamma s with
      | Env_struct _ -> ()
      | _ -> error ("type struct " ^ s ^ " not well formed in environment") loc
    with
    | Not_found -> error ("type struct " ^ s ^ " not declared in environment") loc
  end
| _ -> ()

let env_has_identifier gamma name =
  try
    Hashtbl.find gamma name; true
  with
  | Not_found -> false

let rec find_double_item_aux acc = function
| [] -> None
| h::t when List.mem h acc -> Some h
| h::t -> find_double_item_aux (h::acc) t

let find_double_item lst =
  find_double_item_aux [] lst

let rec type_expr gamma expr =
  let loc = snd expr in
  match fst expr with
  | Eint 0 -> (TEint 0, Type_null)
  | Eint i -> (TEint i, Type_int)
  | Efetch(e, x) ->
    begin
      let typed_expr = type_expr gamma e in
      match snd typed_expr with
      | Type_struct id ->
        begin
          try
            match Hashtbl.find gamma id with
            | Env_struct st ->
              begin
                try
                  match Hashtbl.find st x with
                  | Env_var typ -> (TEfetch(typed_expr, x), typ)
                  | _ -> error (x ^ " is not a record of struct " ^ id) loc
                with
                | Not_found -> error ("record " ^ x ^ " is not declared in struct " ^ id) loc
              end
            | _ -> error ("left term should be a structure") loc
          with
          | Not_found -> error ("struct " ^ id ^ " is not declared") loc
        end
      | _ -> error ("left term should be a structure") loc
    end
  | Esizeof id ->
    begin
      try
        match Hashtbl.find gamma id with
        | Env_struct _ -> (TEsizeof id, Type_int)
        | _ -> error ("sizeof expects a structure as parameter") loc
      with
      | Not_found -> error ("struct " ^ id ^ " is not declared") loc
    end
  | Ebinop(Beq, e1, e2) ->
    begin
      if not (lvalue (fst e1)) then
        error "left side of the assignment is not well typed" loc;
      let typed_expr1 = type_expr gamma e1 in
      let typed_expr2 = type_expr gamma e2 in
      let typ1 = (snd typed_expr1) in
      let typ2 = (snd typed_expr2) in
      if are_compat typ1 typ2 then
        (TEbinop(TBeq, typed_expr1, typed_expr2), typ1)
      else
        error ("types " ^ (str_of_typ typ1) ^ " and "
          ^ (str_of_typ typ2) ^ " are incompatible") loc
    end
  | Eunop(Uneg, e) ->
    begin
      let typed_expr = type_expr gamma e in
      let typ = (snd typed_expr) in
      if are_compat typ Type_int then
        (TEunop(TUneg, typed_expr), Type_int)
      else
        error ("operator minus expects int, " ^ (str_of_typ typ) ^ " given") loc
    end
  | Eunop(Unot, e) ->
    begin
      let typed_expr = type_expr gamma e in
        (TEunop(TUnot, typed_expr), Type_int)
    end
  | Ebinop(op, e1, e2) ->
    begin
      let typed_expr1 = type_expr gamma e1 in
      let typed_expr2 = type_expr gamma e2 in
      let typ1 = (snd typed_expr1) in
      let typ2 = (snd typed_expr2) in
      match op with
      | Band | Bor ->
        (TEbinop(top_of_op op, typed_expr1, typed_expr2), Type_int)
      | Badd | Bsub | Bmul | Bdiv ->
        begin
          if are_compat typ1 Type_int && are_compat typ2 Type_int then
            (TEbinop(top_of_op op, typed_expr1, typed_expr2), Type_int)
          else
            error ("operator " ^ (str_of_op op) ^ " expects ints, received "
              ^ (str_of_typ typ1) ^ " and " ^ (str_of_typ typ2)) loc
        end
      | _ ->
        begin
          if are_compat typ1 typ2 then
            (TEbinop(top_of_op op, typed_expr1, typed_expr2), Type_int)
          else
            error ("types " ^ (str_of_typ typ1) ^ " and " ^ (str_of_typ typ2) ^ " are incompatible") loc
        end
    end
  | Ecall(id, el) ->
    begin
      try
        match Hashtbl.find gamma id with
        | Env_fun(fun_type, lst) ->
            (TEcall(id, (aux_check_fun_args gamma id 1 loc el lst)), fun_type)
        | _ -> error (id ^ "is not a function, cannot apply") loc
      with
      | Not_found -> error ("function " ^ id ^ " is not declared") loc
    end
  | Eterm(e) ->
    begin
      let typed_expr = type_expr gamma e in
      let typ = (snd typed_expr) in
      (TEterm(typed_expr), typ)
    end
  | Eident id ->
    try
      match Hashtbl.find gamma id with
      | Env_fun(_, _) ->
          error (id ^ "is a function, not an expression") loc
      | Env_struct _ -> error (id ^ "is a struct, not an expression") loc
      | Env_var t -> TEident id, t
    with
    | Not_found -> error ("term " ^ id ^ " is not declared") loc
and aux_check_fun_args gamma id cnt loc expr_lst spec_lst =
  match expr_lst, spec_lst with
  | (e::te), (s::ts) ->
    begin
      let typed_expr = type_expr gamma e in
      let typ = (snd typed_expr) in
      if are_compat typ s then typed_expr::(aux_check_fun_args gamma id (cnt + 1) loc te ts)
      else error ("argument " ^ (string_of_int cnt) ^ " of function " ^ id
        ^ " should be of type " ^ (str_of_typ s) ^ ", "
        ^ (str_of_typ typ) ^ " given") loc
    end
  | [], [] -> []
  | _, _ -> error ("incorrect number of arguments given to " ^ id) loc

let type_of_decl_var = function
| DVint _, _ -> Type_int
| DVstruct(s, _), _ -> Type_struct s

let type_of_param p = match p with
| Pint _ -> Type_int
| Pstruct(s, _) -> Type_struct s

let extend_env_decl_vars decl_list gamma =
  begin
    let add_var_to_gamma typ var =
      Hashtbl.add gamma var (Env_var typ) in
    let add_to_gamma elem = (
      match elem with
      | DVint lst, _ ->
        List.iter (add_var_to_gamma Type_int) lst
      | DVstruct(s, lst), _ ->
        List.iter (add_var_to_gamma (Type_struct s)) lst
      ) in
    List.iter add_to_gamma decl_list; ()
  end

let new_env_decl_vars decl_list gamma =
  let new_gamma = Hashtbl.copy gamma in
  extend_env_decl_vars decl_list new_gamma; new_gamma

let extend_env_decl_struct decl_struct gamma =
  begin
    match decl_struct with
    | DTstruct(s, varlist), _ ->
      let vardecl = Hashtbl.create (List.length varlist) in
      let add_var_to_gamma typ var = (
        Hashtbl.add vardecl var (Env_var typ)
        ) in
      let add_to_gamma elem = (
        match elem with
        | DVint lst, _ ->
          List.iter (add_var_to_gamma Type_int) lst
        | DVstruct(s, lst), _ ->
          List.iter (add_var_to_gamma (Type_struct s)) lst
        ) in
      List.iter add_to_gamma varlist;
      Hashtbl.add gamma s (Env_struct vardecl); ()
  end

let new_env_decl_struct decl_struct gamma =
  let new_gamma = Hashtbl.copy gamma in
  extend_env_decl_struct decl_struct new_gamma; new_gamma

let new_env_decl_params decl_params gamma =
  begin
    let new_gamma = Hashtbl.copy gamma in
    let add_to_gamma param = (match fst param with
      | Pint n -> Hashtbl.add new_gamma n (Env_var Type_int)
      | Pstruct(n, s) -> Hashtbl.add new_gamma s (Env_var (Type_struct n))
      ) in
    List.iter add_to_gamma decl_params;
    new_gamma;
  end

let extend_env_fun_prototype typ name args gamma =
  begin
    let lstargs = List.map type_of_param args in
    Hashtbl.add gamma name (Env_fun(typ, lstargs))
  end

let new_env_fun_prototype typ name args gamma =
  begin
    let new_gamma = Hashtbl.copy gamma in
    extend_env_fun_prototype typ name args new_gamma;
    new_gamma
  end

let check_double_decl_var loc decl_list =
  let tr1 = (function
  | DVint l, _ -> l
  | DVstruct(_, l), _ -> l) in
  match find_double_item (List.flatten (List.map tr1 decl_list)) with
  | None -> ()
  | Some v -> error ("variable " ^ v ^ " declared twice") loc

let rec type_block gamma tau0 block =
  let loc = snd block in
  match fst block with
  | Block(decl_list, instr_list) ->
    begin
      let check1 elem =
      (check_well_formed gamma (type_of_decl_var elem) loc) in
      List.iter check1 decl_list; (* raises exception if not well formed *)
      check_double_decl_var loc decl_list;
      let new_gamma = new_env_decl_vars decl_list gamma in
      let type_decl elem = match elem with
      | DVint l, _ -> TDVint(l)
      | DVstruct(s, l), _ -> TDVstruct(s, l)
      in
      let new_lst_instr = List.map (type_instr new_gamma tau0) instr_list in
      let new_lst_var = List.map (type_decl) decl_list in
      (TBlock(new_lst_var, new_lst_instr), tau0)
    end
and type_instr gamma tau0 instr =
  let loc = snd instr in
  let return_node a = (a, tau0) in
  match fst instr with
  | Ivoid -> return_node TIvoid
  | Iexpr e ->
    let typed_expr = type_expr gamma e in
    return_node (TIexpr(typed_expr))
  | Iret e ->
    let typed_expr = type_expr gamma e in
    let typ = (snd typed_expr) in
    if are_compat typ tau0 then
      return_node (TIret(typed_expr))
    else error ("instruction has type " ^ (str_of_typ typ)
      ^ ", incompatible with type expected " ^ (str_of_typ tau0)) loc
  | Iifelse(e, i1, i2) ->
    let typed_expr = type_expr gamma e in
    let typed_i1 = type_instr gamma tau0 i1 in
    let typed_i2 = type_instr gamma tau0 i2 in
    return_node (TIifelse(typed_expr, typed_i1, typed_i2))
  | Iif(e, i) ->
    let typed_expr = type_expr gamma e in
    let typed_i = type_instr gamma tau0 i in
    return_node (TIif(typed_expr, typed_i))
  | Iwhile(e, i) ->
    let typed_expr = type_expr gamma e in
    let typed_i = type_instr gamma tau0 i in
    return_node (TIwhile(typed_expr, typed_i))
  | Iblock blk ->
    let typed_block = type_block gamma tau0 blk in
    return_node (TIblock(typed_block))

let typed_dv_of_decl_var = function
| DVint l, _ -> TDVint l
| DVstruct(s, l), _ -> TDVstruct(s, l)

let type_decl_var gamma decl =
  begin
    let loc = snd decl in
    let varlst = (match (fst decl) with
    | DVint l -> l
    | DVstruct(_, l) -> l) in
    let checkvar v = (
      if env_has_identifier gamma v then
        error ("cannot define var " ^ v ^ ", already defined") loc
      ) in
    match find_double_item varlst with
    | Some v -> error ("cannot define variable " ^ v ^ " twice") loc;
    | None ->
      List.iter checkvar varlst;
      check_well_formed gamma (type_of_decl_var decl) loc;
      extend_env_decl_vars [decl] gamma;
      typed_dv_of_decl_var decl;
  end

let type_decl_struct gamma decl =
  begin
    let loc = snd decl in
    let new_gamma = new_env_decl_struct decl gamma in
    let check elem =
      (check_well_formed new_gamma (type_of_decl_var elem) loc) in
    match fst decl with
    | DTstruct(n, dvlist) ->
      if env_has_identifier gamma n then
        error ("cannot define struct " ^ n ^ ", already defined") loc;
      check_double_decl_var loc dvlist;
      List.iter check dvlist; extend_env_decl_struct decl gamma;
      let tdvlist = List.map typed_dv_of_decl_var dvlist in
      TDTstruct(n, tdvlist)
  end

let typed_param_of_param = function
| Pint n, _ -> TPint n
| Pstruct(n, s), _ -> TPstruct(n, s)

let type_decl_fun gamma decl =
  begin
    let loc = snd decl in
    let (typ, name, args, block) = (match fst decl with
    | DFint(n, a, b) -> Type_int, n, a, b
    | DFstruct(s, n, a, b) -> (Type_struct s), n, a, b) in
    if env_has_identifier gamma name then
      error ("cannot define function " ^ name ^ ", already defined") loc;
    let check_param elem =
      (check_well_formed gamma (type_of_param elem) loc) in
    List.iter check_param (List.map (fun (a,b) -> a) args);
    let striped_args = (List.map (fun (a,b) -> a) args) in
    let new_env = new_env_fun_prototype typ name striped_args gamma in
    let test_env = new_env_decl_params args new_env in
    let typed_block = type_block test_env typ block in
    extend_env_fun_prototype typ name striped_args gamma;
    let typed_args = List.map typed_param_of_param args in
    match fst decl with
    | DFint(n, _, _) -> TDFint(n, typed_args, typed_block)
    | DFstruct(s, n, _, _) -> TDFstruct(s, n, typed_args, typed_block)
  end

let type_decl gamma decl = match fst decl with
| DV dv -> TDV (type_decl_var gamma dv)
| DF df -> TDF (type_decl_fun gamma df)
| DT dt -> TDT (type_decl_struct gamma dt)

let init_env () =
  let env = Hashtbl.create 100 in
  extend_env_fun_prototype Type_int "putchar" [Pint "c"] env;
  extend_env_fun_prototype Type_void "sbrk" [Pint "n"] env;
  env

let check_main_fun gamma loc =
  try
    match Hashtbl.find gamma "main" with
    | Env_fun(t, lst) ->
      if not (t = Type_int) then
        error "main function should have return type int" loc;
      if List.length lst > 0 then
        error "main function should take no arguments" loc
    | _ -> error "main should be a function" loc
  with
  | Not_found -> error "function main not found" loc

let type_file file =
  begin
    let env = init_env () in
    let typed_decl_list = List.map (type_decl env) (fst file) in
    check_main_fun env (snd file); typed_decl_list;
  end
