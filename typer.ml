open Ast
open Ast_typed

exception Typing_error of string

let error s loc = raise (Typing_error s)

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
| _, _ -> false

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
  | Ebinop(Beq, e1, e2) when lvalue (fst e1) ->
    begin
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
          if typ1 = Type_int && typ2 = Type_int then
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
  | Eident id -> TEident id, Type_null
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
