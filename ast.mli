open Lexing

(* Arbres de syntaxe abstraite de Mini-C *)

type ident = string
type loc = Lexing.position * Lexing.position

type expr =
  exprd * loc
and exprd =
  | Eint of int
  | Eident of ident
  | Efetch of expr * ident
  | Ecall of ident * expr list
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Esizeof of ident
  | Eterm of expr
and unop =
  | Uneg
  | Unot
and binop =
  | Beq | Bdbleq | Bneq | Blt | Blte | Bgt | Bgte
  | Badd | Bsub | Bmul | Bdiv | Band | Bor
and file =
  filed * loc
and filed =
  decl list
and decl =
  decld * loc
and decld =
  | DV of decl_vars | DF of decl_fct | DT of decl_typ
and decl_vars =
  decl_varsd * loc
and decl_varsd =
  | DVint of ident list
  | DVstruct of ident * ident list
and decl_fct =
  decl_fctd * loc
and decl_fctd =
  | DFint of ident * param list * block
  | DFstruct of ident * ident * param list * block
and decl_typ =
  decl_typd * loc
and decl_typd =
  | DTstruct of ident * decl_vars list
and param =
  paramd * loc
and paramd =
  | Pint of ident
  | Pstruct of ident * ident
and instr =
  instrd * loc
and instrd =
  | Ivoid
  | Iexpr of expr
  | Iif of expr * instr
  | Iifelse of expr * instr * instr
  | Iwhile of expr * instr
  | Iblock of block
  | Iret of expr
and block =
  blockd * loc
and blockd =
  | Block of decl_vars list * instr list
