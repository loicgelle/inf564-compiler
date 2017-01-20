
(* Arbres de syntaxe abstraite de Mini-C *)

type ident = string

type expr =
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
and file = decl list
and decl =
  | DV of decl_vars | DF of decl_fct | DT of decl_typ
and decl_vars =
  | DVint of ident list
  | DVstruct of ident * ident list
and decl_fct =
  | DFint of ident * param list * block
  | DFstruct of ident * ident * param list * block
and decl_typ =
  | DTstruct of ident * decl_vars list
and param =
  | Pint of ident
  | Pstruct of ident * ident
and instr =
  | Ivoid
  | Iexpr of expr
  | Iif of expr * instr
  | Iifelse of expr * instr * instr
  | Iwhile of expr * instr
  | Iblock of block
  | Iret of expr
and block =
  | Block of decl_vars list * instr list
