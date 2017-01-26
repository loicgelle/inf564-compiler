
(* Arbres de syntaxe abstraite typée de Mini-C *)

type tident = string

type typ =
| Type_int
| Type_struct of string
| Type_void
| Type_null

type env = (string, env_decl) Hashtbl.t
and env_decl =
  | Env_var of typ
  | Env_struct of env
  | Env_fun of typ * typ list

type texpr =
  texprd * typ
and texprd =
  | TEint of int
  | TEident of tident
  | TEfetch of texpr * tident
  | TEcall of tident * texpr list
  | TEunop of tunop * texpr
  | TEbinop of tbinop * texpr * texpr
  | TEsizeof of tident
  | TEterm of texpr
and tunop =
  | TUneg
  | TUnot
and tbinop =
  | TBeq | TBdbleq | TBneq | TBlt | TBlte | TBgt | TBgte
  | TBadd | TBsub | TBmul | TBdiv | TBand | TBor
and tfile =
  tdecl list
and tdecl =
  | TDV of tdecl_vars | TDF of tdecl_fct | TDT of tdecl_typ
and tdecl_vars =
  | TDVint of tident list
  | TDVstruct of tident * tident list
and tdecl_fct =
  | TDFint of tident * tparam list * tblock
  | TDFstruct of tident * tident * tparam list * tblock
and tdecl_typ =
  | TDTstruct of tident * tdecl_vars list
and tparam =
  | TPint of tident
  | TPstruct of tident * tident
and tinstr =
  tinstrd * typ
and tinstrd =
  | TIvoid
  | TIexpr of texpr
  | TIif of texpr * tinstr
  | TIifelse of texpr * tinstr * tinstr
  | TIwhile of texpr * tinstr
  | TIblock of tblock
  | TIret of texpr
and tblock =
  tblockd * typ
and tblockd =
  | TBlock of tdecl_vars list * tinstr list
