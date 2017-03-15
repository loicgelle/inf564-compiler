
/* Analyseur syntaxique pour Mini-C */

%{
  open Ast

  let loc_wrapper elem sp ep =
    (elem, (sp, ep))
%}

%token <int> INTEG
%token <string> IDENT
%token IF ELSE RETURN WHILE SIZEOF STRUCT INT
%token EOF
%token LP RP COMMA EQUAL SEMICOLON LB RB RIGHTARROW
%token PLUS MINUS STAR DIV
%token GT GTE LT LTE EXCL AND OR DBLEQ NEQ

/* D�finitions des priorit�s et associativit�s des tokens */

%right EQUAL
%left OR AND DBLEQ NEQ LT LTE GT GTE PLUS MINUS
%left STAR DIV
%right EXCL unary_minus
%left RIGHTARROW

/* Point d'entr�e de la grammaire */
%start file

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| df = decl*; EOF
  { loc_wrapper df $startpos $endpos }
;

decl:
| d = decl_vars
  { loc_wrapper (DV d) $startpos $endpos }
| d = decl_typ
  { loc_wrapper (DT d) $startpos $endpos }
| d = decl_fct
  { loc_wrapper (DF d) $startpos $endpos }

starident:
| STAR; x = ident
  { x }

decl_vars:
| INT; l = separated_nonempty_list(COMMA, ident); SEMICOLON
  { loc_wrapper (DVint l) $startpos $endpos }
| STRUCT; id = ident; l = separated_nonempty_list(COMMA, starident); SEMICOLON
  { loc_wrapper (DVstruct(id, l)) $startpos $endpos }

decl_typ:
| STRUCT; id = ident; LB; dv = decl_vars*; RB; SEMICOLON
  { loc_wrapper (DTstruct(id, dv)) $startpos $endpos }

decl_fct:
| INT; id = ident; LP; l = separated_list(COMMA, param); RP; b = block
  { loc_wrapper (DFint(id, l, b)) $startpos $endpos }
| STRUCT; id1 = ident; STAR; id2 = ident; LP; l = separated_list(COMMA, param); RP; b = block
  { loc_wrapper (DFstruct(id1, id2, l, b)) $startpos $endpos }

param:
| INT; id = ident
  { loc_wrapper (Pint id) $startpos $endpos }
| STRUCT; id1 = ident; STAR; id2 = ident
  { loc_wrapper (Pstruct(id1, id2)) $startpos $endpos }

expr:
| x = INTEG
  { loc_wrapper (Eint x) $startpos $endpos }
| id = ident
  { loc_wrapper (Eident id) $startpos $endpos }
| e = expr; RIGHTARROW; id = ident
  { loc_wrapper (Efetch(e, id)) $startpos $endpos }
| id = ident; LP; l = separated_list(COMMA, expr); RP
  { loc_wrapper (Ecall(id, l)) $startpos $endpos }
| EXCL; e = expr
  { loc_wrapper (Eunop(Unot, e)) $startpos $endpos }
| MINUS; e = expr %prec unary_minus
  { loc_wrapper (Eunop(Uneg, e)) $startpos $endpos }
| e1 = expr; op = operator; e2 = expr
  { loc_wrapper (Ebinop(op, e1, e2)) $startpos $endpos }
| SIZEOF; LP; STRUCT; id = ident; RP
  { loc_wrapper (Esizeof id) $startpos $endpos }
| LP; e = expr; RP
  { loc_wrapper (Eterm e) $startpos $endpos }

%inline operator:
| EQUAL
  { Beq }
| DBLEQ
  { Bdbleq }
| NEQ
  { Bneq }
| LT
  { Blt }
| LTE
  { Blte }
| GT
  { Bgt }
| GTE
  { Bgte }
| PLUS
  { Badd }
| MINUS
  { Bsub }
| STAR
  { Bmul }
| DIV
  { Bdiv }
| AND
  { Band }
| OR
  { Bor }

instruction:
| SEMICOLON
  { loc_wrapper Ivoid $startpos $endpos }
| e = expr; SEMICOLON
  { loc_wrapper (Iexpr e) $startpos $endpos }
| RETURN; e = expr; SEMICOLON
  { loc_wrapper (Iret e) $startpos $endpos }
| IF; LP; e = expr; RP; s1 = instruction; ELSE; s2 = instruction
  { loc_wrapper (Iifelse(e, s1, s2)) $startpos $endpos }
| IF; LP; e = expr; RP; s = instruction
  { loc_wrapper (Iif(e, s)) $startpos $endpos }
| WHILE; LP; e = expr; RP; s = instruction
  { loc_wrapper (Iwhile(e, s)) $startpos $endpos }
| b = block
  { loc_wrapper (Iblock b) $startpos $endpos }

block:
| LB; l1 = decl_vars*; l2 = instruction*; RB
  { loc_wrapper (Block(l1, l2)) $startpos $endpos }

ident:
  id = IDENT { id }
;
