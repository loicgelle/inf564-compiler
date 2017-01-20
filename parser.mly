
/* Analyseur syntaxique pour Mini-Python */

%{
  open Ast
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
%left OR AND DBLEQ NEQ LT LTE GT GTE PLUS MINUS STAR DIV
%right EXCL unary_minus
%left RIGHTARROW

/* Point d'entr�e de la grammaire */
%start file

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| df = decl*; EOF
  { df }
;

decl:
| d = decl_vars
  { DV d }
| d = decl_typ
  { DT d }
| d = decl_fct
  { DF d }

starident:
| STAR; x = ident
  { x }

decl_vars:
| INT; l = separated_nonempty_list(COMMA, ident); SEMICOLON
  { DVint l }
| STRUCT; id = ident; l = separated_nonempty_list(COMMA, starident); SEMICOLON
  { DVstruct(id, l) }

decl_typ:
| STRUCT; id = ident; LB; dv = decl_vars*; RB; SEMICOLON
  { DTstruct(id, dv) }

decl_fct:
| INT; id = ident; LP; l = separated_list(COMMA, param); RP; b = block
  { DFint(id, l, b) }
| STRUCT; id1 = ident; STAR; id2 = ident; LP; l = separated_list(COMMA, param); RP; b = block
  { DFstruct(id1, id2, l, b) }

param:
| INT; id = ident
  { Pint id }
| STRUCT; id1 = ident; STAR; id2 = ident
  { Pstruct(id1, id2) }

expr:
| x = INTEG
  { Eint x }
| id = ident
  { Eident id }
| e = expr; RIGHTARROW; id = ident
  { Efetch(e, id) }
| id = ident; LP; l = separated_list(COMMA, expr); RP
  { Ecall(id, l) }
| EXCL; e = expr
  { Eunop(Unot, e) }
| MINUS; e = expr %prec unary_minus
  { Eunop(Uneg, e) }
| e1 = expr; op = operator; e2 = expr
  { Ebinop(op, e1, e2) }
| SIZEOF; LP; STRUCT; id = ident; RP
  { Esizeof id }
| LP; e = expr; RP
  { Eterm e }

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

simple_instr:
| SEMICOLON
  { Ivoid }
| e = expr; SEMICOLON
  { Iexpr e }
| RETURN; e = expr; SEMICOLON
  { Iret e }

suite_ctrl:
| s = simple_instr
  { s }
| b = block
  { Iblock b }

instruction:
| s = simple_instr
  { s }
| IF; LP; e = expr; RP; s1 = suite_ctrl; ELSE; s2 = suite_ctrl
  { Iifelse(e, s1, s2) }
| IF; LP; e = expr; RP; s = suite_ctrl
  { Iif(e, s) }
| WHILE; LP; e = expr; RP; s = suite_ctrl
  { Iwhile(e, s) }

block:
| LB; l1 = decl_vars*; l2 = instruction*; RB
  { Block(l1, l2) }

ident:
  id = IDENT { id }
;
