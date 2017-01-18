
/* Analyseur syntaxique pour Mini-Python */

%{
  open Ast
%}

%token <int> INTEG
%token <string> IDENT
%token <Ast.constant> CST
%token IF ELSE RETURN WHILE SIZEOF STRUCT INT
%token EOF
%token LP RP COMMA EQUAL SEMICOLON STAR LB RB
%token PLUS MINUS TIMES DIV
%token GT LT EXCL AMP VB

/* D�finitions des priorit�s et associativit�s des tokens */

/*
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus
%nonassoc LSQ
*/

/* Point d'entr�e de la grammaire */
%start file

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.file> file

%%

file:
| dl = decl*; EOF
  { dl, NullExpr }
;

decl:
| e = decl_vars
  { e, NullExpr }
| e = decl_typ
  { e, NullExpr }
| e = decl_fct
  { e, NullExpr }

starident:
| STAR; x = ident
  { x, NullExpr }

decl_vars:
| INT; x = separated_nonempty_list(COMMA, ident); SEMICOLON
  { x, NullExpr }
| STRUCT; x = ident; y = separated_list(COMMA, starident); SEMICOLON
  { x, NullExpr }

decl_typ:
| STRUCT; x = ident; LB; y = decl_vars*; RB; SEMICOLON
  { x, y, NullExpr }

decl_fct:
| INT; x = ident; LP; y = separated_list(COMMA, param); RP; b = block
  { x, y, b, NullExpr }
| STRUCT; x = ident; STAR; y = ident; LP; z = separated_list(COMMA, param); RP; b = block
  { x, z, b, NullExpr } (* TODO : UTILISER Y *)

param:
| INT; x = ident
  { x, NullExpr }
| STRUCT; x = ident; STAR; y = ident
  { x, y, NullExpr }

expr:
| x = INTEG
  { x, NullExpr }
| x = ident
  { x, NullExpr }
| x = expr; MINUS; GT; y = ident
  { x, y, NullExpr }
| x = ident; LP; y = separated_list(COMMA, param); RP
  { x, y, NullExpr }
| EXCL; x = expr
  { x, NullExpr }
| MINUS; x = expr
  { x, NullExpr }
| x = expr; y = operator; z = expr
  { x, y, z, NullExpr }
| SIZEOF; LP; STRUCT; x = ident; RP
  { x, NullExpr }
| LB; x = expr; RB
  { x, NullExpr }

operator:
| EQUAL
  { NullExpr }
| EQUAL; EQUAL
  { NullExpr }
| EXCL; EQUAL
  { NullExpr }
| LT
  { NullExpr }
| LT ; EQUAL
  { NullExpr }
| GT
  { NullExpr }
| GT; EQUAL
  { NullExpr }
| PLUS
  { NullExpr }
| MINUS
  { NullExpr }
| TIMES
  { NullExpr }
| DIV
  { NullExpr }
| AMP; AMP (* && *)
  { NullExpr }
| VB; VB (* || *)
  { NullExpr }

instruction:
| SEMICOLON
  { NullExpr }
| x = expr; SEMICOLON
  { x, NullExpr }
| IF; LP; x = expr; RP; y = instruction
  { x, y, NullExpr }
| IF; LP; x = expr; RP; y = instruction; ELSE; z = instruction
  { x, y, z, NullExpr }
| WHILE; LP; x = expr; RP; y = instruction
  { x, y, NullExpr }
| x = block
  { x, NullExpr }
| RETURN; x = expr; SEMICOLON
  { x, NullExpr }

block:
| LB; x = decl_vars*; y = instruction*; RB
  { x, y }

ident:
  id = IDENT { id }
;
