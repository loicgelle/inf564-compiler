
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
  { NullExpr }
;

decl:
| e = decl_vars
  { NullExpr }
| e = decl_typ
  { NullExpr }
| e = decl_fct
  { NullExpr }

starident:
| STAR; x = ident
  { NullExpr }

decl_vars:
| INT; x = separated_nonempty_list(COMMA, ident); SEMICOLON
  { NullExpr }
| STRUCT; x = ident; y = separated_list(COMMA, starident); SEMICOLON
  { NullExpr }

decl_typ:
| STRUCT; x = ident; LB; y = decl_vars*; RB; SEMICOLON
  { NullExpr }

decl_fct:
| INT; x = ident; LP; y = separated_list(COMMA, param); RP; b = block
  { NullExpr }
| STRUCT; x = ident; STAR; y = ident; LP; z = separated_list(COMMA, param); RP; b = block
  { NullExpr }

param:
| INT; x = ident
  { NullExpr }
| STRUCT; x = ident; STAR; y = ident
  { NullExpr }

expr:
| x = INTEG
  { NullExpr }
| x = ident
  { NullExpr }
| x = expr; MINUS; GT; y = ident
  { NullExpr }
| x = ident; LP; y = separated_list(COMMA, param); RP
  { NullExpr }
| EXCL; x = expr
  { NullExpr }
| MINUS; x = expr
  { NullExpr }
| x = expr; y = operator; z = expr
  { NullExpr }
| SIZEOF; LP; STRUCT; x = ident; RP
  { NullExpr }
| LB; x = expr; RB
  { NullExpr }

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
  { NullExpr }
| IF; LP; x = expr; RP; y = instruction
  { NullExpr }
| IF; LP; x = expr; RP; y = instruction; ELSE; z = instruction
  { NullExpr }
| WHILE; LP; x = expr; RP; y = instruction
  { NullExpr }
| x = block
  { NullExpr }
| RETURN; x = expr; SEMICOLON
  { NullExpr }

block:
| LB; x = decl_vars*; y = instruction*; RB
  { NullExpr }

ident:
  id = IDENT { id }
;
