
(* Analyseur lexical pour Mini-C *)

{
  open Lexing
  open Parser

  exception Lexing_error of string

  let kwd_tbl = ["if", IF; "else", ELSE;
		 "return", RETURN; "while", WHILE;
     "sizeof", SIZEOF; "int", INT;
     "struct", STRUCT]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = 0 }
}

let space = ' ' | '\t' | '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*

let octal_digit = ['0'-'7']
let hexa_digit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']
let char  = " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+"
          | "," | "-" | "." | "/" | "0" | "1" | "2" | "3" | "4" | "5"
          | "6" | "7" | "8" | "9" | ":" | ";" | "<" | "=" | ">" | "?"
          | "@" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
          | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S"
          | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "[" | "]" | "^"
          | "_" | "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h"
          | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
          | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "{" | "|"
          | "}" | "~" | "" | "\\\\" | "\\\'" | "\\\""
          | "\\x" (hexa_digit) (hexa_digit)
let integer = '0'
            | ['0'-'9'] (digit)*
            | '0' (octal_digit)+
            | "0x" (hexa_digit)+
let integerChar = "\'" char "\'"

let comment = "//" [^'\n']*
            | '/' '*' ('*'* char | '\n')* '*' '*'* '/'

rule next_tokens = parse
  | '\n'
      { newline lexbuf; next_tokens lexbuf }
  | (space | comment)+
      { next_tokens lexbuf }
  | '*'     { [STAR] }
  | '-'     { [MINUS] }
  | ident as id { [id_or_kwd id] }
  | '+'     { [PLUS] }
  | '/'     { [DIV] }
  | '='     { [EQUAL] }
  | "=="    { [DBLEQ] }
  | "!="    { [NEQ] }
  | "!"     { [EXCL] }
  | "<"     { [LT] }
  | ">"     { [GT] }
  | "<="    { [LTE] }
  | ">="    { [GTE] }
  | "->"    { [RIGHTARROW] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | '{'     { [LB] }
  | '}'     { [RB] }
  | ','     { [COMMA] }
  | ';'     { [SEMICOLON] }
  | "&&"    { [AND] }
  | "||"    { [OR] }
  | integer as s
            { try [INTEG (int_of_string s)]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | integerChar as c
            { try [INTEG (Char.code c.[1])]
              with _ -> raise (Lexing_error ("unable to cast to int: " ^ c)) }
  | eof     { [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

{

  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
