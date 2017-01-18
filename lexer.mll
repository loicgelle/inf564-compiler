
(* Analyseur lexical pour Mini-Python *)

{
  open Lexing
  open Ast
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
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let string_buffer = Buffer.create 1024
}

let space = ' ' | '\t' | '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*

let octal_digit = ['0'-'7']
let hexa_digit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']
let char  = " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "[" | "]" | "^" | "_" | "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "{" | "|" | "}" | "~" | ""
          | "\\\\" | "\\\'" | "\\\"" | "\\x" (hexa_digit) (hexa_digit)

let integer = '0'
            | ['0'-'9'] (digit)*
            | '0' (octal_digit)+
            | "0x" (hexa_digit)+
            | "\\\'" char "\\\'"

let r1  = " " | "!" | "#" | "$" | "%" | "&" | "(" | "+" | "," | "-" | "." | "/" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "[" | "]" | "^" | "_" | "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "{" | "|" | "}" | "~" | ""
          | "\\\\" | "\\\'" | "\\\"" | "\\x" (hexa_digit) (hexa_digit)

let r2  = " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "+" | "," | "-" | "." | "/" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "[" | "]" | "^" | "_" | "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "{" | "|" | "}" | "~" | ""
          | "\\\\" | "\\\'" | "\\\"" | "\\x" (hexa_digit) (hexa_digit)

let comment = "//" [^'\n']*
            | '(' '*' ('*'* r1 | r2)* '*' '*'* ')'

rule next_tokens = parse
  | '\n'
      { newline lexbuf; [] }
  | (space | comment)+
      { next_tokens lexbuf }
  | ident as id { [id_or_kwd id] }
  | '+'     { [PLUS] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | '/'     { [DIV] }
  | '='     { [EQUAL] }
  | "!"     { [EXCL] }
  | "<"     { [LT] }
  | ">"     { [GT] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | '['     { [LB] }
  | ']'     { [RB] }
  | ','     { [COMMA] }
  | ';'     { [SEMICOLON] }
  | '&'     { [AMP] }
  | '|'     { [VB] }
  | integer as s
            { try [CST (Cint (int_of_string s))]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '"'     { [CST (Cstring (string lexbuf))] }
  | eof     { [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and indentation = parse
  | (space | comment)* '\n'
      { newline lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

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
