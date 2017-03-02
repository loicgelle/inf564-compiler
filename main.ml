
(* Programme principal *)

open Format
open Lexing
open Parser
open Typer
open Pass1_to_rtl

let usage = "usage: compiler [options] file.c"

let parse_only = ref false
let type_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, "  stop after typing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.next_token lb in
    close_in c;
    if !parse_only then exit 0;
    let typed_ast = Typer.type_file f in
    if !type_only then exit 0;
    let rtl_file = Pass1_to_rtl.transform_to_rtl typed_ast in
    let ertl_file = Pass2_to_ertl.transform_to_ertl rtl_file in
    let ltl_file = Pass3_to_ltl.transform_to_ltl ertl_file in
    Pass4_to_assembly.transform_to_assembly ltl_file
  with
    | Typer.Typing_error(s, (l1, l2)) ->
  report (l1, l2);
  eprintf "typing error: %s@." s;
  exit 1
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
