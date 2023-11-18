
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let rec read_command () =
	let line = read_line () in
	if Str.last_chars line 2 = ";;" then
		Str.string_before line (String.length line - 2) ^ ""
	else
		line ^ " " ^ read_command ()
;;

let top_level_loop () =
	print_endline "Evaluator of lambda expressions...";
	let rec loop (ctx, ctxt) =
		print_string ">> ";
		flush stdout;
	try
		let tm = s token (from_string (read_command ())) in
		loop (execute (ctx, ctxt) tm)
    with
		Lexical_error ->
			print_endline "lexical error";
			loop (ctx, ctxt)
		| Parse_error ->
			print_endline "syntax error";
			loop (ctx, ctxt)
		| Type_error e ->
			print_endline ("type error: " ^ e);
			loop (ctx, ctxt)
		| End_of_file ->
			print_endline "...bye!!!"
  in
	loop (emptydef, emptyctx)
  ;;

top_level_loop ()
;;

