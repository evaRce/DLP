
{
	open Parser;;
	exception Lexical_error;;
}

rule token = parse
	[' ' '\t']  { token lexbuf }
	| "lambda"    { LAMBDA }
	| "L"         { LAMBDA }
	| "true"      { TRUE }
	| "false"     { FALSE }
	| "if"        { IF }
	| "then"      { THEN }
	| "else"      { ELSE }
	| "succ"      { SUCC }
	| "pred"      { PRED }
	| "strhead"   { STRHEAD }
	| "strtail"   { STRTAIL }
	| "iszero"    { ISZERO }
	| "let"       { LET }
	| "letrec"    { LETREC }
	| "in"        { IN }
	| "concat"    { CONCAT }
	| "Bool"      { BOOL }
	| "Nat"       { NAT }
	| "String"    { STRING }
	| "as"        { AS }
	| "nil"       { NIL }
	| "cons"      { CONS }
	| "isnil"     { ISNIL }
	| "head"      { HEAD }
	| "tail"      { TAIL }
	| "List"      { LIST }
	| "case"      { CASE }
	| "of"        { OF }
	| '|'         { PIPE }
	| "=>"        { EARROW }
	| '['         { OBRACKET }
	| ']'         { CBRACKET }
	| '<'         { LVAR }
	| '>'         { RVAR }
	| '('         { LPAREN }
	| ')'         { RPAREN }
	| '{'         { LKEY }
	| '}'         { RKEY }
	| '.'         { DOT }
	| '='         { EQ }
	| ':'         { COLON }
	| ','         { COMMA }
	| "->"        { ARROW }
	| ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
	| ['a'-'z']['a'-'z' '_' '0'-'9']*
					{ IDV (Lexing.lexeme lexbuf) }
	| ['A'-'Z']*['a'-'z' '_' '0'-'9']*
					{ IDT (Lexing.lexeme lexbuf) }
	| '"'[^ '"' ';' '\n']*'"'
					{ let s = Lexing.lexeme lexbuf in
					STRINGV (String.sub s 1 (String.length s - 2)) }
	| eof         { EOF }
	| _           { raise Lexical_error }

