type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | HEAD
  | TAIL
  | ISZERO
  | LET
  | LETREC
  | IN
  | CONCAT
  | BOOL
  | NAT
  | STRING
  | LPAREN
  | RPAREN
  | LKEY
  | RKEY
  | DOT
  | EQ
  | COLON
  | COMMA
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.action
