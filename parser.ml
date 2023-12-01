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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 39 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* HEAD *);
  266 (* TAIL *);
  267 (* ISZERO *);
  268 (* LET *);
  269 (* LETREC *);
  270 (* IN *);
  271 (* CONCAT *);
  272 (* BOOL *);
  273 (* NAT *);
  274 (* STRING *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* LKEY *);
  278 (* RKEY *);
  279 (* DOT *);
  280 (* EQ *);
  281 (* COLON *);
  282 (* COMMA *);
  283 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INTV *);
  285 (* IDV *);
  286 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\004\000\004\000\007\000\007\000\007\000\007\000\007\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\004\000\002\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\002\000\003\000\002\000\003\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\001\000\
\001\000\003\000\003\000\001\000\001\000\001\000\003\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\017\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\021\000\034\000\000\000\000\000\008\000\000\000\
\019\000\000\000\009\000\010\000\011\000\012\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\015\000\
\000\000\000\000\000\000\000\000\014\000\016\000\000\000\022\000\
\000\000\028\000\029\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\031\000\
\005\000\026\000\004\000\006\000\000\000\033\000\000\000\007\000"

let yydgoto = "\002\000\
\020\000\036\000\022\000\063\000\023\000\037\000\056\000\064\000"

let yysindex = "\005\000\
\015\255\000\000\229\254\000\000\000\000\072\255\010\255\010\255\
\010\255\010\255\010\255\231\254\234\254\010\255\072\255\072\255\
\000\000\241\254\000\000\000\000\010\000\010\255\000\000\246\254\
\000\000\016\255\000\000\000\000\000\000\000\000\000\000\008\255\
\021\255\010\255\027\255\023\255\013\255\072\255\000\000\000\000\
\078\255\072\255\072\255\078\255\000\000\000\000\072\255\000\000\
\049\000\000\000\000\000\000\000\078\255\078\255\032\255\034\255\
\057\255\055\255\046\255\000\000\000\000\051\255\060\255\050\255\
\072\255\078\255\072\255\072\255\072\255\000\000\078\255\000\000\
\000\000\000\000\000\000\000\000\074\255\000\000\072\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\067\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\255\
\000\000\000\000\000\000\000\000\000\000\000\000\068\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\223\255\043\000\045\000\000\000\027\000"

let yytablesize = 287
let yytable = "\021\000\
\019\000\024\000\003\000\032\000\026\000\001\000\033\000\055\000\
\038\000\039\000\059\000\004\000\005\000\035\000\041\000\003\000\
\004\000\005\000\006\000\062\000\042\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\015\000\014\000\016\000\043\000\
\074\000\015\000\048\000\016\000\049\000\017\000\025\000\019\000\
\057\000\058\000\017\000\018\000\019\000\044\000\046\000\047\000\
\061\000\027\000\028\000\029\000\030\000\031\000\065\000\025\000\
\034\000\025\000\025\000\025\000\066\000\025\000\067\000\073\000\
\040\000\075\000\076\000\077\000\068\000\069\000\070\000\072\000\
\003\000\004\000\005\000\006\000\045\000\080\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\071\000\014\000\079\000\
\024\000\032\000\015\000\060\000\016\000\050\000\051\000\052\000\
\053\000\078\000\054\000\017\000\025\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\019\000\000\000\000\000\000\000\003\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\000\000\000\000\019\000\000\000\019\000\003\000\000\000\
\003\000\000\000\000\000\003\000\019\000\019\000\019\000"

let yycheck = "\001\000\
\000\000\029\001\000\000\029\001\006\000\001\000\029\001\041\000\
\024\001\000\000\044\000\002\001\003\001\015\000\025\001\001\001\
\002\001\003\001\004\001\053\000\005\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\019\001\015\001\021\001\024\001\
\066\000\019\001\022\001\021\001\038\000\028\001\029\001\030\001\
\042\000\043\000\028\001\029\001\030\001\025\001\020\001\025\001\
\000\000\007\000\008\000\009\000\010\000\011\000\023\001\020\001\
\014\000\022\001\023\001\024\001\027\001\026\001\006\001\065\000\
\022\000\067\000\068\000\069\000\014\001\024\001\020\001\022\001\
\001\001\002\001\003\001\004\001\034\000\079\000\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\026\001\015\001\014\001\
\022\001\022\001\019\001\047\000\021\001\016\001\017\001\018\001\
\019\001\071\000\021\001\028\001\029\001\030\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\255\255\255\255\019\001\255\255\021\001\020\001\255\255\
\022\001\255\255\255\255\025\001\028\001\029\001\030\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  HEAD\000\
  TAIL\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  CONCAT\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  LPAREN\000\
  RPAREN\000\
  LKEY\000\
  RKEY\000\
  DOT\000\
  EQ\000\
  COLON\000\
  COMMA\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 47 "parser.mly"
    ( Bind (_1, _3))
# 255 "parser.ml"
               : Lambda.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 49 "parser.mly"
      ( Eval _1 )
# 262 "parser.ml"
               : Lambda.action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 53 "parser.mly"
  ( _1 )
# 269 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 55 "parser.mly"
  ( TmIf (_2, _4, _6) )
# 278 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
  ( TmAbs (_2, _4, _6) )
# 287 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
  ( TmLetIn (_2, _4, _6) )
# 296 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
  ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 306 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 65 "parser.mly"
  ( _1 )
# 313 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
  ( TmSucc _2 )
# 320 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
  ( TmPred _2 )
# 327 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
  ( TmHead _2)
# 334 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
  ( TmTail _2)
# 341 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
  ( TmIsZero _2 )
# 348 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
  ( TmConcat (_2, _3) )
# 356 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
  ( TmApp (_1, _2) )
# 364 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 83 "parser.mly"
  ( _2 )
# 371 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
  ( TmTrue )
# 377 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
  ( TmFalse )
# 383 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
  (TmVar _1)
# 390 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
  ( let rec f = function
				0 -> TmZero
			| n -> TmSucc (f (n-1))
			in f _1 )
# 400 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
  ( TmString _1 )
# 407 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TmSequence) in
    Obj.repr(
# 98 "parser.mly"
  ( TmTuple _2 )
# 414 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TmSequence) in
    Obj.repr(
# 102 "parser.mly"
  ( _1::_3 )
# 422 "parser.ml"
               : 'TmSequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 104 "parser.mly"
  (	[_1] )
# 429 "parser.ml"
               : 'TmSequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 108 "parser.mly"
  ( _1 )
# 436 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 110 "parser.mly"
  ( TyArr (_1, _3) )
# 444 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 114 "parser.mly"
  ( _2 )
# 451 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
  ( TyBool )
# 457 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
  ( TyNat )
# 463 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
  ( TyString )
# 469 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TySequence) in
    Obj.repr(
# 122 "parser.mly"
  ( TyTuple _2 )
# 476 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 126 "parser.mly"
   ( [_1] )
# 483 "parser.ml"
               : 'TySequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TySequence) in
    Obj.repr(
# 128 "parser.mly"
   ( _1::_3 )
# 491 "parser.ml"
               : 'TySequence))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.action)
