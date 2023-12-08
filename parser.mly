
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token STRHEAD
%token STRTAIL
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token BOOL
%token NAT
%token STRING
%token AS
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token LIST
%token CASE
%token OF

%token PIPE
%token EARROW
%token OBRACKET
%token CBRACKET
%token LVAR
%token RVAR
%token LPAREN
%token RPAREN
%token LKEY
%token RKEY
%token DOT
%token EQ
%token COLON
%token COMMA
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.action> s

%%

s :
	term EOF
		{ Eval $1 }
  	| IDV EQ term EOF
  		{ Bind ($1, $3) }
	| IDT EQ ty EOF
		{ BindTy ($1, $3) }
	| IDT EOF
		{ EvalTy $1}

term :
    appTerm
		{ $1 }
	| IF term THEN term ELSE term
		{ TmIf ($2, $4, $6) }
	| LAMBDA IDV COLON ty DOT term
		{ TmAbs ($2, $4, $6) }
	| LET IDV EQ term IN term
		{ TmLetIn ($2, $4, $6) }
	| LETREC IDV COLON ty EQ term IN term
		{ TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
	accessTerm
		{ $1 }
	| SUCC accessTerm
		{ TmSucc $2 }
	| PRED accessTerm
		{ TmPred $2 }
	| STRHEAD accessTerm
		{ TmStrHead $2}
	| STRTAIL accessTerm
		{ TmStrTail $2}
	| ISZERO accessTerm
		{ TmIsZero $2 }
	| CONCAT accessTerm accessTerm
		{ TmConcat ($2, $3) }
	| CONS OBRACKET ty CBRACKET accessTerm accessTerm
		{ TmCons ($3, $5, $6) }
	| ISNIL OBRACKET ty CBRACKET accessTerm 
		{ TmIsNil ($3, $5) }
	| HEAD OBRACKET ty CBRACKET accessTerm 
		{ TmHead ($3, $5) }
	| TAIL OBRACKET ty CBRACKET accessTerm 
		{ TmTail ($3, $5) }
	| NIL OBRACKET ty CBRACKET 
		{ TmNil $3 }
	// | CASE accessTerm OF tmMatch
	// 	{ TmCase ($2, $4) }
	| appTerm AS ty
		{ TmAscr ($1, $3) }
	| appTerm accessTerm
		{ TmApp ($1, $2) }

accessTerm :
	accessTerm DOT INTV
		{ TmGet ($1, (string_of_int $3))}
	| accessTerm DOT IDV
		{ TmGet ($1, $3)}
	| atomicTerm
		{ $1 }

atomicTerm :
	LPAREN term RPAREN
		{ $2 }
	| TRUE
		{ TmTrue }
	| FALSE
		{ TmFalse }
	| IDV
		{TmVar $1}
	| INTV
		{ let rec f = function
				0 -> TmZero
			| n -> TmSucc (f (n-1))
			in f $1 }
	| STRINGV
		{ TmString $1 }
	| LKEY tmSequence RKEY
		{ TmTuple $2 }
	| LKEY tmFieldSeq RKEY
		{ TmRecord $2 }
	| variantTerm
		{ $1 }

variantTerm:
	LVAR tmFieldSeq RVAR
		{ TmVariant $2 }

tmSequence:
	term COMMA tmSequence
		{ $1::$3 }
	| term
		{	[$1] }

tmFieldSeq:
	{ [] }
	| non_empty
		{ $1 }

non_empty:
	IDV EQ term
		{[$1, $3]}
	| IDV EQ term COMMA non_empty
		{($1, $3)::$5}

// tmMatch:
// 	variantTerm EARROW LPAREN appTerm RPAREN 
// 		{ [($1, $4)] }
// 	| variantTerm EARROW LPAREN appTerm RPAREN PIPE tmMatch
// 		{ ($1, $4)::$7 }


ty :
	atomicTy
		{ $1 }
	| atomicTy ARROW ty
		{ TyArr ($1, $3) }

atomicTy :
	LPAREN ty RPAREN
		{ $2 }
	| BOOL
		{ TyBool }
	| NAT
		{ TyNat }
	| STRING
		{ TyString }
	| LKEY tySequence RKEY
		{ TyTuple $2 }
	| LKEY tyFieldSeq RKEY
		{ TyRecord $2 }
	| LVAR tyFieldSeq RVAR
		{ TyVariant $2 }
	| IDT
		{ TyVar $1}
	| LIST OBRACKET ty CBRACKET
		{ TyList $3 }

tyFieldSeq:
	{ [] }
	| non_empty_ty
		{ $1 }

non_empty_ty:
	IDV COLON ty
		{[$1, $3]}
	| IDV COLON ty COMMA non_empty_ty
		{($1, $3)::$5}

tySequence:
	| ty
		{ [$1] }
	| ty COMMA tySequence
		{ $1::$3 }

// tyMatch:
// 	TyVariant EARROW LPAREN ty RPAREN
// 		{ [($1, $4)] }
// 	| TyVariant EARROW LPAREN ty RPAREN PIPE tyMatch
// 		{ ($1, $4)::$7 }