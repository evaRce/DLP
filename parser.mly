
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
%token HEAD
%token TAIL
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token BOOL
%token NAT
%token STRING

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
%token <string> STRINGV

%start s
%type <Lambda.action> s

%%

s :
  	IDV EQ term EOF
  		{ Bind ($1, $3)}
  | term EOF
      { Eval $1 }

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
	| HEAD accessTerm
		{ TmHead $2}
	| TAIL accessTerm
		{ TmTail $2}
	| ISZERO accessTerm
		{ TmIsZero $2 }
	| CONCAT accessTerm accessTerm
		{ TmConcat ($2, $3) }
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
	| LKEY TmSequence RKEY
		{ TmTuple $2 }
	| LKEY TmFieldSeq RKEY
		{ TmRecord $2 }

TmSequence:
	term COMMA TmSequence
		{ $1::$3 }
	| term
		{	[$1] }

TmFieldSeq:
	{ [] }
	| non_empty
		{ $1 }

non_empty:
	IDV EQ term
		{[$1, $3]}
	| IDV EQ term COMMA non_empty
		{($1, $3)::$5}

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
	| LKEY TySequence RKEY
		{ TyTuple $2 }
	| LKEY TyFieldSeq RKEY
		{ TyRecord $2 }

TyFieldSeq:
	{ [] }
	| non_empty_ty
		{ $1 }

non_empty_ty:
	IDV COLON ty
		{[$1, $3]}
	| IDV COLON ty COMMA non_empty_ty
		{($1, $3)::$5}

TySequence:
	| ty
		{ [$1] }
	| ty COMMA TySequence
		{ $1::$3 }

