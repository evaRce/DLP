
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
	atomicTerm
		{ $1 }
	| SUCC atomicTerm
		{ TmSucc $2 }
	| PRED atomicTerm
		{ TmPred $2 }
	| HEAD atomicTerm
		{ TmHead $2}
	| TAIL atomicTerm
		{ TmTail $2}
	| ISZERO atomicTerm
		{ TmIsZero $2 }
	| CONCAT atomicTerm atomicTerm
		{ TmConcat ($2, $3) }
	| appTerm atomicTerm
		{ TmApp ($1, $2) }

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

TmSequence:
	| term COLON TmSequence
		{ $1::$3 }
	| term
		{	[$1] }

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

	TySequence:
		| ty
			{ [$1] }
		| ty COMMA TySequence
			{ $1::$3 }

