
type ty =
    TyBool
	| TyNat
	| TyArr of ty * ty
	| TyString
	| TyTuple of ty list
	| TyRecord of (string * ty) list
    | TyVariant of (string * ty) list
	| TyVar of string
	| TyList of ty list
;;

type contextty =
	(string * ty) list
;;

type term =
    TmTrue
	| TmFalse
	| TmIf of term * term * term
	| TmZero
	| TmSucc of term
	| TmPred of term
	| TmIsZero of term
	| TmVar of string
	| TmAbs of string * ty * term
	| TmApp of term * term
	| TmLetIn of string * term * term
	| TmFix of term
	| TmString of string
	| TmConcat of term * term
	| TmHead of term
	| TmTail of term
	| TmTuple of term list
	| TmRecord of (string * term) list
    | TmVariant of (string * term) list
	| TmGet of term * string
	| TmAscr of term * ty
	| TmList of term list
;;

type contextv =
	(string * term) list
;;

type action =
    Eval of term
    | Bind of string * term
	| EvalTy of string
	| BindTy of string * ty
;;

val emptyctx : contextty;;
val addbinding : contextty -> string -> ty -> contextty;;
val getbinding : contextty -> string -> ty;;
val emptydef : contextv;;
val adddef : contextv -> string -> term -> contextv;;
val getdef : contextv -> string -> term;;

val string_of_ty : contextty -> ty -> string;;
exception Type_error of string;;
val typeof : contextty -> term -> ty;;

val string_of_term : contextty -> term -> string;;
exception NoRuleApplies;;
val eval : contextv -> term -> term;;

val execute : contextv * contextty -> action -> contextv * contextty;;