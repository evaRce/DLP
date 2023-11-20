
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
;;

type context =
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
;;

type contextv =
  (string * term) list
;;

type action =
    Eval of term
    | Bind of string * term
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;
val emptydef : contextv;;
val adddef : contextv -> string -> term -> contextv;;
val getdef : contextv -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : contextv -> term -> term;;

val execute : contextv * context -> action -> contextv * context;;