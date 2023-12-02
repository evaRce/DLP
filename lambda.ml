
(* TYPE DEFINITIONS *)

type ty =
	TyBool
	| TyNat
	| TyArr of ty * ty
	| TyString
	| TyTuple of ty list
	| TyRecord of (string * ty) list
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
	| TmTuple of term list
	| TmRecord of (string * term) list
	| TmGet of term * string
;;

type contextv =
	(string * term) list
;;

type action =
	Eval of term
	| Bind of string * term
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
	[]
;;

let addbinding ctx x bind =
	(x, bind) :: ctx
;;

let getbinding ctx x =
	List.assoc x ctx
;;

let emptydef =
	[]
;;

let adddef def x bind =
	(x, bind) :: def
;;

let getdef def x =
	List.assoc x def
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
	TyBool ->
		"Bool"
	| TyNat ->
		"Nat"
	| TyArr (ty1, ty2) ->
		"(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
	| TyString ->
		"String"
	| TyTuple tyseq ->
		let rec print = function
			[] -> ""
			| (ty::[]) -> (string_of_ty ty)
			| (ty::t) -> (string_of_ty ty) ^ " , " ^ print t
		in "{" ^ (print tyseq) ^ "}"
	| TyRecord tyfseq ->
		let rec print = function
			[] -> ""
			| ((s,ty)::[]) -> s ^ ":" ^ (string_of_ty ty)
			| ((s,ty)::t) -> s ^ ":" ^ (string_of_ty ty) ^ " , " ^ print t
		in "{" ^ (print tyfseq) ^ "}"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
	(* T-True *)
	TmTrue ->
		TyBool

	(* T-False *)
	| TmFalse ->
		TyBool

	(* T-If *)
	| TmIf (t1, t2, t3) ->
		if typeof ctx t1 = TyBool then
		let tyT2 = typeof ctx t2 in
		if typeof ctx t3 = tyT2 then tyT2
		else raise (Type_error "arms of conditional have different types")
		else
		raise (Type_error "guard of conditional not a boolean")

	(* T-Zero *)
	| TmZero ->
		TyNat

	(* T-Succ *)
	| TmSucc t1 ->
		if typeof ctx t1 = TyNat then TyNat
		else raise (Type_error "argument of succ is not a number")

	(* T-Pred *)
	| TmPred t1 ->
		if typeof ctx t1 = TyNat then TyNat
		else raise (Type_error "argument of pred is not a number")

	(* T-Iszero *)
	| TmIsZero t1 ->
		if typeof ctx t1 = TyNat then TyBool
		else raise (Type_error "argument of iszero is not a number")

	(* T-Var *)
	| TmVar x ->
		(try getbinding ctx x with
		_ -> raise (Type_error ("no binding type for variable " ^ x)))

	(* T-Abs *)
	| TmAbs (x, tyT1, t2) ->
		let ctx' = addbinding ctx x tyT1 in
		let tyT2 = typeof ctx' t2 in
		TyArr (tyT1, tyT2)

	(* T-App *)
	| TmApp (t1, t2) ->
		let tyT1 = typeof ctx t1 in
		let tyT2 = typeof ctx t2 in
		(match tyT1 with
			TyArr (tyT11, tyT12) ->
				if tyT2 = tyT11 then tyT12
				else raise (Type_error "parameter type mismatch")
			| _ -> raise (Type_error "arrow type expected"))

	(* T-Let *)
	| TmLetIn (x, t1, t2) ->
		let tyT1 = typeof ctx t1 in
		let ctx' = addbinding ctx x tyT1 in
		typeof ctx' t2

	(* T-Fix *)
	| TmFix t1 ->
		let tyT1 = typeof ctx t1 in
		(match tyT1 with
			TyArr (tyT11, tyT12) ->
				if tyT11 = tyT12 then tyT12
				else raise (Type_error "result of body not compatible with domain")
			| _ -> raise (Type_error "arrow type expected"))

	(* new rule for string *)
	| TmString _ ->
		TyString
	
	(* new rule for string *)
	| TmConcat (t1, t2) ->
		if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
		else raise (Type_error "argument of concat is not a string")

	| TmHead t1 ->
		if typeof ctx t1 = TyString then TyString
		else raise (Type_error "argument of Head is not a string")

	| TmTail t1 ->
		if typeof ctx t1 = TyString then TyString
		else raise (Type_error "argument of Tail is not a string")

	| TmTuple tup ->
		let rec types = function
			[] -> []
			|(tm::t) -> ((typeof ctx tm)::(types t))
		in TyTuple (types tup)
	| TmRecord reco ->
		let rec types = function
			[] -> []
			|((s,tm)::t) -> ((s, typeof ctx tm)::(types t))
		in TyRecord (types reco)
	| TmGet (t, x) ->
		(match(typeof ctx t, x) with
			|(TyRecord (reco), s) ->
				(try List.assoc s reco with
				_ -> raise (Type_error (s ^ " key doesn't exist in record")))

			|(TyTuple (tup), s) ->
				(try List.nth tup (int_of_string s - 1) with
				_ -> raise (Type_error (s ^ " is out of bounds for this tuple")))
			|(y,_) -> raise (Type_error("Expected tuple or record type, got " ^ string_of_ty y)))
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
	TmTrue ->
		"true"
	| TmFalse ->
		"false"
	| TmIf (t1,t2,t3) ->
		"if " ^ "(" ^ string_of_term t1 ^ ")" ^
		" then " ^ "(" ^ string_of_term t2 ^ ")" ^
		" else " ^ "(" ^ string_of_term t3 ^ ")"
	| TmZero ->
		"0"
	| TmSucc t ->
		let rec f n t' = match t' with
			TmZero -> string_of_int n
		| TmSucc s -> f (n+1) s
		| _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
		in f 1 t
	| TmPred t ->
		"pred " ^ "(" ^ string_of_term t ^ ")"
	| TmIsZero t ->
		"iszero " ^ "(" ^ string_of_term t ^ ")"
	| TmVar s ->
		s
	| TmAbs (s, tyS, t) ->
		"(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
	| TmApp (t1, t2) ->
		"(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
	| TmLetIn (s, t1, t2) ->
		"let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
	| TmFix t ->
		"(fix " ^ string_of_term t ^ ")"
	| TmString s ->
		"\"" ^ s ^ "\""
	| TmConcat (t1, t2) ->
		"concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
	| TmHead t1 ->
		"head " ^ string_of_term t1
	| TmTail t1 ->
		"tail " ^ string_of_term t1
	| TmTuple t1 ->
		let rec print = function
			[] -> ""
			| (tm::[]) -> (string_of_term tm)
			| (tm::t) -> (string_of_term tm) ^ " , " ^ print t
		in "{" ^ (print t1) ^ "}"
	| TmRecord t1 ->
		let rec print = function
			[] -> ""
			| ((s,tm)::[]) -> s ^ "=" ^ (string_of_term tm)
			| ((s,tm)::t) -> s ^ "=" ^ (string_of_term tm) ^ " , " ^ print t
		in "{" ^ (print t1) ^ "}"
	| TmGet (t, x) ->
		string_of_term t ^ "." ^ x
;;

let rec ldif l1 l2 = match l1 with
	[] -> []
	| h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
	[] -> l2
	| h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
	TmTrue ->
		[]
	| TmFalse ->
		[]
	| TmIf (t1, t2, t3) ->
		lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
	| TmZero ->
		[]
	| TmSucc t ->
		free_vars t
	| TmPred t ->
		free_vars t
	| TmIsZero t ->
		free_vars t
	| TmVar s ->
		[s]
	| TmAbs (s, _, t) ->
		ldif (free_vars t) [s]
	| TmApp (t1, t2) ->
		lunion (free_vars t1) (free_vars t2)
	| TmLetIn (s, t1, t2) ->
		lunion (ldif (free_vars t2) [s]) (free_vars t1)
	| TmFix t ->
		free_vars t
	| TmString _ ->
		[]
	| TmConcat (t1, t2) ->
		lunion (free_vars t1) (free_vars t2)
	| TmHead t1 ->
		free_vars t1
	| TmTail t1 ->
		free_vars t1
	| TmTuple t1 ->
		let rec freeseq = function
			[] -> []
			|(tm::t) -> lunion (free_vars tm) (freeseq t)
		in freeseq t1
	| TmRecord t1 ->
		let rec freefseq = function
			[] -> []
			|((_,tm)::t) -> lunion (free_vars tm) (freefseq t)
		in freefseq t1
	| TmGet (t, x) ->
		free_vars t
;;

let rec fresh_name x l =
	if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
	TmTrue ->
		TmTrue
	| TmFalse ->
		TmFalse
	| TmIf (t1, t2, t3) ->
		TmIf (subst x s t1, subst x s t2, subst x s t3)
	| TmZero ->
		TmZero
	| TmSucc t ->
		TmSucc (subst x s t)
	| TmPred t ->
		TmPred (subst x s t)
	| TmIsZero t ->
		TmIsZero (subst x s t)
	| TmVar y ->
		if y = x then s else tm
	| TmAbs (y, tyY, t) ->
		if y = x then tm
		else let fvs = free_vars s in
			if not (List.mem y fvs)
			then TmAbs (y, tyY, subst x s t)
			else let z = fresh_name y (free_vars t @ fvs) in
				TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
	| TmApp (t1, t2) ->
		TmApp (subst x s t1, subst x s t2)
	| TmLetIn (y, t1, t2) ->
		if y = x then TmLetIn (y, subst x s t1, t2)
		else let fvs = free_vars s in
			if not (List.mem y fvs)
			then TmLetIn (y, subst x s t1, subst x s t2)
			else let z = fresh_name y (free_vars t2 @ fvs) in
				TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
	| TmFix t ->
		TmFix (subst x s t)
	| TmString st ->
		TmString st
	| TmConcat (t1, t2) ->
		TmConcat (subst x s t1, subst x s t2)
	| TmHead t1 ->
		TmHead (subst x s t1)
	| TmTail t1 ->
		TmTail (subst x s t1)
	| TmTuple tup ->
		let rec substseq = function
			[] -> []
			|(tm::t) -> (subst x s tm)::(substseq t)
		in TmTuple (substseq tup)
	| TmRecord reco ->
		let rec substfseq = function
			[] -> []
			|((st,tm)::t) -> (st, (subst x s tm))::(substfseq t)
		in TmRecord (substfseq reco)
	| TmGet (t, y) ->
		TmGet (subst x s t, y)
;;

let rec isnumericval tm = match tm with
	TmZero -> true
	| TmSucc t -> isnumericval t
	| _ -> false
;;

let rec isval tm = match tm with
	TmTrue  -> true
	| TmFalse -> true
	| TmAbs _ -> true
	| TmString _ -> true
	| TmTuple l -> List.for_all(fun t -> isval(t)) l
	| TmRecord l -> List.for_all(fun (s,t) -> isval(t)) l
	| t when isnumericval t -> true
	| _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
	(* E-IfTrue *)
	TmIf (TmTrue, t2, _) ->
		t2

	(* E-IfFalse *)
	| TmIf (TmFalse, _, t3) ->
		t3

	(* E-If *)
	| TmIf (t1, t2, t3) ->
		let t1' = eval1 ctx t1 in
		TmIf (t1', t2, t3)

	(* E-Succ *)
	| TmSucc t1 ->
		let t1' = eval1 ctx t1 in
		TmSucc t1'

	(* E-PredZero *)
	| TmPred TmZero ->
		TmZero

	(* E-PredSucc *)
	| TmPred (TmSucc nv1) when isnumericval nv1 ->
		nv1

	(* E-Pred *)
	| TmPred t1 ->
		let t1' = eval1 ctx t1 in
		TmPred t1'

	(* E-IszeroZero *)
	| TmIsZero TmZero ->
		TmTrue

	(* E-IszeroSucc *)
	| TmIsZero (TmSucc nv1) when isnumericval nv1 ->
		TmFalse

	(* E-Iszero *)
	| TmIsZero t1 ->
		let t1' = eval1 ctx t1 in
		TmIsZero t1'

	(* E-AppAbs *)
	| TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
		subst x v2 t12

	(* E-App2: evaluate argument before applying function *)
	| TmApp (v1, t2) when isval v1 ->
		let t2' = eval1 ctx t2 in
		TmApp (v1, t2')

	(* E-App1: evaluate function before argument *)
	| TmApp (t1, t2) ->
		let t1' = eval1 ctx t1 in
		TmApp (t1', t2)

	(* E-LetV *)
	| TmLetIn (x, v1, t2) when isval v1 ->
		subst x v1 t2

	(* E-Let *)
	| TmLetIn(x, t1, t2) ->
		let t1' = eval1 ctx t1 in
		TmLetIn (x, t1', t2)
	
	(* E-FixBeta *)
	| TmFix (TmAbs (x, _, t2)) ->
		subst x tm t2

	(* E-Fix *)
	| TmFix t1 ->
		let t1' = eval1 ctx t1 in
		TmFix t1'

	(* new rule for String *)
	| TmConcat (TmString s1, TmString s2) ->
		TmString (s1 ^ s2)

	(* new rule for String *)
	| TmConcat (TmString s1, t2) ->
		let t2' = eval1 ctx t2 in
		TmConcat (TmString s1, t2')

	(* new rule for String *)
	| TmConcat (t1, t2) ->
		let t1' = eval1 ctx t1 in
		TmConcat (t1', t2)

	| TmHead (TmString t1) ->
		TmString (String.sub t1 0 1)

	| TmHead t1 ->
		let t1' = eval1 ctx t1 in
		TmHead t1'

	| TmTail (TmString t1) ->
		TmString (String.sub t1 1 ((String.length t1)-1))


	| TmTail t1 ->
		let t1' = eval1 ctx t1 in
		TmTail t1'

	| TmTuple t1 ->
		let rec seq_eval = function
			[] -> raise NoRuleApplies
			|(tm::t) when isval tm -> tm::(seq_eval t)
			|(tm::t) -> (eval1 ctx tm)::t
		in TmTuple (seq_eval t1)

	| TmRecord t1 ->
		let rec field_seq_eval = function
			[] -> raise NoRuleApplies
			|((st, tm)::t) when isval tm -> (st, tm)::(field_seq_eval t)
			|((st, tm)::t) -> (st, (eval1 ctx tm))::t
		in TmRecord (field_seq_eval t1)

	| TmGet (TmRecord l as v, s) when isval(v) ->
		List.assoc s l

	| TmGet (TmRecord (reco), x) ->
		List.assoc x reco

	| TmGet (TmTuple l as v, s) when isval(v) ->
		List.nth l (int_of_string s - 1)

	| TmGet (t, x) ->
		TmGet ((eval1 ctx t), x)

	| TmVar s ->
		getbinding ctx s

	| _ ->
		raise NoRuleApplies
;;

let give_ctx ctx tm =
	List.fold_left (fun t x -> subst x (getbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
	try
		let tm' = eval1 ctx tm in
		eval ctx tm'
	with
		NoRuleApplies -> give_ctx ctx tm
;;

let execute (ctx, ctxt) = function
	Eval tm ->
		let ty_tm = typeof ctxt tm in
		let tm' = eval ctx tm in
		print_endline("- : " ^ (string_of_ty ty_tm) ^ " = " ^ (string_of_term tm'));
		(ctx, ctxt)
	| Bind (s, tm) ->
		let ty_tm = typeof ctxt tm in
		let tm' = eval ctx tm in
		print_endline(s ^ " : " ^ string_of_ty ty_tm ^ " = " ^ string_of_term tm');
		(addbinding ctx s tm', addbinding ctxt s ty_tm)
;;


