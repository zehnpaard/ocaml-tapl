type ty =
  | TyVar of int
  | TyArrow of ty * ty
  | TyAll of ty
  | TySome of ty
;;

type binding =
  | VarBind of ty
  | TyVarBind
;;
type context = (string * binding) list;;

exception BindingError;;
let addbinding ctx bind = bind::ctx;;
let rec getbinding ctx n = match ctx, n with
  | [], _ -> raise BindingError
  | b::ctx', n -> if n = 0 then b else getbinding ctx' (n - 1)
;;
let getTypeFromContext ctx n = match getbinding ctx n with
  | VarBind ty1 -> ty1
  | _ -> raise BindingError
;;


let tymap onvar c tyt =
  let rec walk c tyt = match tyt with
    | TyArrow (tyt1, tyt2) -> TyArrow (walk c tyt1, walk c tyt2)
    | TyVar x -> onvar c x
    | TyAll tyt2 -> TyAll (walk (c+1) tyt2)
    | TySome tyt2 -> TySome (walk (c+1) tyt2)
  in
  walk c tyt
;;

let typeShiftAbove d c tyt =
    let f c x =
        if x >= c
        then TyVar (x+d)
        else TyVar x
    in
    tymap f c tyt
;;

let typeShift d tyt = typeShiftAbove d 0 tyt;;

let typeSubst tys j tyt =
    let f j x =
        if x = j
        then typeShift j tys
        else TyVar x
    in
    tymap f j tyt
;;

let typeSubstTop tys tyt =
    typeShift (-1) (typeSubst (typeShift 1 tys) 0 tyt)
;;


type term =
  | TmVar of int
  | TmAbs of ty * term
  | TmApp of term * term
  | TmTAbs of term
  | TmTApp of term * ty
  | TmPack of ty * term * ty
  | TmUnpack of term * term
;;

let tmmap onvar ontype c t =
  let rec walk c t = match t with
    | TmVar x -> onvar c x
    | TmAbs (ty1, t1) -> TmAbs (ontype c ty1, walk (c+1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | TmTAbs t1 -> TmTAbs (walk (c+1) t1)
    | TmTApp (t1, ty1) -> TmTApp (walk c t1, ontype c ty1)
    | TmPack (ty1, t2, ty3) -> TmPack (ontype c ty1, walk c t2, ontype c ty3)
    | TmUnpack (t1, t2) -> TmUnpack (walk c t1, walk (c+2) t2)
  in
  walk c t
;;

let termShiftAbove d c t =
    let f c x =
        if x >= c
        then TmVar (x+d)
        else TmVar x
    in
    tmmap f (typeShiftAbove d) c t
;;

let termShift d t = termShiftAbove d 0 t;;

let termSubst j s t =
    let f j x =
        if x = j
        then termShift j s
        else TmVar x
    in
    let g j t = t in
    tmmap f g j t
;;

let rec tytermSubst tys j t =
    let f c x = TmVar x in
    let g j t = typeSubst tys j t in
    tmmap f g j t
;;

let termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)
;;
let tytermSubstTop tys t =
    termShift (-1) (tytermSubst (typeShift 1 tys) 0 t)
;;


let isval = function
  | TmAbs _ -> true
  | _ -> false
;;

exception NoRuleApplies;;
let rec eval1 = function
  | TmApp (TmAbs (_, t1), t2) when isval t2 -> termSubstTop t2 t1
  | TmApp (t1, t2) when isval t1 -> TmApp (t1, eval1 t2)
  | TmApp (t1, t2) -> TmApp (eval1 t1, t2)
  | TmTApp (TmTAbs t1, ty2) -> tytermSubstTop ty2 t1
  | TmTApp (t1, ty2) -> TmTApp (eval1 t1, ty2)
  | TmUnpack (TmPack (ty1, t1, _), t2) when isval t1 ->
          tytermSubstTop ty1 (termSubstTop (termShift 1 t1) t2)
  | TmUnpack (t1, t2) -> TmUnpack (eval1 t1, t2)
  | TmPack (ty1, t2, ty3) -> TmPack (ty1, eval1 t2, ty3)
  | _ -> raise NoRuleApplies
;;

let rec eval t =
    try (let t' = eval1 t in eval t')
    with NoRuleApplies -> t
;;



