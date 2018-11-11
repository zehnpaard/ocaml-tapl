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
