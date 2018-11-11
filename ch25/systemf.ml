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
