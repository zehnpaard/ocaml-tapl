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
    | TyVar n -> onvar c n
    | TyAll tyt2 -> TyAll (walk (c+1) tyt2)
    | TySome tyt2 -> TySome (walk (c+1) tyt2)
  in
  walk c tyt
;;
