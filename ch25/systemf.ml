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
