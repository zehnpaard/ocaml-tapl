type ty =
  | TyBool
  | TyArrow of ty * ty
;;

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmVar of int
  | TmAbs of ty * term
  | TmApp of term * term
;;
