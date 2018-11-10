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

type binding = VarBind of ty;;
type context = (string * binding) list;;

exception BindingError;;
let addbinding ctx bind = bind::ctx;;
let rec getbinding ctx n = match ctx, n with
  | [], _ -> raise BindingError
  | b::ctx', n -> if n = 0 then b else getbinding ctx' (n - 1)
;;
let getTypeFromContext ctx n = match getbinding ctx n with
  | VarBind ty1 -> ty1
;;

exception TypeError;;
let rec typeOf ctx = function
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf (t1, t2, t3) ->
          if TyBool != typeOf ctx t1
          then raise TypeError
          else
              let ty2 = typeOf ctx t2 in
              let ty3 = typeOf ctx t3 in
              if ty2 != ty3
              then raise TypeError
              else ty2
  | TmVar n -> getTypeFromContext ctx n
  | TmAbs (tya, tb) ->
          let ctx' = addbinding ctx (VarBind tya) in
          let tyr = typeOf ctx' tb in
          TyArrow (tya, tyr)
  | TmApp (t1, t2) ->
          let ty1 = typeOf ctx t1 in
          let ty2 = typeOf ctx t2 in
          match ty1 with
            | TyArrow (tya, tyr) when ty2 = tya -> tyr
            | _ -> raise TypeError
;;
