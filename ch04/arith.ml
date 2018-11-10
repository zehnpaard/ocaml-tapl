type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
;;

let rec isnumericval = function
  | TmZero -> true
  | TmSucc t1 -> isnumericval t1
  | _ -> false
;;

let rec isval = function
  | TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies;;
let rec eval1 = function
  | TmIf (TmTrue, t2, t3) -> t2
  | TmIf (TmFalse, t2, t3) -> t3
  | TmIf (t1, t2, t3) -> TmIf (eval1 t1, t2, t3)
  | TmSucc t1 -> TmSucc (eval1 t1)
  | TmPred TmZero -> TmZero
  | TmPred (TmSucc nv1) when isnumericval nv1 -> nv1
  | TmPred t1 -> TmPred (eval1 t1)
  | TmIsZero TmZero -> TmTrue
  | TmIsZero (TmSucc nv1) when isnumericval nv1 -> TmFalse
  | TmIsZero t1 -> TmIsZero (eval1 t1)
  | _ -> raise NoRuleApplies
;;

let rec eval t =
    try (let t' = eval1 t in eval t')
    with NoRuleApplies -> t
;;
