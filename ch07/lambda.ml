type term =
  | TmVar of int
  | TmAbs of term
  | TmApp of term * term
;;

let termShift d t =
    let rec walk c = function
      | TmVar x -> if x >= c then TmVar (x+d) else TmVar x
      | TmAbs t1 -> TmAbs (walk (c+1) t1)
      | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    in
    walk 0 t
;;

let termSubst j s t =
    let rec walk c = function
      | TmVar x -> if x = j+c then termShift c s else TmVar x
      | TmAbs t1 -> TmAbs (walk (c+1) t1)
      | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    in
    walk 0 t
;;

let termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)
;;

let isval = function
  | TmAbs _ -> true
  | _ -> false
;;

exception NoRuleApplies;;
let rec eval1 = function
  | TmApp (TmAbs t1, t2) when isval t2 -> termSubstTop t2 t1
  | TmApp (t1, t2) when isval t1 -> TmApp (t1, eval1 t2)
  | TmApp (t1, t2) -> TmApp (eval1 t1, t2)
  | _ -> raise NoRuleApplies
;;

let rec eval t =
    try (let t' = eval1 t in eval t')
    with NoRuleApplies -> t
;;
