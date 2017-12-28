
(* Identificatori (nomi di variabile) *)
type  ide = Ide of string;;

(* Tipi di valutazione per inferenza dei tipi*)
type etype = 
    TBool 
  | TInt
  | TChar
  | TVar of string
  | TPair of etype * etype 
  | TList of etype list
  | TFun of etype * etype;;

(* Tipo dell'espressione *)
type exp = 
    Val of ide
  | Eint of int
  | Echar of char
  | True
  | False 
  | Empty 
  | Sum of exp * exp 
  | Diff of exp * exp 
  | Times of exp * exp 
  | And of exp * exp 
  | Or of exp * exp 
  | Not of exp
  | Eq of exp * exp 
  | Less of exp * exp 
  | Cons of exp * exp
  | Head of exp 
  | Tail of exp 
  | Fst of exp 
  | Snd of exp
  | Pair of exp * exp 
  | Ifthenelse of exp * exp * exp 
  | Let of ide * exp * exp 
  | Fun of ide * exp 
  | Appl of exp * exp (*sbagliato da Pinna*)
  | Rec of ide * exp;;



(* TODO Definizione ambiente per i tipi *)
(* !!! TODO rivedere inferenza liste in generale !!! *)



(* Verifica (true) se la variabile di tipo di nome name compare in expr, false altrimenti *)
let rec isContainedInExpr name expr = match expr with
    TBool | TInt | TChar -> false
  | TVar n -> n = name
  | TPair(t1,t2) | TFun (t1,t2) -> (isContainedInExpr name t1) || (isContainedInExpr name t2)
  | TList [t] -> match t with
        TVar n -> n = name
      | _ -> isContainedInExpr name t
;;


(* Algoritmo di sostituzione *)
let rec subst newVal oldVal constrs = 
  let rec substValue newVal oldVal expr = match expr with
      TBool | TInt | TChar -> expr
    | TVar value -> if value = oldVal then newVal else TVar value
    | TPair(t1,t2) -> TPair(substValue newVal oldVal t1, substValue newVal oldVal t2)
    | TFun(t1,t2) -> TFun(substValue newVal oldVal t1, substValue newVal oldVal t2)
    | TList [t] -> match t with
          TVar oldVal -> TList [newVal]
        | _ -> substValue newVal oldVal t
in List.fold_right (fun x acc -> (substValue newVal oldVal (fst x), substValue newVal oldVal (snd x))::acc) 
                                constrs [];;
  




let rec typeinf expr = 
(* Genera una coppia (tipo espressione, lista di vincoli) *)
(* !!!!!!!!!!! Ne mancano parecchi TODO !!!!!!!!! *)
let rec getConstraints expr = match expr with
(* TODO qui ci va Val(x:ide) tale che <<x,type>> --> (type(x),vuoto) ovvero si chiede il tipo all' ambiente dei tipi *)
(* Inferenza per caratteri *)
    Echar c -> (TChar,[])
(* Inferenza per numerici *)
  | Eint x -> (TInt, [])
  | Sum(t1,t2) | Diff(t1,t2) | Times(t1,t2) -> 
      let ((t1Type, t1Constrs),(t2Type, t2Constrs)) = (getConstraints t1, getConstraints t2)
            in (TInt, ([(t1Type,TInt)]@
                       [(t2Type,TInt)]@
                        (t1Constrs)@
                        (t2Constrs)))
(* Inferenza per booleani o per espressioni booleane *)
  | True | False -> (TBool, [])
  | And(b1,b2) | Or(b1,b2) -> 
      let ((b1Type,b1Constrs),(b2Type,b2Constrs)) = (getConstraints b1, getConstraints b2)
        in(TBool, ([(b1Type,TBool)]@
                   [(b2Type, TBool)]@
                    (b1Constrs)@
                    (b2Constrs)))
  | Not(b) ->
      let (bType,bConstrs) = getConstraints b
        in (TBool, ([bType, TBool]@
                    (bConstrs)))
  | Less(t1,t2) ->
      let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1, getConstraints t2)
        in (TBool, ([(t1Type, TInt)]@
                    [(t2Type, TInt)]@
                    (t1Constrs)@
                    (t2Constrs)))
  | Eq(t1,t2) ->
      let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1,getConstraints t2)
        in (TBool, ([(t1Type, t2Type)]@
                    [(t1Type, t1Type)]@
                    [(t2Type, t2Type)]@
                    (t1Constrs)@
                    (t2Constrs))) 
(* Inferenza per il tipo coppia *)
  | Pair(t1,t2) -> 
      let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1,getConstraints t2)
        in ( TPair(t1Type,t2Type),([(t1Type,t1Type)]@
                                   [(t2Type,t2Type)]@
                                    (t1Constrs)@
                                    (t2Constrs)))
  | Fst(t) ->
      let (TPair(typeL, typeR),tConstrs) = getConstraints t
        in (typeL, ([(TPair(typeL,typeR), TPair(typeL,typeR))]@
                     (tConstrs)))
  | Snd(t) ->
      let (TPair(typeL, typeR),tConstrs) = getConstraints t
        in (typeR, ([(TPair(typeL,typeR), TPair(typeL,typeR))]@
                     (tConstrs)))
(* Inferenza per il tipo lista !! potrebbe servire modificarli !!! TODO *)
  | Head(l) -> 
      let (lType, lConstraints) = getConstraints l 
        in (lType, ([(TList [lType], TList [lType])]@lConstraints)) 
  | Tail l -> (* non funge *)
      let (lType,lConstraints) = getConstraints l
        in (lType, (lConstraints))
  | Cons(t1,t2) ->(* non funge *)
      let ((t1Type, t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1, getConstraints t2)
        in (TList [t1Type], ([(t1Type,t1Type)]@
                       [(t2Type, TList [t1Type])])@
                       (t1Constrs)@
                       (t2Constrs))
             (* !!! TMP !!! TODO *)
  | Empty -> (TList [TInt], [])
(* !!! TODO !!! *)
(* Inferenza per if-then-else *)
  | Ifthenelse(b,t1,t2) ->
      let ((bType,bConstrs),(t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints b,getConstraints t1, getConstraints t2)
        in  (t1Type, ([(bType,TBool)]@
                      [(t1Type,t2Type)]@
                      (bConstrs)@
                      (t1Constrs)@
                      (t2Constrs)))
(* Rompere solo in caso di incendio *)
  | _ -> failwith "Tipo non inferibile"

and

(* Risolve i vincoli (entry point dell'algoritmo di unificazione) *)
(* !!! Per il momendo restituisce true se l'espressione ÅË inferenziabile false (invece che esplodere
   se non lo ÅË, da modificare con eccezioni poi TODO !!! *)
solveConstraints constrs = match constrs with
    [] -> true
  | hd::tl -> match fst hd, snd hd with
        (* Regola 2 *)
        (TVar name, _) -> if not (isContainedInExpr name (snd hd)) (* Se il lato sinistro ÅÅË una variabile che non compare a destra *)
                          then solveConstraints (subst (snd hd) name constrs)
                          else false
      | (_, TVar name) -> if not (isContainedInExpr name (fst hd)) (* Se il lato destro ÅÅË una variabile che non compare a sinistra *)
                          then solveConstraints (subst (fst hd) name constrs )
                          else false
        (* Regola 3 *)
      | (TFun(a,b), TFun(c,d)) | (TPair(a,b),TPair(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
     (* | (TList a, TList b) -> solveConstraints ((a,b)::tl) TODO da verificare bene *)
        (* Regola 1 *)
      | (TInt,TInt) | (TBool,TBool) | (TChar,TChar) -> solveConstraints tl
      | _ -> false

(* Se il tipo ÅÅË inferibile, ovvero rispetta le regole (verificato da solveConstraints), allora restituisci il suo tipo *)
in let exprConstraints = getConstraints expr
in (if solveConstraints ( snd exprConstraints ) then fst exprConstraints else failwith "Tipo non inferibile");;
(* !!! TODO mancano varie cose delle liste !!! *)
