
(* Identificatori (nomi di variabile) *)
type  ide = Ide of string;;

(* Tipi di valutazione per inferenza dei tipi*)
type etype = 
    TBool 
  | TInt
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


(* Verifica (true) se la variabile di tipo di nome name compare in expr, false altrimenti *)
let rec isContainedInExpr name expr = match expr with
    TBool | TInt -> false
  | TVar n -> n = name
  | TPair(t1,t2) | TFun (t1,t2) -> (isContainedInExpr name t1) || (isContainedInExpr name t2)
 (* | TList l -> serve? ÅË giusto? TODO *);;

(* Algoritmo di sostituzione *)
let rec subst newVal oldVal constrs = 
  let rec substValue newVal oldVal expr = match expr with
      TBool | TInt -> expr
    | TVar value -> if value = oldVal then newVal else TVar value
    | TPair(t1,t2) -> TPair(substValue newVal oldVal t1, substValue newVal oldVal t2)
    | TFun(t1,t2) -> TFun(substValue newVal oldVal t1, substValue newVal oldVal t2)
(* serve list? boh *)

  in List.fold_right (fun x acc -> (substValue newVal oldVal (fst x), substValue newVal oldVal (snd x))::acc) 
                                constrs [];;
  


let rec typeinf expr = 
(* Genera una coppia (tipo espressione, lista di vincoli) *)
(* !!!!!!!!!!! Ne mancano parecchi TODO !!!!!!!!! *)
(* !!! ÅË pesantissimo, serve ottimizzarlo? !!! *)
let rec getConstraints expr = match expr with
(* TODO qui ci va Val(x:ide) tale che <<x,type>> --> (type(x),vuoto) ovvero si chiede il tipo all' ambiente dei tipi *)
(* Inferenza per char ma non so come si fa (per ora) TODO*)
(* Inferenza per numerici *)
    Eint(x) -> (TInt, [])
  | Sum(t1,t2) | Diff(t1,t2) | Times(t1,t2) -> (TInt, 
                   ( [(fst (getConstraints t1) ,TInt)]@
                       [(fst (getConstraints t2),TInt)]@
                       (snd(getConstraints t1))@
                       (snd(getConstraints t2))@[]))
(* Inferenza per booleani o per espressioni booleane *)
  | True | False -> (TBool, [])
  | And(b1,b2) | Or(b1,b2) -> (TBool, 
                   ( [(fst (getConstraints b1), TBool )]@
                         [(fst (getConstraints b2), TBool)]@
                         (snd(getConstraints b1))@
                         (snd(getConstraints b2))@[] ))
  | Not(b) -> (TBool, ( [fst(getConstraints b), TBool]@
                          (snd(getConstraints b))@[] ))
  | Less(t1,t2) -> (TBool, ([(fst (getConstraints t1), TInt)]@
                                  [(fst (getConstraints t2), TInt)]@
                                  (snd(getConstraints t1))@
                                  (snd(getConstraints t2))@[]))
  | Eq(t1,t2) -> (TBool, ( [(fst (getConstraints t1), typeinf t1)]@
                             [(fst (getConstraints t2), typeinf t2)]@
                             (snd(getConstraints t1))@
                             (snd(getConstraints t2))@[])) (* !!! l'ha sbagliata pinna? al momento confronta fischi con fiaschi !!! *)
  | Pair(t1,t2) -> ( TPair(typeinf t1,typeinf t2),([fst(getConstraints t1),typeinf t1]@
                                                 [fst(getConstraints t2),typeinf t2]@
                                                 (snd(getConstraints t1))@
                                                 (snd(getConstraints t2))@[]))
 
 

and

(* Risolve i vincoli (entry point dell'algoritmo di unificazione) *)
(* !!! Per il momendo restituisce true se l'espressione ÅË inferenziabile false (invece che esplodere
   se non lo ÅË, da modificare con eccezioni poi TODO !!! *)
solveConstraints constrs = match constrs with
    [] -> true
  | hd::tl -> match fst hd, snd hd with
        (* Regola 2 *)
        (TVar name, _) -> if not (isContainedInExpr name (snd hd)) (* Se il lato sinistro ÅË una variabile che non compare a destra *)
                          then solveConstraints (subst (snd hd) name constrs)
                          else false (* TODO implementare eccezione *)
      | (_, TVar name) -> if not (isContainedInExpr name (fst hd)) (* Se il lato destro ÅË una variabile che non compare a sinistra *)
                          then solveConstraints (subst (fst hd) name constrs )
                          else false
        (* Regola 3 *)
        (* le condizioni si possono unificare ma emacs rompe i coglioni TODO *)
      | (TFun(a,b), TFun(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
      | (TPair(a,b),TPair(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
     (* oh ma sta cazzo di lista serve o no?  | (TList a, TList b) -> solveConstraints ((a,b)::tl) *)
        (* Regola 1 *)
      | (TInt,TInt) | (TBool,TBool) -> solveConstraints tl
      | _ -> false

(* Se il tipo ÅË inferibile, ovvero rispetta le regole (verificato da solveConstraints), allora restituisci il suo tipo *)
in (if solveConstraints(snd( getConstraints expr) ) then (fst (getConstraints expr)) else failwith "Tipo non inferibile");;

