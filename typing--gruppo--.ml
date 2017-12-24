
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

(* Tipi per la valutazione del valore delle espressioni *)
type eval = Undefined | Int of int | Bool of bool | Char of char | List of eval list | Pair of eval*eval | Closure of exp*env
(* Tipo dell'ambiente di esecuzione *)
and env = ide -> eval;;
(* TODO Definizione ambiente per i tipi *)

(* Genera una coppia (tipo espressione, lista di vincoli) *)
(* !!!!!!!!!!! Ne mancano parecchi TODO !!!!!!!!! *)
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
  | Not(b) -> (TBool, ( snd(getConstraints b)@[] ));; (* non funge *)

(* Verifica (true) se la variabile di tipo di nome name compare in expr, false altrimenti *)
let rec isContainedInExpr name expr = match expr with
    TBool | TInt -> false
  | TVar n -> n = name
  | TPair(t1,t2) | TFun (t1,t2) -> (isContainedInExpr name t1) || (isContainedInExpr name t2)
 (* | TList l -> serve? è giusto? TODO *);;

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
  

(* Risolve i vincoli (entry point dell'algoritmo di unificazione)TODO rimuovere forzatura *)
(* !!! Per il momendo restituisce true se l'espressione è inferenziabile false (invece che esplodere
   se non lo è, da modificare con eccezioni poi TODO !!! *)
let rec solveConstraints (constrs:(etype*etype) list) = match constrs with
    [] -> true
  | hd::tl -> match fst hd, snd hd with
        (* Regola 2 *)
        (TVar name, _) -> if not (isContainedInExpr name (snd hd)) (* Se il lato sinistro è una variabile che non compare a destra *)
                          then solveConstraints (subst (snd hd) name constrs)
                          else false (* TODO implementare eccezione *)
      | (_, TVar name) -> if not (isContainedInExpr name (fst hd)) (* Se il lato destro è una variabile che non compare a sinistra *)
                          then solveConstraints (subst (fst hd) name constrs )
                          else false
        (* Regola 3 *)
        (* le condizioni si possono unificare ma emacs rompe i coglioni TODO *)
      | (TFun(a,b), TFun(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
      | (TPair(a,b),TPair(c,d)) -> solveConstraints ((a,c)::(b,c)::tl)
     (* oh ma sta cazzo di lista serve o no?  | (TList a, TList b) -> solveConstraints ((a,b)::tl) *)
        (* Regola 1 *)
      | (TInt,TInt) | (TBool,TBool) -> solveConstraints tl
      | _ -> false;;
