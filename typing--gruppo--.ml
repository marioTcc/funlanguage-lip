(*stringa che identifica il tipo dei dati*)
type  ide = Ide of string;;

(*tipo per i tipi primitivi dell'ambiente*)
type etype = 
    TBool 
  | TInt
  | TVar of string
  | TPair of etype * etype 
  | TList of etype list
  | TFun of etype * etype;;

(*tipo dell'espressione valutata*)
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

type eval = Undefined | Int of int | Bool of bool | Char of char | List of eval list | Pair of eval*eval | Closure of exp*env
and env = ide -> eval;;

(* Genera una coppia (tipo espressione, lista di vincoli) *)
(* !!!!!!!!!!! Ne mancano parecchi TODO !!!!!!!!! *)
let rec getConstraints t = match t with
    Eint(x) -> (TInt, [])
  | True -> (TBool, [])
  | False -> (TBool, [])
  | Not(b) -> (TBool, [])
  | Sum(t1,t2) -> (TInt, 
                   ( [(fst (getConstraints t1) ,TInt)]@
                       [(fst (getConstraints t2),TInt)]@
                       (snd(getConstraints t1))@
                       (snd(getConstraints t2))@[]  )       );;

(* Risolve i vincoli *)
let rec solveConstraints (constrs:(etype*etype) list) = match constrs with
    [] -> true
  | hd::tl ->
      if fst hd = snd hd then solveConstraints tl else false(* Verifica se la coppia è fatta di termini uguali, se si la elimina *)
;; (* Ne mancano parecchie TODO *)

let expr = Sum(Eint(2), True);;
let expr2 = Sum(Eint(2), Eint(3));;
solveConstraints (snd (getConstraints expr) );;
solveConstraints (snd (getConstraints expr2) );;

(* Aloritmo di sostituzione *)
let subst newval oldval constrs = match constrs with
    [] -> []
  | hd::tl -> if fst hd = oldval then [(newva    , newval

        
                                        
  






(* !!! Da qui in poi è tutto da rivedere !!! *)

(*funzione per inferire il tipo delle variabili date  
let rec typeinf expr = match expr with 
    Val expr -> 
  | Eint of int 
  | Echar of char ->
  | True -> TBool
  | False  TBool
  | Empty 
  | Sum (exp * exp) 
  | Diff of (exp * exp) ->
  | Times of (exp * exp) 
  | And of (exp * exp) ->
  | Or of (exp * exp) 
  | Not of exp ->
  | Eq of (exp * exp) ->  
  | Less of (exp * exp) 
  | Cons of (exp * exp) ->
  | Head of exp ->
  | Tail of exp 
  | Fst of exp ->
  | Snd of exp ->
  | Pair of (exp * exp) 
  | Ifthenelse of (exp * exp * exp) -> 
  | Let of (ide * exp * exp) ->
  | Fun of (ide * exp) 
  | Appl of (exp * exp) ->  (*non è sbagliato si fa così
  | Rec of (ide * exp);;

funzioni per prima parte ancora da inserire nel codice

let rec variableFinder (var, env) =
  match env with
    [] -> failwith "Valore non trovato"
    |(name, value) :: envTl -> 
                   if name = var then value else variableFinder (var,envTl);;


let typeinf (tipo,value) =
  match tipo with
      "int" -> (match value with
                    Int(x) -> true
                  | _ -> false)
    | "bool" -> (match value with
                    Bool(x) -> true
                  | _ -> false)
    | "char" -> (match value with 
                     Char(x) -> true
                  | _ -> false)
    | _ -> failwith ("Invalid type")

let eq (x,y) =
    if typeinf("int",x) && typeinf("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Bool(u = w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let less (x,y) =
    if typeinf("int",x) && typeinf("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Bool(u < w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let sum (x,y) =
    if typeinf("int",x) && typeinf("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u+w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let diff (x,y) =
    if typeinf("int",x) && typeinf("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u-w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let times (x,y) =
    if typeinf("int",x) && typeinf("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u*w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let logicAnd (x,y) =
    if typeinf("bool",x) && typeinf("bool",y) then (
        match (x,y) with
              (Bool(u), Bool(w)) -> Bool(u && w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let logicOr (x,y) =
    if typeinf("bool",x) && typeinf("bool",y) then (
        match (x,y) with
              (Bool(u), Bool(w)) -> Bool(u || w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error")

let unaryNegation x =
    if typeinf("bool",x) then (
        match x with
              Bool(y) -> Bool(not y)
            | (_) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let cons (x,y) = 
   (*quando sarà finita bisognerà aggiungere l' if per
     il typeinf sull'head ed il tail della lista *)
      x::y;;

let rec sem (e: exp) =
    match e with
        Sum (a,b) -> sum ( sem (a), sem (b));;

sem (2+3);;*)
