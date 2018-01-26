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
  | Epair of exp * exp 
  | Ifthenelse of exp * exp * exp 
  | Let of ide * exp * exp 
  | Fun of ide * exp 
  | Appl of exp * exp 
  | Rec of ide * exp;;

type eval =
  Undefined
| Int of int
| Bool of bool
| Char of char
| List of eval list
| Pair of eval * eval
| Closure of exp * env
and
env = ide -> eval;;

(* Definizione ambiente per i tipi *)
let newtypenv = ([]:(ide*etype)list) ;;
(*funzione appoggio confronto ide*)
let confrontIde (a:ide) (b:ide) = match a,b with
Ide x , Ide y -> if (x=y) then true else false;;
let rec applytypenv (l:(ide*etype)list) s = match l with
[] -> failwith "applytypenv: Env is empty"
  |(i,e)::[] -> if (confrontIde i s) then e else failwith "applytypenv - Undefined name"
  |(i,e)::l1 -> if (confrontIde i s) then e else applytypenv l1 s ;;
let rec bindtyp (l:(ide*etype)list) ni ne = match l with 
[] -> (ni,ne)::[]
  |(i,e)::[] -> if (confrontIde i ni) then (i,ne)::[] 
    else (ni,ne)::(i,e)::[]
  |(i,e)::l2 -> if (confrontIde i ni) then (i,ne)::l2
    else (i,e)::(bindtyp l2 ni ne);;

(* Ambiente di esecuzione *)
let emptyenv = function (i:ide) -> Undefined;;
let applyenv ((r:env),(x:ide))= r x ;;
let bind (ambiente, nome, ev) =
 function nuovo -> if nuovo = nome then ev else applyenv (ambiente, nuovo);;

(* Generatore di nuove variabili di tipo *)
let nextsym = ref (-1);;
let newvar = fun () -> nextsym:=!nextsym+1; TVar ("?T" ^ string_of_int (!nextsym));;



(* Verifica (true) se la variabile di tipo di nome name compare in expr, false altrimenti *)
let rec isContainedInExpr name expr = match expr with
    TBool | TInt | TChar -> false
  | TVar n -> n = name
  | TPair(t1,t2) | TFun (t1,t2) -> (isContainedInExpr name t1) || (isContainedInExpr name t2)
  | TList [t] -> (match t with
        TVar n -> n = name
      | _ -> isContainedInExpr name t)
  | _ -> failwith "Errore nella verifica di occorrenza"
;;

(* Algoritmo di sostituzione *)
let rec subst newVal oldVal constrs = 
  let rec substValue newVal oldVal expr = match expr with
      TBool | TInt | TChar -> expr
    | TVar value -> if value = oldVal then newVal else TVar value
    | TPair(t1,t2) -> TPair(substValue newVal oldVal t1, substValue newVal oldVal t2)
    | TFun(t1,t2) -> TFun(substValue newVal oldVal t1, substValue newVal oldVal t2)
    | TList [t] -> TList [substValue newVal oldVal t]
    | _ -> failwith "Errore nella sostituzione"
in List.fold_right (fun x acc -> (substValue newVal oldVal (fst x), substValue newVal oldVal (snd x))::acc) 
                                constrs [];;
  
(* Valuta il tipo dell'espressione rispetto al vincolo (t1,t2) *)
let rec getType expType (t1,t2) = match expType with
    TInt -> TInt
  | TBool -> TBool
  | TChar -> TChar
  | TVar name -> if name = t1 then t2 else TVar name
  | TFun (t3,t4) -> TFun (getType t3 (t1,t2) , getType t4 (t1,t2))
  | TPair(t3,t4) -> TPair (getType t3 (t1,t2), getType t4 (t1,t2))
  | TList [l] -> TList [getType l (t1,t2)]
  | _ -> failwith "getType - Errore";;


let rec substInResult expType remConstrs = match remConstrs with
    [] -> expType
  | (TVar name, expr)::tl -> 
      substInResult (getType expType (name, expr)) tl
  | _ -> failwith "Tipo non inferibile (solving)";;

(* Risolve i vincoli (entry point dell'algoritmo di unificazione) *)
let rec solveConstraints constrs = match constrs with
    [] -> ([]:(etype * etype) list)
  | hd::tl as constrsList -> match fst hd, snd hd with
        (* Regola 1 *)
        (TInt,TInt) | (TBool,TBool) | (TChar,TChar) -> solveConstraints tl
      | (TVar n1, TVar n2) -> 
          if n1 = n2
          then solveConstraints tl 
          else (fst hd, snd hd)::solveConstraints (subst (snd hd) n1 tl)
          
      (* Regola 2 *)
      | (TVar (_ as name), _) -> 
          if not (isContainedInExpr name (snd hd)) 
          then (fst hd, snd hd)::solveConstraints (subst (snd hd) name tl)
          else failwith "solveConstraints:(TVar _, _) - Erroe occorrenza tipo"
      | (_, TVar (_ as name)) -> 
          if not (isContainedInExpr name (fst hd)) 
          then (snd hd, fst hd)::solveConstraints (subst (fst hd) name tl)
          else failwith "solveConstraints:(_, TVar _) - Errore occorrenza tipo"
            
      (* Regola 3 *)
      | (TFun(a,b), TFun(c,d)) 
      | (TPair(a,b),TPair(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
      
      | (TList [a], TList [b]) -> solveConstraints ((a,b)::tl)
      
      (* Altrimenti ... *)
      | _ -> constrsList;;



let rec buildExpr ev (execEnv:env) =
  match ev with
      Undefined -> failwith "inferEval:Undefined - Nome indefinito"
    | Int x -> Eint x
    | Bool b -> if b then True else False
    | Char c -> Echar c
    | List [] -> Empty
    | List (hd::tl) -> Cons(buildExpr hd execEnv, buildExpr (List tl) execEnv)
    | Pair(sx,dx) -> Epair(buildExpr sx execEnv, buildExpr dx execEnv)
    | _ -> failwith "buildExpr: - Espressione non valida"
;;




(* Genera una coppia (tipo espressione, lista di vincoli) *)
let rec getConstraints expr typeEnv execEnv = 
  match expr with
      Val nome ->
        let (isdefinedT, gConstrs) = 
          try (true, (applytypenv typeEnv nome, [])) 
          with _ -> (false, (TVar "", []))
        in if isdefinedT
          then gConstrs 
          else (match applyenv(execEnv,nome) with
                    Closure(f,amb_locale) ->
                      (match f with
                           Fun(x,t) -> getConstraints f newtypenv amb_locale
                         | _ -> failwith "getConstraints:Val:Closure - Chiusura non valida")
                  | _ -> getConstraints (buildExpr (applyenv(execEnv,nome)) execEnv) typeEnv execEnv)
        
    | Echar c -> (TChar,[])
    | Eint x -> (TInt, [])
             
    | True 
    | False -> (TBool, [])
        
    | Sum(t1,t2) 
    | Diff(t1,t2) 
    | Times(t1,t2) -> 
        let ((t1Type, t1Constrs),(t2Type, t2Constrs)) = 
          (getConstraints t1 typeEnv execEnv, getConstraints t2 typeEnv execEnv)
        in (TInt, ([(t1Type,TInt)]@
                   [(t2Type,TInt)]@
                    (t1Constrs)@
                    (t2Constrs)))
        
    | And(b1,b2) 
    | Or(b1,b2) -> 
        let ((b1Type,b1Constrs),(b2Type,b2Constrs)) = 
          (getConstraints b1 typeEnv execEnv, getConstraints b2 typeEnv execEnv)
        in(TBool, ([(b1Type,TBool)]@
                   [(b2Type, TBool)]@
                    (b1Constrs)@
                    (b2Constrs)))
            
    | Not(b) ->
        let (bType,bConstrs) = getConstraints b typeEnv execEnv
        in (TBool, ([(bType, TBool)]@
                     (bConstrs)))
             
    | Less(t1,t2) ->
        let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv execEnv, getConstraints t2 typeEnv execEnv)
        in (TBool, ([(t1Type, TInt)]@
                    [(t2Type, TInt)]@
                     (t1Constrs)@
                     (t2Constrs)))
             
    | Eq(t1,t2) ->
        let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv execEnv, getConstraints t2 typeEnv execEnv)
        in (TBool, ([(t1Type, t2Type)]@
                    [(t1Type,t1Type)]@
                    [(t2Type,t2Type)]@
                     (t1Constrs)@
                     (t2Constrs))) 
             
    | Epair(t1,t2) -> 
        let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv execEnv, getConstraints t2 typeEnv execEnv)
        in (TPair(t1Type,t2Type), ([(t1Type,t1Type)]@
                                   [(t2Type,t2Type)]@
                                    (t1Constrs)@
                                    (t2Constrs)))
             
    | Fst(t) ->
        let (tType, tConstrs) = getConstraints t typeEnv execEnv
        in (match tType with
                (TPair(typeL,typeR)) -> 
                  (typeL, ([tType, TPair(typeL,typeR)]@(tConstrs)))
              | TVar n ->
                  let a = newvar()
                  in (a, [tType, TPair(a,newvar())]@tConstrs)
              | _ -> failwith "getConstraints:Fst - Espressione non valida")
             
    | Snd(t) ->
        let (tType,tConstrs) = getConstraints t typeEnv execEnv
        in (match tType with
                (TPair(typeL,typeR)) -> 
                  (typeR, ([(tType, TPair(typeL,typeR))]@(tConstrs)))
              | TVar n -> 
                  let a = newvar()
                  in (a, [tType, TPair(newvar(),a)]@tConstrs)
              | _ -> failwith "getConstraints:Snd - Espressione non valida")
             
    | Head l -> 
        let (typeL, lConstraints) = getConstraints l typeEnv execEnv
        in (match typeL with
                (TList [lType]) -> (lType, ([(typeL, TList [lType])]@lConstraints))
              | TVar n ->
                  let a = newvar()
                  in (a, ([(TList [a], typeL)]@lConstraints))
              | _ -> failwith "getConstraints:Head - L'espressione non è una lista")
             
    | Tail l ->
        let (lType,lConstraints) = getConstraints l typeEnv execEnv
        in (match lType with
                (TList [typel]) -> (lType, (lConstraints))
              | TVar n -> 
                  let a = newvar()
                  in (lType, ([TList[a], lType]@lConstraints))
              | _ -> failwith "L'espressione non è una lista")
             
             
             
    | Cons(t1,t2) ->
        let ((t1Type, t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv execEnv, getConstraints t2 typeEnv execEnv)
        in (TList [t1Type], ([t1Type,t1Type]@
                               [(t2Type, TList [t1Type])]@
                               (t1Constrs)@
                               (t2Constrs))) 
             
    | Empty -> (TList [newvar()], [])
        
    | Ifthenelse(b,t1,t2) ->
        let ((bType,bConstrs),(t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints b typeEnv execEnv,
           getConstraints t1 typeEnv execEnv,
           getConstraints t2 typeEnv execEnv)
        in  (t1Type, ([(bType,TBool)]@
                        [t1Type,t2Type]@
                        (bConstrs)@
                        (t1Constrs)@
                        (t2Constrs)))
              
    | Let (name,t1,t2) -> 
        let a = newvar() in
        let (t1Type,t1Constrs) = (getConstraints t1 typeEnv execEnv)
        in let (t2Type,t2Constrs) = (getConstraints t2 (bindtyp typeEnv name t1Type) execEnv)
        in (t2Type, ([(t1Type, a)]@
                       (t1Constrs)@
                       (t2Constrs)))
             
    | Fun(x,t) ->
        let a = newvar() in
        let (tType, tConstrs) = getConstraints t (bindtyp typeEnv x a) execEnv
        in (TFun(a,tType),tConstrs)
             

    | Appl(t1,t2) ->
        let a = newvar()
        in let (t1Type, t1Constrs) = getConstraints t1 typeEnv execEnv
        in let (t2Type, t2Constrs) = getConstraints t2 typeEnv execEnv
        in (a, ([t1Type, TFun(t2Type,a)]@
                  (t1Constrs)@
                  (t2Constrs)))
             
    | Rec(y,f) -> 
        (match f with
             Fun(x,t) ->
               let a = newvar() 
               in let (fType, fConstrs) = 
                   getConstraints f (bindtyp typeEnv y a) execEnv
               in (fType, ([a,a]@fConstrs))
           | _ -> failwith "getConstraints:Rec - Il secondo termine non è una funzione")
;;


(* Se il tipo e inferibile, ovvero rispetta le regole, allora restituisci il suo tipo *)
let rec inferType expr execEnv = 
  let exprConstraints = getConstraints expr newtypenv execEnv in
  let unifiedConstrs = solveConstraints (snd exprConstraints) in 
    (if unifiedConstrs = [] then fst exprConstraints else
       substInResult  (fst exprConstraints) unifiedConstrs);;



let eq (x,y) = match (x,y) with
    (Int u, Int w) -> Bool(u = w)
  | (Char u, Char w) -> Bool(u = w)
  | (Bool u, Bool w ) -> Bool(u = w)
  | (List u, List w) -> Bool(u = w)
  | (Pair(u,v), Pair(w,z)) -> Bool(u = w && v = z)
  | _ -> failwith "Espressione non valida in Eq";;
  

let less (x,y) = match (x,y) with
    (Int u, Int w) -> Bool(u < w)
  | _ -> failwith ("Espressione non valida in Less");;

let sum (x,y) = match (x,y) with
    (Int u, Int w) -> Int(u + w)
  | _ -> failwith ("Espressione non valida in Sum");;

let diff (x,y) = match (x,y) with
    (Int u, Int w) -> Int(u - w)
  | _ -> failwith ("Espressione non valida in Diff");;
  

let times (x,y) = match (x,y) with
    (Int u, Int w) -> Int(u * w)
  | _ -> failwith ("Espressione non valida in Times");;

let logicAnd (x,y) = match (x,y) with
    (Bool u, Bool w) -> Bool(u && w)
  | _ -> failwith ("Espressione non valida in And");;

let logicOr (x,y) = match (x,y) with
    (Bool u, Bool w) -> Bool(u || w)
  | _ -> failwith ("Espressione non valida in Or");;

let unaryNegation x = match x with
    Bool y -> Bool(not y)
  | _ -> failwith ("Espressione non valida in not");;

let pair (x,y) = Pair(x,y);;

let cons (a,b) = match b with
    (List t) -> List (a::t)
   |_ -> failwith "Espressione non valida in Cons";;

let head l = match l with
    (List (hd::tl)) -> hd
  | _ -> failwith "Espressione non valida in Head";;

let tail l = match l with
    (List (hd::tl)) -> List tl
  | _ -> failwith "Espressione non valida in Tail";;
  


let rec calcFV expr amb_old amb_new = match expr with
    (* Se si incontra un identificatore *)
    Val ((Ide v)as var) -> bind (amb_new, var, applyenv (amb_old,var)) 
      (* ----------------------------------------------*)
  | Eint a -> amb_new
  | Echar a -> amb_new
  | True | False | Empty -> amb_new
  | Sum(t1,t2) | Diff(t1,t2) | Times(t1,t2) 
  | And(t1,t2) | Or(t1,t2) | Eq(t1,t2) | Less(t1,t2) 
  | Cons(t1,t2) | Epair(t1,t2) | Appl(t1,t2) 
      -> calcFV t1 amb_old (calcFV t2 amb_old amb_new)
  | Head t | Tail t | Fst t | Snd t | Not t -> calcFV t amb_old amb_new
  | Ifthenelse (t0,t1,t2) -> calcFV t0 amb_old (calcFV t1 amb_old (calcFV t2 amb_old amb_new))
  | Fun (x, t1) -> calcFV t1 amb_old (bind (amb_new, x, Undefined))
  | Rec (y, t1) -> 
      (match t1 with
          Fun(x,t) -> calcFV (Fun(x,t)) amb_old amb_new
        | _ -> failwith "funzione non valida in calcFV:Rec")
  | Let (x, t1, t2) -> calcFV t1 amb_old 
      (calcFV t2 amb_old (bind (amb_new, x, Undefined)));;


let rec substRec newVal oldVal expr = match expr with
(* Chiave d'uscita: sostiuisce il nome della funzione ricorsiva con la sua espressione *)
    Val name -> if name = oldVal then newVal else Val name
(* ------------------ *)
  | True | False | Empty -> expr
  | Echar x -> expr
  | Eint x -> expr
  | Sum(a,b) -> Sum(substRec newVal oldVal a, substRec newVal oldVal b)
  | Diff(a,b) -> Diff(substRec newVal oldVal a, substRec newVal oldVal b)
  | Times(a,b) -> Times(substRec newVal oldVal a, substRec newVal oldVal b)
  | And(a,b) -> And(substRec newVal oldVal a, substRec newVal oldVal b)
  | Or(a,b) -> Or(substRec newVal oldVal a, substRec newVal oldVal b)
  | Eq(a,b) -> Eq(substRec newVal oldVal a, substRec newVal oldVal b)
  | Less(a,b) -> Less(substRec newVal oldVal a, substRec newVal oldVal b)
  | Cons(a,b) -> Cons(substRec newVal oldVal a, substRec newVal oldVal b)
  | Epair(a,b) -> Epair(substRec newVal oldVal a, substRec newVal oldVal b)
  | Not a -> Not(substRec newVal oldVal a)
  | Head a -> Head(substRec newVal oldVal a)
  | Tail a -> Tail(substRec newVal oldVal a)
  | Fst a -> Fst(substRec newVal oldVal a)
  | Snd a -> Snd(substRec newVal oldVal a)
  | Ifthenelse(a,b,c) -> Ifthenelse(substRec newVal oldVal a, substRec newVal oldVal b, substRec newVal oldVal c)
  | Let(a,b,c) -> Let(a, substRec newVal oldVal b, substRec newVal oldVal c)
  | Fun(x,t) -> Fun(x, substRec newVal oldVal t)
  | Appl(a,b) -> Appl(substRec newVal oldVal a, substRec newVal oldVal b)
  | _ -> failwith "substRec: Errore nella sostituzione Rec";;




let rec sem (e:exp) (amb:env) =
  let typeCheck = try (true, inferType e amb) with _ -> (false, TVar "")
  in let rec evaluate (e:exp) (amb:env) =
      if fst typeCheck
      then (match e with
        | Val x -> applyenv (amb, x)
        | Eint n -> Int n
        | Echar b -> Char b
        | True -> Bool true
        | False -> Bool false
        | Empty -> List []
        | Cons(a,b) -> cons(evaluate a amb, evaluate b amb)                
        | Head a -> head (evaluate a amb) 
        | Tail a -> tail (evaluate a amb) 
        | Epair(a,b) -> pair (evaluate a amb, evaluate b amb) 
            
        | Fst(p) -> (match evaluate p amb with
                         Pair(a,b) -> a
                       | _ -> failwith "sem:Fst - Type error in Fst")
        | Snd(p) -> (match evaluate p amb with
                         Pair(a,b) -> b
                       | _ -> failwith "sem:Snd - Type error in Snd")
            
        | Eq(a,b) -> eq(evaluate a amb, evaluate b amb)                
        | Times(a,b) -> times(evaluate a amb, evaluate b amb)
        | Sum(a,b) -> sum(evaluate a amb, evaluate b amb)
        | Diff(a,b) -> diff(evaluate a amb, evaluate b amb)                
        | And(a,b) -> logicAnd(evaluate a amb, evaluate b amb)                
        | Or(a,b) -> logicOr(evaluate a amb, evaluate b amb)                
        | Less(a,b) -> less(evaluate a amb, evaluate b amb)                
        | Not(a) -> unaryNegation(evaluate a amb)                
        | Ifthenelse(a,b,c) -> if evaluate a amb = Bool true then evaluate b amb else evaluate c amb
        | Let(a,b,c) -> 
            let newExecEnv = bind (amb, a, (evaluate b amb))
            in evaluate c newExecEnv                 
        | Fun(a,b) -> Closure(Fun(a,b), calcFV b amb emptyenv) 
        | Rec(a,b) -> 
            (match b with
                 (Fun(x,t)) ->            
                   let tSubst = substRec (Rec(a,Fun(x,t))) a t in
                     Closure(Fun(x,tSubst), calcFV tSubst amb emptyenv)
               | _ -> failwith "sem:Rec - Funzione non valida")
        | Appl(a,b) -> 
            (match evaluate a amb with
                 Closure(Fun(x2,t2), amb_locale) ->
                   let newExecEnv = bind (amb_locale, x2, evaluate b amb)
                   in evaluate t2 newExecEnv
               |  _ -> failwith "sem:Appl - Funzione non valida"))
      else failwith "sem:All - Type error in sem"
  in evaluate e amb;;

