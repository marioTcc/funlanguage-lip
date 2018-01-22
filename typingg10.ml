


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


(* Definizione ambiente per i tipi *)
let newtypenv = ([]:(ide*etype)list) ;;
(*funzione appoggio confronto ide*)
let confrontIde (a:ide) (b:ide) = match a,b with
Ide x , Ide y -> if (x=y) then true else false;;
let rec applytypenv (l:(ide*etype)list) s = match l with
[] -> failwith "applytypenv: Env is empty"
  |(i,e)::[] -> if (confrontIde i s) then e else failwith "applytypenv: undefined name"
  |(i,e)::l1 -> if (confrontIde i s) then e else applytypenv l1 s ;;
let rec bindtyp (l:(ide*etype)list) ni ne = match l with 
[] -> (ni,ne)::[]
  |(i,e)::[] -> if (confrontIde i ni) then (i,ne)::[] 
    else (ni,ne)::(i,e)::[]
  |(i,e)::l2 -> if (confrontIde i ni) then (i,ne)::l2
    else (i,e)::(bindtyp l2 ni ne);;






(* Generatore di nuove variabili di tipo *)
let nextsym = ref (-1);;
let newvar = fun () -> nextsym:=!nextsym+1; TVar ("?T" ^ string_of_int (!nextsym));;



let rec typeEquality t1 t2 =
  match t1,t2 with
      (TInt, TInt) | (TBool, TBool) | (TChar, TChar)-> true
    | (TPair(a,b), TPair(c,d)) -> typeEquality a c && typeEquality b d
    | (TFun(a,b), TFun(c,d)) -> typeEquality a c && typeEquality b d
    | (TList [a], TList [b]) -> typeEquality a b
    | (TVar _, _) | (_, TVar _) -> true
    | _ -> failwith "Espressione non valida in typeEquality";;



(* Verifica (true) se la variabile di tipo di nome name compare in expr, false altrimenti *)
let rec isContainedInExpr name expr = match expr with
    TBool | TInt | TChar -> false
  | TVar n -> n = name
  | TPair(t1,t2) | TFun (t1,t2) -> (isContainedInExpr name t1) || (isContainedInExpr name t2)
  | TList [t] -> isContainedInExpr name t
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
  | _ -> failwith "Errore";;

(* Valuta se sia possibile risolvere i vincoli rimanenti e inferire il tipo dell'espressione *) 
let rec solveRemainingConstrs expType remConstrs = match remConstrs with
    [] -> expType
  | (TVar name, expr)::tl -> solveRemainingConstrs (getType expType (name, expr)) tl
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
          else failwith "Erroe occorrenza tipo"
      | (_, TVar (_ as name)) -> 
          if not (isContainedInExpr name (fst hd)) 
          then (snd hd, fst hd)::solveConstraints (subst (fst hd) name tl)
          else failwith "Errore occorrenza tipo"
            
      (* Regola 3 *)
      | (TFun(a,b), TFun(c,d)) 
      | (TPair(a,b),TPair(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
      
      | (TList [a], TList [b]) -> solveConstraints ((a,b)::tl)
      
      (* Altrimenti ... *)
      | _ -> constrsList;;





(* Genera una coppia (tipo espressione, lista di vincoli) *)
let rec getConstraints expr typeEnv = 
  match expr with
      Val nome -> (applytypenv typeEnv nome, [])     
        
    | Echar c -> (TChar,[])
    | Eint x -> (TInt, [])
             
    | True 
    | False -> (TBool, [])
        
    | Sum(t1,t2) 
    | Diff(t1,t2) 
    | Times(t1,t2) -> 
        let ((t1Type, t1Constrs),(t2Type, t2Constrs)) = 
          (getConstraints t1 typeEnv, getConstraints t2 typeEnv)
        in (TInt, ([(t1Type,TInt)]@
                   [(t2Type,TInt)]@
                    (t1Constrs)@
                    (t2Constrs)))
        
    | And(b1,b2) 
    | Or(b1,b2) -> 
        let ((b1Type,b1Constrs),(b2Type,b2Constrs)) = 
          (getConstraints b1 typeEnv, getConstraints b2 typeEnv)
        in(TBool, ([(b1Type,TBool)]@
                   [(b2Type, TBool)]@
                    (b1Constrs)@
                    (b2Constrs)))
            
    | Not(b) ->
        let (bType,bConstrs) = getConstraints b typeEnv
        in (TBool, ([(bType, TBool)]@
                     (bConstrs)))
             
    | Less(t1,t2) ->
        let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv, getConstraints t2 typeEnv)
        in (TBool, ([(t1Type, TInt)]@
                    [(t2Type, TInt)]@
                     (t1Constrs)@
                     (t2Constrs)))
             
    | Eq(t1,t2) ->
        let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv, getConstraints t2 typeEnv)
        in (TBool, ([(t1Type, t2Type)]@
                    [(t1Type,t1Type)]@
                    [(t2Type,t2Type)]@
                     (t1Constrs)@
                     (t2Constrs))) 
             
    | Epair(t1,t2) -> 
        let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv, getConstraints t2 typeEnv)
        in (TPair(t1Type,t2Type), ([(t1Type,t1Type)]@
                                   [(t2Type,t2Type)]@
                                    (t1Constrs)@
                                    (t2Constrs)))
             
    | Fst(t) ->
        let (tType, tConstrs) = getConstraints t typeEnv
        in (match tType with
                (TPair(typeL,typeR)) -> 
                  (typeL, ([tType, TPair(typeL,typeR)]@(tConstrs)))
              | TVar n ->
                  let a = newvar()
                  in (a, [tType, TPair(a,newvar())]@tConstrs)
              | _ -> failwith "getConstraints:Fst - Espressione non valida")
             
    | Snd(t) ->
        let (tType,tConstrs) = getConstraints t typeEnv
        in (match tType with
                (TPair(typeL,typeR)) -> 
                  (typeR, ([(tType, TPair(typeL,typeR))]@(tConstrs)))
              | TVar n -> 
                  let a = newvar()
                  in (a, [tType, TPair(newvar(),a)]@tConstrs)
              | _ -> failwith "getConstraints:Snd - Espressione non valida")
             
    | Head l -> 
        let (typeL, lConstraints) = getConstraints l typeEnv
        in (match typeL with
                (TList [lType]) -> (lType, ([(typeL, TList [lType])]@lConstraints))
              | TVar n ->
                  let a = newvar()
                  in (a, ([(TList [a], typeL)]@lConstraints))
              | _ -> failwith "getConstraints:Head - L'espressione non è una lista")

    | Tail l ->
        let (lType,lConstraints) = getConstraints l typeEnv execEnv
        in (match lType with
                (TList [typel]) -> (lType, (lConstraints))
              | TVar n -> 
                  let a = newvar()
                  in (lType, ([TList[a], lType]@lConstraints))
              | _ -> failwith "L'espressione non è una lista")

             
             
             
    | Cons(t1,t2) ->
        let ((t1Type, t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints t1 typeEnv, getConstraints t2 typeEnv)
        in (TList [t1Type], ([t1Type,t1Type]@
                               [(t2Type, TList [t1Type])]@
                               (t1Constrs)@
                               (t2Constrs))) 
             
    | Empty -> (TList [newvar()], [])
        
    | Ifthenelse(b,t1,t2) ->
        let ((bType,bConstrs),(t1Type,t1Constrs),(t2Type,t2Constrs)) = 
          (getConstraints b typeEnv,
           getConstraints t1 typeEnv,
           getConstraints t2 typeEnv)
        in  (t1Type, ([(bType,TBool)]@
                        [t1Type,t2Type]@
                        (bConstrs)@
                        (t1Constrs)@
                        (t2Constrs)))
              
    | Let (name,t1,t2) -> 
        let a = newvar() in
        let (t1Type,t1Constrs) = (getConstraints t1 typeEnv)
        in let (t2Type,t2Constrs) = (getConstraints t2 (bindtyp typeEnv name t1Type))
        in (t2Type, ([(t1Type, a)]@
                       (t1Constrs)@
                       (t2Constrs)))
             
    | Fun(x,t) ->
        let a = newvar() in
        let (tType, tConstrs) = getConstraints t (bindtyp typeEnv x a)
        in (TFun(a,tType),tConstrs)
             

    | Appl(t1,t2) ->
        let a = newvar()
        in let (t1Type, t1Constrs) = getConstraints t1 typeEnv
        in let (t2Type, t2Constrs) = getConstraints t2 typeEnv
        in (a, ([t1Type, TFun(t2Type,a)]@
                  (t1Constrs)@
                  (t2Constrs)))
             
    | Rec(y,f) -> 
        (match f with
             Fun(x,t) ->
               let a = newvar() 
               in let (fType, fConstrs) = 
                   getConstraints f (bindtyp typeEnv y a)
               in (fType, ([a,a]@fConstrs))
           | _ -> failwith "getConstraints:Rec - Il secondo termine non è una funzione")
;;


(* Se il tipo è inferibile, ovvero rispetta le regole (verificato da solveConstraints), allora restituisci il suo tipo *)
let rec typeinf expr = 
  let exprConstraints = getConstraints expr newtypenv in
  let unifiedConstrs = solveConstraints (snd exprConstraints) in 
(* fst exprConstraints,"separatore", snd exprConstraints, "separatore",
  unifiedConstrs,"separatore" ,solveRemainingConstrs (fst exprConstraints) unifiedConstrs *)
    (if unifiedConstrs = [] then fst exprConstraints else
       solveRemainingConstrs  (fst exprConstraints) unifiedConstrs);;
