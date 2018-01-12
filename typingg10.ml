


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
[] -> failwith "listaVuota"
  |(i,e)::[] -> if (confrontIde i s) then e else failwith "nonPresente"
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
  | _ -> failwith "Errore";;

(* Valuta se sia possibile risolvere i vincoli rimanenti e inferire il tipo dell'espressione *) 
let rec solveRemainingConstrs expType remConstrs = match remConstrs with
    [] -> expType
  | (TVar name, expr)::tl | (expr, TVar name)::tl -> 
      solveRemainingConstrs (getType expType (name, expr)) tl
  | _ -> failwith "Tipo non inferibile (solving)";;







(* Risolve i vincoli (entry point dell'algoritmo di unificazione) *)
let rec solveConstraints constrs = match constrs with
    [] -> ([]:(etype * etype) list)
  | hd::tl as constrsList -> match fst hd, snd hd with
         (* Regola 1 *)
         (TInt,TInt) | (TBool,TBool) | (TChar,TChar) -> solveConstraints tl
       | (TVar n1, TVar n2) -> if n1 = n2 then solveConstraints tl else solveConstraints tl@[hd]
         (* Regola 2 *)
       | (TVar (_ as name), _) -> if not (isContainedInExpr name (snd hd)) 
                          then (fst hd, snd hd)::solveConstraints (subst (snd hd) name tl)
                          else failwith "Erroe occorrenza tipo"
       | (_, TVar (_ as name)) -> if not (isContainedInExpr name (fst hd)) 
                          then (fst hd, snd hd)::solveConstraints (subst (fst hd) name tl)
                          else failwith "Errore occorrenza tipo"
          (* Regola 3 *)
       | (TFun(a,b), TFun(c,d)) | (TPair(a,b),TPair(c,d)) -> solveConstraints ((a,c)::(b,d)::tl)
       | (TList [a], TList [b]) -> solveConstraints ((a,b)::tl)
          (* Altrimenti ... *)
       | _ -> constrsList;;





(* Genera una coppia (tipo espressione, lista di vincoli) *)
let rec getConstraints expr amb = match expr with
    Val nome -> (applytypenv amb nome, [])
(* Inferenza per caratteri *)
  | Echar c -> (TChar,[])
(* Inferenza per numerici *)
  | Eint x -> (TInt, [])
  | Sum(t1,t2) | Diff(t1,t2) | Times(t1,t2) -> 
      let ((t1Type, t1Constrs),(t2Type, t2Constrs)) = (getConstraints t1 amb, getConstraints t2 amb)
            in (TInt, ([(t1Type,TInt)]@
                       [(t2Type,TInt)]@
                        (t1Constrs)@
                        (t2Constrs)))
(* Inferenza per booleani o per espressioni booleane *)
  | True | False -> (TBool, [])
  | And(b1,b2) | Or(b1,b2) -> 
      let ((b1Type,b1Constrs),(b2Type,b2Constrs)) = (getConstraints b1 amb, getConstraints b2 amb)
        in(TBool, ([(b1Type,TBool)]@
                   [(b2Type, TBool)]@
                    (b1Constrs)@
                    (b2Constrs)))
  | Not(b) ->
      let (bType,bConstrs) = getConstraints b amb
        in (TBool, ([bType, TBool]@
                    (bConstrs)))
  | Less(t1,t2) ->
      let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1 amb, getConstraints t2 amb)
        in (TBool, ([(t1Type, TInt)]@
                    [(t2Type, TInt)]@
                    (t1Constrs)@
                    (t2Constrs)))
  | Eq(t1,t2) ->
      let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1 amb, getConstraints t2 amb)
        in (TBool, ([(t1Type, t2Type)]@
                    [(t1Type, t1Type)]@
                    [(t2Type, t2Type)]@
                    (t1Constrs)@
                    (t2Constrs))) 
(* Inferenza per il tipo coppia *)
  | Epair(t1,t2) -> 
      let ((t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1 amb, getConstraints t2 amb)
        in ( TPair(t1Type,t2Type),([(t1Type,t1Type)]@
                                   [(t2Type,t2Type)]@
                                    (t1Constrs)@
                                    (t2Constrs)))
  | Fst(Epair(a,b) as t) ->
      let (tType,tConstrs) = getConstraints t amb
        in (match tType with
            (TPair(typeL,typeR)) -> (typeL, ([(TPair(typeL,typeR), TPair(typeL,typeR))]@
                     (tConstrs)))
          | _ -> failwith "L espressione non e una coppia")
  | Snd(Epair(a,b) as t) ->
      let (tType,tConstrs) = getConstraints t amb
        in (match tType with
                (TPair(typeL,typeR)) -> (typeR, ([(TPair(typeL,typeR), TPair(typeL,typeR))]@
                     (tConstrs)))
              | _ -> failwith "L espressione non e una lista")
(* Inferenza per il tipo lista !! potrebbe servire modificarli !!! TODO *)
  | Head l -> 
      let (TList [lType], lConstraints) = getConstraints l  amb
        in (lType, ([(TList [lType], TList [lType])]@lConstraints)) 
  | Tail l ->
      let (lType,lConstraints) = getConstraints l amb
        in (lType, (lConstraints))
  | Cons(t1,t2) ->
      let ((t1Type, t1Constrs),(t2Type,t2Constrs)) = (getConstraints t1 amb, getConstraints t2 amb)
        in (TList [t1Type], ([(t1Type,t1Type)]@
                             [(t2Type, TList [t1Type])])@
                               (t1Constrs)@
                               (t2Constrs))
  | Empty -> (TList [newvar()], [])
(* Inferenza per if-then-else *)
  | Ifthenelse(b,t1,t2) ->
      let ((bType,bConstrs),(t1Type,t1Constrs),(t2Type,t2Constrs)) = (getConstraints b amb,getConstraints t1 amb, getConstraints t2 amb)
        in  (t1Type, ([(bType,TBool)]@
                      [(t1Type,t2Type)]@
                      (bConstrs)@
                      (t1Constrs)@
                      (t2Constrs)))
(* Inferenza per Let *)
  | Let (name,t1,t2) -> 
      let a = newvar() in
        let (t1Type,t1Constrs) = (getConstraints t1 amb)
          in let (t2Type,t2Constrs) = (getConstraints t2 (bindtyp amb name a))
             in (t2Type, ([(t1Type, a)]@
                           (t1Constrs)@
                           (t2Constrs)))
 (* Inferenza per Fun *)
  | Fun(x,t) ->
      let a = newvar() in
      let (tType, tConstrs) = getConstraints t (bindtyp amb x a)
      in (TFun(a,tType), tConstrs)
(* Inferenza per Appl *)
  | Appl(t1,t2) ->
      let a = newvar() in
      let (t1Type, t1Constrs) = getConstraints t1 amb in
        let (t2Type, t2Constrs) = getConstraints t2 amb 
        in (a, ([t1Type, TFun(t2Type,a)]@
                  (t1Constrs)@
                  (t2Constrs)))
(* Inferenza per Rec *)
  | Rec(y, Fun(x,t)) -> 
      let a = newvar() in
      let (tType, tConstrs) = getConstraints (Fun(x,t)) (bindtyp amb y a)
      in match tType with
          TFun(xType,termType) -> (TFun(xType,termType), ([(TFun(xType,termType),a)]@tConstrs))
          | _ -> failwith "Il secondo termine non è una funzione"
(* Rompere solo in caso di incendio *)
  | _ -> failwith "Tipo non inferibile (getConstraints)";;


(* Se il tipo e inferibile, ovvero rispetta le regole (verificato da solveConstraints), allora restituisci il suo tipo *)
let rec typeinf expr = let exprConstraints = getConstraints expr newtypenv in
let unifiedConstrs = solveConstraints (snd exprConstraints) in 
(if unifiedConstrs = [] then fst exprConstraints else
     solveRemainingConstrs  (fst exprConstraints) unifiedConstrs);;



(*
typeinf (Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), Appl(Rec(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 2))), Eint 2)));;
typeinf (Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), Eint 3));;
typeinf (Eq(Appl(Fun(Ide "x", Val( Ide "x")), Eint 2), 
            Appl(Rec(Ide "x", Fun(Ide "y", Val(Ide "y"))), Eint 2)));;
*)

