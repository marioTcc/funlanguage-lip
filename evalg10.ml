
type ide = Ide of string;;

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
| Rec of ide * exp
;;


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

(* Ambiente di esecuzione *)
let emptyenv = function (i:ide) -> Undefined;;
let applyenv ((r:env),(x:ide))= r x ;;
let bind (ambiente, nome, ev) =
 function lu -> if lu = nome then ev else applyenv (ambiente, lu);;


let typeChecker (tipo,value) =
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
    | "list" -> (match value with
                     List(x) -> true
                  | _ -> false)
    | "pair" -> (match value with
                     Pair(x,y) -> true
                  | _ -> false)
    | _ -> failwith ("Invalid type");;

let eq (x,y) =
  if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with (Int(u), Int(w)) -> Bool(u = w))
  else
    if typeChecker("char",x) && typeChecker("char",y) then(
      match (x,y) with
        (Char(u),Char(w)) -> Bool(u = w))     
    else
      if typeChecker("bool",x) && typeChecker("bool",y) then(
        match (x,y) with
          (Bool(u),Bool(w)) -> Bool(u = w))
      else
	if typeChecker("list",x) && typeChecker("list",y) then(
          match (x,y) with
            (List(u),List(w)) -> Bool(u = w))
	else
	  if typeChecker("pair",x) && typeChecker("pair",y) then(
            match (x,y) with
              (Pair(u,v),Pair(w,z)) ->
		Bool(u = w && v = z))
          else
            failwith ("Errore sul tipo dei dati");;

let less (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Bool(u < w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let sum (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u+w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let diff (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u-w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;
  

let times (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u*w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let logicAnd (x,y) =
    if typeChecker("bool",x) && typeChecker("bool",y) then (
        match (x,y) with
              (Bool(u), Bool(w)) -> Bool(u && w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let logicOr (x,y) =
    if typeChecker("bool",x) && typeChecker("bool",y) then (
        match (x,y) with
              (Bool(u), Bool(w)) -> Bool(u || w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let unaryNegation x =
    if typeChecker("bool",x) then (
        match x with
              Bool(y) -> Bool(not y)
            | (_) -> failwith ("Invalid type") )
    else failwith ("Type error");;

let pair (x,y) = Pair(x,y);;

let cons a b = match b with
    (List t) -> List (a::t)
   |_ ->failwith"";;

let head l = match l with
    (List (hd::tl)) -> hd
  | _ -> failwith "";;

let tail l = match l with
    (List (hd::tl)) -> List tl
  | _ -> failwith "";;
  

(* TODO eccezione valore non in ambiente vecchio *)
let rec calcFV expr amb_old amb_new = match expr with

    (* Se si incontra un identificatore *)
    Val var -> bind (amb_new, var, applyenv (amb_old,var)) 

  | Eint a -> amb_new
  | Echar a -> amb_new
  | True | False | Empty -> amb_new

  | Sum(t1,t2) | Diff(t1,t2) | Times(t1,t2) 
  | And(t1,t2) | Or(t1,t2) | Eq(t1,t2) | Less(t1,t2) 
  | Cons(t1,t2) | Epair(t1,t2) | Appl(t1,t2) 
      -> calcFV t1 amb_old (calcFV t2 amb_old amb_new)

  | Head t | Tail t | Fst t | Snd t | Not t -> calcFV t amb_old amb_new (* Per ora il Not lo parcheggio qui, ma boh *)

  | Ifthenelse (t0,t1,t2) -> calcFV t0 amb_old (calcFV t1 amb_old (calcFV t2 amb_old amb_new))

  | Fun (x, t1) -> calcFV t1 amb_old (bind (amb_new, x, Undefined))
  | Rec (y, (Fun(x,t) as t1)) -> calcFV t1 amb_old amb_new

  | Let (x, t1, t2) -> calcFV t1 amb_old 
      (calcFV t2 amb_old (bind (amb_new, x, Undefined)))
;;



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
  | Not(a) -> Not(substRec newVal oldVal a)
  | Head a -> Head(substRec newVal oldVal a)
  | Tail a -> Tail(substRec newVal oldVal a)
  | Fst a -> Fst(substRec newVal oldVal a)
  | Snd a -> Snd(substRec newVal oldVal a)
  | Ifthenelse(a,b,c) -> Ifthenelse(substRec newVal oldVal a, substRec newVal oldVal b, substRec newVal oldVal c)
  | Let(a,b,c) -> Let(a, substRec newVal oldVal b, substRec newVal oldVal c)
  | Fun(x,t) -> Fun(x, substRec newVal oldVal t)
  | Appl(a,b) -> Appl(substRec newVal oldVal a, substRec newVal oldVal b)
  | _ -> failwith "Errore nella sostituzione Rec";;

let rec sem (e:exp) (amb:env) =
  match e with
      | Val x -> applyenv (amb, x)
      | Eint(n) -> Int(n)
      | Echar(b) -> Char(b)
      | True -> Bool(true)
      | False -> Bool(false)
      | Empty -> List []
      | Cons(a,b) -> cons (sem a amb) (sem b amb)
      | Head a -> head (sem a amb) 
      | Tail a -> tail  (sem a amb)
      | Epair(a,b) -> pair(sem a amb,sem b amb)
      | Fst(Epair(a,b)) -> sem a amb
      | Snd(Epair(a,b)) -> sem b amb
      | Eq(a,b) -> eq( (sem a amb),(sem b amb) )
      | Times(a,b) -> times( (sem a amb),(sem b amb) )
      | Sum(a,b) -> sum( (sem a amb),(sem b amb) )
      | Diff(a,b)  -> diff( (sem a amb),(sem b amb) )
      | And(a,b) -> logicAnd( (sem a amb),(sem b amb) )
      | Or(a,b) ->  logicOr( (sem a amb),(sem b amb) )
      | Less(a,b) -> less( sem a amb,sem b amb)
      | Not(a) -> unaryNegation( (sem a amb) )
      | Ifthenelse(a,b,c) ->  let g = sem a amb in if typeChecker("bool",g) then
                                                 (if g = Bool(true) 
                                                   then sem b amb else sem c amb)
                           else failwith ("Condizione booleana non rispettata")
      | Let(a,b,c) -> sem c (bind (amb,a,(sem b amb)))
      | Fun(a,b) -> Closure( Fun(a,b), calcFV b amb emptyenv) 
      | Rec(a,(Fun(x,t))) -> 
          let tSubst = substRec (Rec(a,Fun(x,t))) a t in
            Closure(Fun(x,tSubst), calcFV tSubst amb emptyenv)
      | Appl(a,b) -> (match sem a amb with
                          Closure(Fun(parametro,corpo), amb_locale) ->
                            sem corpo (bind (amb_locale, parametro, sem b amb))
                        |  _ -> failwith "Funzione non valida")
      | _ -> failwith "Command not recognized";;
