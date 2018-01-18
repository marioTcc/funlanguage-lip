  
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
| Try of exp * ide * exp
| Raise of ide
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

let emptyenv = function (i:ide) -> Undefined;;
let applyenv ((r:env),(x:ide))= r x ;;
let bind (ambiente, nome, ev) =
 function lu -> if lu = nome then ev else applyenv (ambiente, lu);;

(* Stack delle eccezioni *)
let emptyStack = ([]:(ide*exp)list);;
let bindInStack (name:ide) (expr:exp) (stack:(ide*exp)list) = (name,expr)::stack;;
let rec getFromStack (name:ide) (stack:(ide*exp)list) = match stack with
    [] -> failwith "Eccezione non presente nello stack"
  | hd::tl -> if fst hd = name then (snd hd, tl) else getFromStack name tl;;



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
    else failwith "Sum operator mismatch ";;

let diff (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u-w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Diff operators mismatch");;
  

let times (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Int(u*w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Times operators mismatch");;

let logicAnd (x,y) =
    if typeChecker("bool",x) && typeChecker("bool",y) then (
        match (x,y) with
              (Bool(u), Bool(w)) -> Bool(u && w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("One or more than one parameters are not of boolean type");;

let logicOr (x,y) =
    if typeChecker("bool",x) && typeChecker("bool",y) then (
        match (x,y) with
              (Bool(u), Bool(w)) -> Bool(u || w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("One or more than one parameters are not of boolean type");;

let unaryNegation x =
    if typeChecker("bool",x) then (
        match x with
              Bool(y) -> Bool(not y)
            | (_) -> failwith ("Invalid type") )
    else failwith ("Parameter is not of boolean type");;

let pair (x,y) = Pair(x,y);;

let cons a b = match b with
    (List t) -> List (a::t)
   |_ ->failwith "Parameters type mismatch";;

let head l = match l with
    (List (hd::tl)) -> hd
  | _ -> failwith ("Empty list, cannot find head in such list");;

let tail l = match l with
    (List (hd::tl)) -> List tl
  | _ -> failwith ("Empty list, cannot find tail in such list");;


let rec calcFV expr amb_old amb_new =
  match expr with
    Val var -> bind (amb_new, var, applyenv (amb_old,var)) 
  | Eint a -> amb_new
  | Echar a -> amb_new
  | True | False | Empty -> amb_new
  | Sum(t1,t2) | Diff(t1,t2) | Times(t1,t2) 
  | And(t1,t2) | Or(t1,t2) | Eq(t1,t2) | Less(t1,t2) 
  | Cons(t1,t2) | Epair(t1,t2) | Appl(t1,t2) 
      -> calcFV t1 amb_old (calcFV t2 amb_old amb_new)
  | Head t | Tail t | Fst t | Snd t | Not t -> calcFV t amb_old amb_new (* Per ora il Not lo parcheggio qui, ma boh *)
  | Ifthenelse (t0,t1,t2) ->
     calcFV t0 amb_old (calcFV t1 amb_old (calcFV t2 amb_old amb_new))
  | Fun (x, t1) -> calcFV t1 amb_old (bind (amb_new, x, Undefined))
  | Rec (y, (Fun(x,t) as t1)) -> calcFV t1 amb_old amb_new
  | Let (x, t1, t2) -> calcFV t1 amb_old 
      (calcFV t2 amb_old (bind (amb_new, x, Undefined)))
  | Try (t1,x,t2)->
     calcFV t1 amb_old (calcFV t2 amb_old (bind(amb_new, x, Undefined)))
  | Raise(x) -> amb_new;;

let rec substRec newVal oldVal expr = match expr with
    Val name -> if name = oldVal then newVal else Val name
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
  | Ifthenelse(a,b,c) ->
     Ifthenelse(substRec newVal oldVal a,
		substRec newVal oldVal b,
		substRec newVal oldVal c)
  | Let(a,b,c) -> Let(a, substRec newVal oldVal b, substRec newVal oldVal c)
  | Fun(x,t) -> Fun(x, substRec newVal oldVal t)
  | Appl(a,b) -> Appl(substRec newVal oldVal a, substRec newVal oldVal b)
  | _ -> failwith "Errore nella sostituzione Rec";;

let semtry (e:exp) (amb:env) =
  let rec sem e amb stack = 
  match e with
      | Val x -> applyenv (amb, x)
      | Eint(n) -> Int(n)
      | Echar(b) -> Char(b)
      | True -> Bool(true)
      | False -> Bool(false)
      | Empty -> List []
      | Cons(a,b) -> cons (sem a amb stack) (sem b amb stack)
      | Head a -> head (sem a amb stack) 
      | Tail a -> tail  (sem a amb stack)
      | Epair(a,b) -> pair(sem a amb stack,sem b amb stack)
      | Fst(Epair(a,b)) -> sem a amb stack
      | Snd(Epair(a,b)) -> sem b amb stack
      | Eq(a,b) -> eq( (sem a amb stack),(sem b amb stack) )
      | Times(a,b) -> times( (sem a amb stack),(sem b amb stack) )
      | Sum(a,b) -> sum( (sem a amb stack),(sem b amb stack) )
      | Diff(a,b)  -> diff( (sem a amb stack),(sem b amb stack) )
      | And(a,b) -> logicAnd( (sem a amb stack),(sem b amb stack) )
      | Or(a,b) ->  logicOr( (sem a amb stack),(sem b amb stack) )
      | Less(a,b) -> less( sem a amb stack,sem b amb stack)
      | Not(a) -> unaryNegation( (sem a amb stack) )
      | Ifthenelse(a,b,c) ->
	 let g = sem a amb stack in
	 if typeChecker("bool",g) then
             (if g = Bool(true) 
              then sem b amb stack else sem c amb stack)
           else failwith ("Condizione booleana non rispettata")
      | Let(a,b,c) -> sem c (bind (amb,a,(sem b amb stack))) stack
      | Fun(a,b) -> Closure( Fun(a,b), calcFV b amb emptyenv) 
      | Rec(a,(Fun(x,t))) -> 
          let tSubst = substRec (Rec(a,Fun(x,t))) a t in
            Closure(Fun(x,tSubst), calcFV tSubst amb emptyenv)
      | Appl(a,b) ->
	 (match sem a amb stack with
           Closure(Fun(parametro,corpo), amb_locale) ->
             sem corpo (bind (amb_locale, parametro, sem b amb stack)) stack
                        |  _ -> failwith "Funzione non valida")
      | Try(a,b,c) -> sem a amb (bindInStack b c stack)
      | Raise b -> let (handler, newStack) = getFromStack b stack
                in sem handler amb newStack
      | _ -> failwith "Command not recognized"

  in sem e amb emptyStack;;




semtry(
  Try(    
    (Try (
       (Ifthenelse( Eq(Val (Ide "x"), Echar 'c'),
        Raise (Ide "ecc1"),
        Raise (Ide "ecc2")
              )),
            Ide "ecc2",
            Eint 3)),
           Ide "ecc1",
           Eint 4)
)
(bind (emptyenv,(Ide "x"), Char 'c'));;


