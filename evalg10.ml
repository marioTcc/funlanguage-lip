
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
    | _ -> failwith ("Invalid type");;

let eq (x,y) =
    if typeChecker("int",x) && typeChecker("int",y) then (
        match (x,y) with
              (Int(u), Int(w)) -> Bool(u = w)
            | (_, _) -> failwith ("Invalid type") )
    else failwith ("Type error");;

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
      | Let(a,b,c) -> sem c (bind (emptyenv,a,(sem b emptyenv)));;
      | Fun(a,b) -> 
      | _ -> failwith "Command not recognized";;


 sem(Times(Eint 4,Eint 5));;
 sem(Eq(Eint 2,Eint 4));;
 sem(Eq(Eint 2,Eint 2));;
 sem(Times(Eint 3,Eint 4));;
 sem(Sum(Eint 3,Eint 2));;
 sem(Diff(Eint 5,Eint 3));;
 sem(Diff(Eint 5,Eint 8));;    
 sem(And(True,False));;
 sem(And(True,True));;
 sem(And(False,True));;
 sem(And(False,False));;
 sem(Or(True,False));;
 sem(Or(True,True));;
 sem(Or(False,True));;
 sem(Or(False,False));;     
 sem(Less(Eint 5,Eint 3));;
 sem(Less(Eint 3,Eint 5));;
 sem(Not(True));;
 sem(Not(False));;
 sem(True);;
 sem(False);;
 sem(Empty);;
 sem(Fst(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) )));;
 sem(Snd(Epair( Sum(Eint 5,Eint 3) , Diff(Eint 5,Eint 3) )));;    
 sem(Sum(Eint 2,Eint 3));;     
 sem ((Cons(Eint 4,Cons(Eint 2,(Cons(Eint 1,Empty))))));;
 sem ((Head(Cons(Eint 2,(Cons(Eint 1,Empty))))));;
 sem ((Head(Cons(Eint 2,Empty))));;
 sem ((Tail((Cons(Eint 3,Cons(Eint 2,(Cons(Eint 1,Empty))))))));;   
 sem ((Tail(Cons(Eint 10,Empty))));; 
 
sem(Sum( Val (Ide "x"), Eint 2) ) (bind (emptyenv, (Ide "x"), (Int 3)));;
sem(Let((Ide "x"),(Sum(Eint 2,Eint 3)),(Times(Val(Ide "x"), Eint 3))))emptyenv;;
