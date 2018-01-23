#use "evaltryg10.ml";;

let test1 = Try(Raise(Ide "x"),Ide "x", Eint 1);;
semtry test1 emptyenv;;
(* valore (semtry): eval = Int 1 *)


let test2 = 
  Appl(
    Fun(Ide "z",
        Try(
          Try(
            Ifthenelse(
              Eq(
                Val(Ide "z"),
                Eint 1),
              Raise(Ide "x"),
              Raise(Ide "y")),
            Ide "x", 
            Eint 1),
          Ide "y",
          Eint 2)),
    Eint 2);;
semtry test2 emptyenv;;
(* valore (semtry): eval = Int 2 *)


let test3 = 
  Appl(
    Fun(Ide "x",
        Ifthenelse(
          Eq(
            Val(Ide "x"),
            Eint 1),
          Try(
            Raise(Ide "uno"),
            Ide "uno", 
            Eint 1),
          Try(
            Raise(Ide "due"),
            Ide "due", 
            Eint 2))),
    Eint 1);;
semtry test3 emptyenv;;
(* valore (semtry): eval = Int 1 *)


let test4 = Appl(Fun(Ide "x",Ifthenelse(Eq(Val(Ide "x"),Eint 1),Try(Raise(Ide "uno"),Ide "uno", Eint 1),Try(Raise(Ide "due"),Ide "due", Eint 2))),Eint 3);;
semtry test4 emptyenv;;
(* valore (semtry): eval = Int 2 *)
 
let test5 = Sum(Let(Ide "f",Fun(Ide "x",Try(Ifthenelse(Val(Ide "x"),Eint 1, Raise(Ide "uno")),Ide "uno",Eint 2)),Appl(Val(Ide "f"),False)),Eint 9);;
semtry test5 emptyenv;;
(* valore (semtry): eval = Int 11 *)



(* --------------------- *)
let testX = 
  Appl(
    Fun(Ide "x",
        Ifthenelse(
          Eq(
            Val(Ide "x"),
            Eint 1),
          Try(
            Raise(Ide "uno"),
            Ide "uno", 
            Val(Ide"x")),
          Try(
            Raise(Ide "due"),
            Ide "due", 
            Let(Ide "x", False,

            And(Val(Ide "x"),True))



               ))),
    Eint 1);;
semtry testX emptyenv;;
(* ------------------------- *)

