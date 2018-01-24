
#use "evalg10.ml";;

(* Vari tests *)
let giuste = 
  [Eint 2, Int 2; 
   Echar 'c', Char 'c'; 
   True, Bool true; 
   False, Bool false; 
   Empty, List [];
   Sum(Eint 2, Eint 3), Int 5;
   Diff(Eint 2, Eint 3), Int (-1);
   Times(Eint 2, Eint 3), Int 6; 
   And(True, False), Bool false;
   Or(False, True), Bool true; 
   Not(True), Bool false;
   Less(Eint 2, Eint 3), Bool true; 
   Less(Sum(Eint 2, Eint 3), Eint 3), Bool false; 
   Less(Diff(Eint 2, Eint 3), Eint 6), Bool true;
   Eq(Eint 2, Eint 4), Bool false; 
   Eq(Eint 3, Eint 3), Bool true; 
   Eq(Echar 'c', Echar 'd'), Bool false; 
   Eq(True, False), Bool false;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 2, Echar 'c')), Bool true;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 3214, Echar 'd')), Bool false;
   Eq(Cons(Eint 3, Empty), Cons(Eint 3, Empty)), Bool true;
   Eq(Cons(Eint 2, Empty), Cons(Eint 3, Empty)), Bool false;
   Eq(Let(Ide "x", Echar 'c', Val (Ide "x")), Echar 'c'), Bool true; 
   Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Eint 3), Bool false;
   Eq(Ifthenelse(True, Eint 2, Eint 3), Ifthenelse(False, Eint 3, Eint 4)), Bool false;
   Cons(Eint 3, Empty), List [Int 3]; 
   Cons(Echar 'c', Empty), List [Char 'c']; 
   Cons(True, Empty), List [Bool true]; 
   Cons(Eint 2, Cons(Eint 3, Empty)), List [Int 2; Int 3];
   Cons(True, Cons(False, Empty)), List [Bool true; Bool false];
   Cons(Echar 'c', Cons(Echar 'd', Empty)), List [Char 'c'; Char 'd'];
   Cons(Cons(Eint 2, Empty), Cons(Cons(Eint 3, Empty), Empty)), List [List [Int 2]; List [Int 3]];
   Cons(Cons(Let(Ide "x", Echar 'c', Val (Ide "x")), Empty), Cons(Empty, Empty)), List [List[Char 'c']; List[]];
(*   Cons(Cons(Fun(Ide "x", Val(Ide "x")), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty)), List [Closure(;*) 
   Head(Cons(Eint 3, Empty)), Int 3; 
   Head(Cons(Echar 'c', Empty)), Char 'c';
   Head(Cons(True, Empty)), Bool true; 
   Head(Cons(Eint 2, Cons(Eint 3, Empty))), Int 2;
   Head(Cons(True, Cons(False, Empty))), Bool true; 
   Head(Cons(Echar 'c', Cons(Echar 'd', Empty))), Char 'c';
   Tail(Cons(Eint 3, Empty)), List []; 
   Tail(Cons(Echar 'c', Empty)), List [];
   Tail(Cons(True, Empty)), List []; 
   Tail(Cons(Eint 2, Cons(Eint 3, Empty))), List [Int 3];
   Tail(Cons(True, Cons(False, Empty))), List [Bool false];
   Tail(Cons(Echar 'c', Cons(Echar 'd', Empty))), List [Char 'd'];
   Epair(Eint 2, Eint 3), Pair(Int 2, Int 3); 
   Epair(Echar 'c', Echar 'd'), Pair(Char 'c', Char 'd'); 
   Epair(True, False), Pair(Bool true, Bool false);
   Epair(Sum(Eint 2, Eint 3), Or(True, False)), Pair(Int 5, Bool true); 
   Fst(Epair(Eint 2, Eint 3)), Int 2; 
   Fst(Epair(Echar 'c', Echar 'd')), Char 'c'; 
   Fst(Epair(True, False)), Bool true; 
   Fst(Epair(Sum(Eint 2, Eint 3), Or(True, False))), Int 5;
   Snd(Epair(Eint 2, Eint 3)), Int 3; 
   Snd(Epair(Echar 'c', Echar 'd')), Char 'd'; 
   Snd(Epair(True, False)), Bool false; 
   Snd(Epair(Sum(Eint 2, Eint 3), Or(True, False))), Bool true;
   Ifthenelse(True, Sum(Eint 1, Eint 2), Diff(Eint 11, Eint 3)), Int 3;
   Ifthenelse(False, True, False), Bool false;
   Ifthenelse(True, Echar 'c', Echar 'd'), Char 'c';
   Let(Ide "x", Eint 2, Sum(Val(Ide "x"),Eint 3)), Int 5;
   Let(Ide "x", True, And(Val(Ide "x"), False)), Bool false;
   Let(Ide "x", Echar 'c', Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), Char 'd';
   Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)), Appl(Val (Ide "x"), Eint 2)), Int 5;
(* Fun(Ide "x", Val (Ide "x")), TFun(TVar "_", TVar "_");
   Fun(Ide "x", Sum(Val(Ide "x"), Eint 2)), TFun(TInt,TInt);
   Fun(Ide "x", And(Val(Ide "x"), True)), TFun(TBool,TBool);
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TFun(TChar,TChar);
   Fun(Ide "x", Appl(Fun(Ide "y", Val (Ide "y")), Val( Ide "x"))), TFun(TVar "_",TVar "_");
*)
   Appl(Fun(Ide "x", Val (Ide "x")), Eint 2), Int 2;
   Appl(Fun(Ide "x", Val (Ide "x")), True), Bool true;
   Appl(Fun(Ide "x", Val (Ide "x")), Echar 'c'), Char 'c'
(*
   Rec(Ide "y", (Fun(Ide "x", Sum(Val (Ide "x"), Appl(Val (Ide "y"), Diff(Val (Ide "x"), Eint 1)))))), TFun(TInt,TInt);
   Rec(Ide "y", (Fun(Ide "x", And(Val (Ide "x"), Appl(Val (Ide "y"), False))))), TFun(TBool,TBool)
*)
];;
	


let sbagliate = [
  Eq(Epair(Eint 2, Echar 'c'),Epair(Echar 'd', Eint 3));
  Eq(Cons(Eint 2, Empty), Cons(Echar 'c', Empty));
  Eq(Ifthenelse(True, Echar 'c', Echar 'd'), Ifthenelse(False, Eint 3, Eint 4));
  Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Echar 'c');
  Cons(Cons(Fun(Ide "x", And(Val(Ide "x"), True)), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty));
];;



(*---------------------------------------------------------------------------
Test per typeinf
-------------------------------------------------------------------------------*)
let isValid (index,expr) = match expr with 
      Int x -> (index+1, true)
    | Char x -> (index+1, true)
    | Bool x -> (index+1, true)
    | Closure(a,b) -> (index+1,true)
    | Pair(a,b) -> (index+1,true)
    | List _ -> (index+1,true)
    | _ -> (index,false);;




let testGiusteScoppio l =
let check l = 
  List.fold_left (fun acc x -> if snd acc 
                  then (try isValid(fst acc, sem (fst x) emptyenv) with _ -> (fst acc,false))
                  else (fst acc,false)) (1,true) l
  in let result = check l
  in if snd result then "Tutto OK" else "Scoppio in "^ string_of_int (fst result);;




(* Ignora Fun, non essendo necessaria quell'uguaglianza *)
let testGiuste l =
  let rec testaGiustaExpr (expr,expected) = match expr, expected with
      (Int x, Int y) -> x = y
    | (Bool x, Bool y) -> x = y
    | (Char x,Char y) -> x = y
    | (Pair(a,b), Pair(c,d)) -> testaGiustaExpr (a,c) && testaGiustaExpr (b,d)
    | (Closure(a,c),b) -> true
    | (List [List a as c], List[List b as d]) -> testaGiustaExpr (c,d)
    | (List [], List []) -> true
    | (List a, List b) -> a = b
    | _ -> false
  in let check = List.fold_left (fun acc x -> 
                      if snd acc && testaGiustaExpr ((sem (fst x) emptyenv),(snd x)) 
                      then ((fst acc)+1, true) else ((fst acc), false) )
       (1, true) l
  in if (snd check) then "Tutto OK" else  ("Errore in "^ string_of_int (fst check));;




let testSbagliate l = 
  let check l = List.fold_left 
    (fun acc x -> if not (snd acc) then 
       (try isValid(fst acc, sem x emptyenv) with _ -> ((fst acc) + 1, false))
     else (fst acc, true)) (0,false) l
  in let result = check l
in if not (snd result) then "Tutto OK" else "La "^string_of_int (fst result)^" non scoppia come dovrebbe";;



(* AREA TEST *)

testGiusteScoppio giuste;; (* se va tutto bene, le espressioni "giuste" sono tutte inferibili *)

(* NON USARE se non si passa il test anti-scoppio *)
testGiuste giuste;;  (* verifica se il risultato di typeinf è quello atteso *)

testSbagliate sbagliate;; (* verifica che nessuna espressione "sbagliata" venga inferita come giusta *)





sem (Cons(Cons(Fun(Ide "x", Val(Ide "x")), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty))) emptyenv;;



let r1 = Rec(Ide "y", 
             Fun(Ide "x", 
                 Ifthenelse(
                   Eq(Val(Ide "x"), Eint 0),
                   Cons(Eint 5, Empty),
                   Cons(Val(Ide "x"),Appl(Val(Ide "y"), Diff(Val(Ide "x"), Eint 1))))));;

typeinf r1;;
sem r1 emptyenv;;

sem (Appl(Val(Ide "r1"), Val(Ide "r2"))) (bind(bind(emptyenv,Ide "r1", sem r1 emptyenv), Ide "r2", Int 150));;

let expr1 = 
  Let(Ide "y", 
      Eint 755,
),
       ;;

let e1 = Fun(Ide "x",Fun(Ide "z",Val(Ide "y")));;
let e2 = Let(Ide "y",Eint 3,(Appl(Val(Ide "f1"), Eint 321)));;

sem (Appl(e2,Echar 'c')) (bind(emptyenv, Ide "f1", sem e1 (bind(emptyenv, Ide "y", Int 755))));;



let e3 = 
  Let(Ide "y", Eint 10,
      Let(Ide "f", Fun(Ide "x", Cons(Val (Ide "y"), Empty)),
          Let(Ide "g", 
              Fun(Ide "z", Let(Ide "y", True, Appl(Val (Ide "z"),Empty))),
              Appl(Val(Ide "g"),Val(Ide "f")))));;

sem e3 emptyenv;;


let e4 = 
  Let(Ide "f", 
      Rec(Ide "fatt", 
          Fun(Ide "x",
              Ifthenelse(Eq(Val(Ide "x"), Eint 0),
                         Eint 1,
                         Times(Val(Ide "x"),
                               Appl(Val(Ide "fatt"),
                                    Diff(Val(Ide "x"),Eint 1)))))),
      Appl(Val(Ide "f"), Val(Ide "n")));;


sem e4 (bind(emptyenv,Ide "n", Int 25));;


let e5 = 
  Let(Ide "insert",
      Rec(Ide "insertRec",
          Fun(Ide "x",
              Ifthenelse(
                Eq(Snd(Val(Ide "x")), Empty),
                Cons(Fst(Val(Ide "x")), Empty),

                Ifthenelse(
                  Less(Fst(Val(Ide "x")), Head(Snd(Val(Ide "x")))),
                  Cons(Fst(Val(Ide "x")), Snd(Val(Ide "x"))),
                  Cons(Head(Snd(Val(Ide "x"))),
                       Appl(Val(Ide "insertRec"), 
                            Epair(Fst(Val(Ide "x")), Tail(Snd(Val(Ide "x")))))))
              ))),
          Let(Ide "sort",
              Rec(Ide "sortRec",
                  Fun(Ide "y",
                      Ifthenelse(Eq(Val(Ide "y"), Empty),
                                 Empty,
                                 Appl(Val(Ide "insert"),
                                      Epair(
                                        Head(Val(Ide "y")),
                                        Appl(Val(Ide "sortRec"), Tail(Val(Ide "y")))
                                      ))))),
              Appl(
                Val(Ide "sort"),
                Cons(Eint 3, Cons(Eint 4, Cons(Eint 1 ,Empty)))
              )));;
                                 


sem e5 emptyenv;;





let e6 =
         Let(Ide "sort",
             Rec(Ide "sortRec",
                 Fun(Ide "y",
                     Ifthenelse(Eq(Val(Ide "y"), Empty),
                                Appl(Val(Ide "sortRec"), Tail(Val(Ide "y")))
                                       ,
                                Cons(Eint 2, Empty)

                    ))),
             Appl(
               Val(Ide "sort"),
               Cons(Eint 3, Cons(Eint 4, Cons(Eint 1 ,Empty)))
             ));;

sem e6 emptyenv;;



sem (Ifthenelse(Eq(Val(Ide "y"), Empty), Empty, Tail(Val(Ide "y")))) (bind(emptyenv, Ide "y", List [Int 2;Int 5]));;
let e7 =
  Let(Ide "sort",
      Rec(Ide "sortRec",
          Fun(Ide "y",
              Ifthenelse(Eq(Val(Ide "y"), Empty),
                         Appl(Val(Ide "sortRec"), Tail(Val(Ide "y")))
                           ,
                         Cons(Eint 123, Empty)
                        ))),
      Appl(Val(Ide "sort"), Cons(Eint 3, Empty)));;
      


typeinf e7;;
sem e7 emptyenv;;

let e8 = (Fun(Ide "x", Head(Val(Ide "x")) ));;
let e9 = Fun(Ide "x", Tail(Val(Ide "x")));;
let e10 = Fun(Ide "x", Fst(Val(Ide "x")));;
let e11 = Fun(Ide "x", Snd(Val(Ide "x")));;
let e12 = Fun(Ide "x", Epair(Val(Ide "x"), Eint 2));;

inferType e8 newtypenv emptyenv;;
typeinf e8;;
sem (Appl(e8,Cons(Eint 1, Empty))) emptyenv;;
sem (Appl(e9,Cons(Eint 2, Cons(Eint 3, Empty)))) emptyenv;;
sem (Appl(e10,Epair(Eint 2, Eint 5))) emptyenv;;
sem e11 emptyenv;;
sem e12 emptyenv;;



match sem e12 emptyenv with
    Closure(a,b) -> applyenv (b,Ide "x");;

applyenv ((snd(sem e12 emptyenv)), Ide "x") ;;
typeinf e8;;
typeinf (Head(True));;
typeinf (Tail(Eint 2));;
typeinf (Fst(Eint 2));;
typeinf (Snd(Echar 'c'));;



let e423 =
  Rec(Ide "insertRec",
      Fun(Ide "x",
          Ifthenelse(Eq(Snd(Val(Ide "x")), Empty),
                     Cons(Fst(Val(Ide "x")), Empty),
                     Snd(Val(Ide "x"))
                    )));;

let e423 = Fun(Ide "x", Epair(Fst(Val(Ide "x")), Less(Fst(Val(Ide "x")), Head(Snd(Val(Ide "x"))))));;

sem (Appl(e423,Epair(Eint 2, Cons(Eint 3, Empty)))) emptyenv;;
typeinf e423;;

inferType (Rec(Ide "chezzi", Fun(Ide "x", Fst(Val(Ide "x"))))) newtypenv emptyenv;;


sem        (Let(Ide "sort",
                Rec(Ide "sortRec",
                    Fun(Ide "y",
                        Ifthenelse(Eq(Val(Ide "y"), Empty),
                                   Empty,
                                   Appl(Val(Ide "insert"),
                                        Epair(
                                          Head(Val(Ide "y")),
                                          Appl(Val(Ide "sort"), Tail(Val(Ide "y")))
                                        ))))),
                Appl(
                  Val(Ide "sort"),
                  Cons(Eint 3, Cons(Eint 4, Cons(Eint 1 ,Empty)))))) emptyenv;;


typeinf (Fun(Ide "x", Sum(Eint 2, Fst(Val(Ide "x")))));;
typeinf (Fun(Ide "x", Snd(Val(Ide "x"))));;
typeinf (Fun(Ide "x", Head(Val(Ide "x"))));;
typeinf (Fun(Ide "x", Tail(Val(Ide "x"))));;

typeinf (Let(Ide "f", Fun(Ide "x", Sum(Eint 2, Val(Ide "x"))), Rec(Ide "r", Val(Ide "f"))));;


















let e5 = 
  Let(Ide "insert",
      Rec(Ide "insertRec",
          Fun(Ide "x",
              Ifthenelse(
                Eq(Snd(Val(Ide "x")), Empty),
                Cons(Fst(Val(Ide "x")), Empty),

                Ifthenelse(
                  Less(Fst(Val(Ide "x")), Head(Snd(Val(Ide "x")))),
                  Cons(Fst(Val(Ide "x")), Snd(Val(Ide "x"))),
                  Cons(Head(Snd(Val(Ide "x"))),
                       Appl(
                         Val(Ide "insertRec"), 
                         Epair(Fst(Val(Ide "x")), Tail(Snd(Val(Ide "x"))))))
                )
              ))),
          Let(Ide "sort",
              Rec(Ide "sortRec",
                  Fun(Ide "y",
                      Ifthenelse(Eq(Val(Ide "y"), Empty),
                                 Empty,
                                 Appl(Val(Ide "insert"),
                                      Epair(
                                        Head(Val(Ide "y")),
                                        Appl(Val(Ide "sortRec"), Tail(Val(Ide "y")))
                                      ))      ))),
              Appl(
                Val(Ide "sort"),
                Cons(Eint 3, Cons(Eint 4, Cons(Eint 1 ,Empty)))
              )     ));;
              
                   
typeinf e5;;
sem e5 emptyenv;;















let e12 =     
  Rec(Ide "insertRec",
      Fun(Ide "x",
          Ifthenelse(
            Eq(Snd(Val(Ide "x")), Empty),
            Cons(Fst(Val(Ide "x")), Empty),
            Ifthenelse(
              Less(Fst(Val(Ide "x")), Head(Snd(Val(Ide "x")))),
              Cons(Fst(Val(Ide "x")), Snd(Val(Ide "x"))),
              Cons(Head(Snd(Val(Ide "x"))),
                   Appl(
                     Val(Ide "insertRec"), 
                     Epair(Fst(Val(Ide "x")), Tail(Snd(Val(Ide "x"))))))
            ) 
          )));;



typeinf e12;;


typeinf ( 
  Rec(Ide "k",
      Fun(Ide "x",    
          Ifthenelse(
            Less(
              Fst(Val(Ide "x")), 
              Head(Snd(Val(Ide "x")))),
            
            Cons(
              Fst(Val(Ide "x")), 
              Snd(Val(Ide "x"))),
            Cons(
              Head(Snd(Val(Ide "x"))),
              Appl(
                Val(Ide "k"), 
                Epair(Fst(Val(Ide "x")), Tail(Snd(Val(Ide "x")))))
          )
         ))));; 


typeinf ( 
  Rec(Ide "k",
      Fun(Ide "x",    
          Appl(
            Val(Ide "k"), 
            Snd(Val(Ide "x")))
         )
     ));; 

typeinf (
  Rec(Ide "y",
      Fun(Ide "x",
          Appl(
            Val(Ide "y"),
            Val(Ide "x")))));;


(* 1 *)
sem(
  Cons(
    Val(Ide "y"),
    Empty))
(bind(emptyenv,Ide "y",Pair(Int 1,Char 'a')));;
(* 2 *)
sem(Sum(Val(Ide "x"),Val(Ide "y"))) (bind(bind(emptyenv,Ide "x",Int 2),Ide "y",Int 5));;
(* 3 *)
sem(Ifthenelse(Eq(Eint 3,Val(Ide "x")),True,False)) (bind(emptyenv,Ide "x",Int 3));;
(* 4 *)
sem(Ifthenelse(Eq(Cons(Val(Ide "x"),Cons(Val(Ide "y"),Empty)),Cons(Val(Ide "z"),Cons(Echar 'c',Empty))),
               Head(Tail((Cons(Val(Ide "w"),Cons(Echar 'b',Empty))))),Fst(Epair(Snd(Epair(Val(Ide "y"),Echar 'c')),True))))
  (bind(bind(bind(bind(emptyenv,Ide "w", Char 'r'),Ide "z",Char 'p'),Ide "y",Char 'q'),Ide "x",Char 's'));;
(* 5 *)
sem(Snd(Head(Cons(Epair(Echar 'c',Eq(Eint 2,Eint 4)),Cons(Epair(Val(Ide "y"),Less(Val(Ide "x"),Eint 4)),Empty)))))
  (bind(bind(emptyenv,Ide "y",Char 'r'),Ide "x",Int 2));;
(* 6 *)
sem(Epair(Appl(Fun(Ide "x", Val (Ide "x")),
    Snd(Epair(Sum(Val(Ide "y"),Val(Ide "x")),Times(Head(Cons(Eint 1,Cons(Val(Ide "x"),Empty))),
    Head(Fst(Epair(Cons(Eint 2,Cons(Sum(Eint 1,Eint 0),Empty)),Not(Or(True,And(Val(Ide "z"),False)))))))))),
             Ifthenelse(Not(Not(Not(Ifthenelse(Eq(True,True),Less(Eint 1,Eint 0),And(Or(True,True),False))))),
             Not(And(True,True)),And(False,True)))) (bind(bind(bind(emptyenv,Ide "y",Int 10),Ide "z",Bool false),Ide "x",Int 5));;
(* 7 *)
sem(Epair(Sum(Val(Ide "x"),Head(Tail(Cons(Eint 5,Cons(Eint 4,Empty))))),
        Ifthenelse(Not(Eq(Less(Sum(Times(Eint 1,Eint 0),Eint 2),Eint 0),Less(Diff(Fst(Val(Ide "y")),Eint 4),Eint 1))),
             Not(Less(Head(Cons(Eint 2,Cons(Eint 3,Empty))),Eint 3)),
		   And(True,Less(Head(Tail(Cons(Eint 2,Cons(Eint 3,Empty)))),Eint 4)))))
  (bind(bind(emptyenv,Ide "y",Pair(Int 1,Char 'a')),Ide "x",Int 2));;
(* 8 *)
sem(Sum(Appl(Fun(Ide "x",Val(Ide "x")),Val(Ide "x")),Appl(Fun(Ide "y",Val(Ide "y")),Val(Ide "y"))))
  (bind(bind(emptyenv,Ide "y",Int 2),Ide "x",Int 5));;
(* 9 *)
sem(Appl(Rec(Ide "rec_fun",Fun(Ide "x",Ifthenelse(Eq(Val(Ide "x"),Eint 0),True,
						  Appl(Val(Ide "rec_fun"), Diff(Val(Ide "x"), Eint 1))))),Val(Ide "x")))
  (bind(bind(emptyenv,Ide "x",Int 4),Ide "guard",Bool true));;
(* 10 *)
sem(Epair(
  Eq(Appl(Fun(Ide "x",Cons(Val(Ide "x"),Empty)),Val(Ide "x")),Cons(Eint 2,Empty)),
  Eq(Appl(Fun(Ide "x",Cons(Val(Ide "x"),Empty)),Val(Ide "x")),Cons(Eint 4,Empty))) )
  (bind(emptyenv,Ide "x",Int 4));;
(* 11 *)
sem(Fun(Ide "x",Times(Sum(Val(Ide "x"),Val(Ide "y")),Val(Ide "z"))))
  (bind(bind(bind(emptyenv,Ide "z",Int 1),Ide "y",Int 1),Ide "x",Int 1));;
(* 12 *)
sem(
  Cons(Epair(Val(Ide "x"),Eint 3),Cons(Epair(Sum(Val(Ide "x"),Eint 1),Eint 4),Cons(Epair(Diff(Val(Ide "y"),Val(Ide "x")),Eint 0),Empty))))
  (bind(bind(emptyenv,Ide "y",Int 4),Ide "x",Int 5));;
(* 13   questo non funziona*)
sem(Sum(Appl(Fun(Ide "y",Fst(Val(Ide "y"))),Val(Ide "y")),Appl(Fun(Ide "x",Val(Ide "x")),Val(Ide "x"))))
  (bind(bind(emptyenv,Ide "y",Pair(Int 3,Char 'f')),Ide "x",Int 1));;
(* 14 *)
sem( Cons(Appl(Fun(Ide "z",Sum(Val(Ide "z"),Val(Ide "x"))),Val(Ide "z")),
     Cons(Ifthenelse(Eq(Val(Ide "x"),Eint 1),Appl(Fun(Ide "y",Times(Val(Ide "y"),Eint 3)),Val(Ide "y")),Val(Ide "z")),Empty)))
  (bind(bind(bind(emptyenv,Ide "z",Int 100),Ide "y",Int 2),Ide "x",Int 1));;
(* 15 *)
sem(Appl(Fun(Ide "z",
     Ifthenelse(Eq(Val(Ide "z"),Eint 100),
		Times(Appl(Fun(Ide "z",Times(Val(Ide "z"),Eint 2)),Val(Ide "z")),Appl(Fun(Ide "z",Sum(Val(Ide "z"),Eint 1)),Val(Ide "z"))),
		Eint 2)),Val(Ide "z"))) (bind(emptyenv,Ide "z",Int 100));;
(* 16 *)
sem(Appl(Rec(Ide "recoursive",Fun(Ide "x",
 Ifthenelse(Eq(Val(Ide "x"),Eint 30),Diff(Eint 50,Val(Ide "x")),Appl(Val(Ide "recoursive"),Diff(Val(Ide "x"),Eint 1))))),Val(Ide "x")))
  (bind(emptyenv,Ide "x",Int 30));;
(* 17 *)
sem(Cons(Appl(Fun(Ide "x",Sum(Val(Ide "x"),Eint 1)),Val(Ide "x")),
	 Cons(Appl(Fun(Ide "y",Times(Val(Ide "y"),Eint 2)),Val(Ide "y")),
	      Cons(Appl(Fun(Ide "z",Ifthenelse(Eq(Val(Ide "z"),True),Diff(Val(Ide "y"),Val(Ide "x")),Eint 0)),Val(Ide "z")) , Empty))))
  (bind(bind(bind(emptyenv,Ide "z",Bool true),Ide "y",Int 5),Ide "x",Int 2));;
(* 18 *)
sem(Epair(Appl(Fun(Ide "x",Cons(Sum(Val(Ide "x"),Eint 2),Empty)),Val(Ide "x")),
	  Cons(Appl(Fun(Ide "y",Times(Val(Ide "y"),Eint 5)),Val(Ide "y")),Empty)))
  (bind(bind(emptyenv,Ide "y",Int 12),Ide "x",Int 8));;
(* 19 *)
sem(Epair(Epair(Head(Cons(Appl(Fun(Ide "x",Sum(Val(Ide "x"),Eint 12)),Val(Ide "x")),Empty)),Sum(Val(Ide "x"),Val(Ide "y"))),
	  Epair(Tail(Cons(Eint 3,Cons(Eint 2,Cons(Appl(Fun(Ide "x",Sum(Val(Ide "x"),Eint 12)),Val(Ide "x")),Empty)))),
		Diff(Val(Ide "x"),Val(Ide "y")))))
  (bind(bind(emptyenv,Ide "y",Int 4),Ide "x",Int 2));;
(* 20 *)
sem(Cons(Appl(Fun(Ide "x",Sum(Val(Ide "x"),Eint 4)),Val(Ide "x")),Cons(Appl(Fun(Ide "y",Sum(Val(Ide "y"),Eint 1)),Val(Ide "y")),
    Cons(Appl(Fun(Ide "z",Sum(Val(Ide "z"),Eint 12)),Val(Ide "z")),Empty))))
  (bind(bind(bind(emptyenv,Ide "z",Int 1),Ide "y",Int 1),Ide "x",Int 1));;
