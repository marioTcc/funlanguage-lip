#use "evalg10.ml";;
#use "typing--gruppo--.ml";;


(* Input: espressione
   Output: il tipo di exp se andata a buon fine, scoppia se errore *) 
(* Asserts per Sum *)
typeinf (Sum(Eint(2), True));; (* Errore *)
typeinf (Sum(Eint(2), Eint(3)));; (* TInt *)
typeinf (Sum(Eint(4),And(False,True)));; (* Errore *)
(* Asserts per Times *)
typeinf (Times(Eint(3),False));;
typeinf (Times(Eint(3), Eint(6)));;
(* Asserts per And *)
typeinf (And(True, False));;
typeinf (And(Eint 2, True));;
(* Asserts per Not *)
typeinf (Not(True));;
typeinf (Not(Eint 1));;
typeinf (Not(And(True,False)));;
typeinf (Not(And(Eint 2, True)));;
(* Asserts per Less *)
typeinf (Less(And(True,False),Eint 3));;
typeinf (Less(Sum(Eint 3, Eint 2),Times(Eint 2, Eint 1)));;
typeinf (Less(Sum(Eint 3, True),Times(True,False)));;
typeinf (And(Less(Eint 3, Eint 2),True));;
typeinf (And(Less(True, Eint 3),False));;
(* Assert per Eq *)
typeinf (Eq(Eint 2, Eint 3));;
typeinf (Eq(True, False));;
typeinf (Eq(True, Eint 1));;
typeinf (Eq(False, Eint 2));;
(* Assert per Pair *)
typeinf (Pair(Eint 2, True));;
typeinf (Pair(True,False));;
typeinf (Pair(Eint 2, Eint 3));;
typeinf (Pair(Pair (Eint 2, And(True,False)),True));;
(* Assert per Fst *)
typeinf (Sum(Fst(Pair(Eint 2,True)),Eint 4));;
typeinf (Sum(Fst(Pair(True,False)),Eint 5));;
(* Assert per Snd *)
typeinf (Sum(Snd(Pair(True,Eint 2)),Eint 5));;
(* Assert per Head *)
typeinf (Head Empty);;
typeinf (Head(Cons(Eint 1, Empty)));;
(* Assert per Tail *)
typeinf (Tail Empty);;
typeinf (Tail(Cons(Eint 1, Empty)));;
(* Assert generici *)
typeinf(Echar 'c');;
typeinf(Sum(Eint 2, Echar 'c'));;
typeinf(Eq(Echar 'c', Eint 2));;
typeinf(Eq(Echar 'c', Echar 'd'));;
typeinf(Eq(Echar 'c', Echar 'c'));;
(* Assert per liste *)
typeinf (Cons(Eint 1, Cons(Eint 2, Empty)));;
typeinf (Cons(Eint 1, Empty));;
typeinf Empty;;
typeinf (Eq( (Cons(Eint 1, Empty)), (Cons(Eint 1, Empty))));;
typeinf (Eq( (Cons(Eint 1, Empty)), (Cons(True, Empty))));;
typeinf (   Cons(  Eint 1,    (Cons(Eint 2, Empty))));;
(* TODO testare tutto il resto *)

(* Test per inferenza *)
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
