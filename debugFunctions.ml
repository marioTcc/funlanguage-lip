#use "typingg10.ml";;


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
typeinf (Eq(Echar 'c', Echar 'c'));;
typeinf (Eq(Echar 'c', Echar 'd'));;
typeinf (Eq(Echar 'c', True));;
typeinf (Eq(Echar 'c', Eint 2));;
typeinf (Eq(True, Eint 1));; 
typeinf (Eq(False, Eint 2));;
(* Assert per Epair *)
typeinf (Epair(Eint 2, True));;
typeinf (Epair(True,False));;
typeinf (Epair(Eint 2, Eint 3));;
typeinf (Epair(Epair (Eint 2, And(True,False)),True));;
(* Assert per Fst *)
typeinf (Sum(Fst(Epair(Eint 2,True)),Eint 4));;
typeinf (Sum(Fst(Epair(True,False)),Eint 5));;
(* Assert per Snd *)
typeinf (Sum(Snd(Epair(True,Eint 2)),Eint 5));;

(* TODO testare tutto il resto *)
