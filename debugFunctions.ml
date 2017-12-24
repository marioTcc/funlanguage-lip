#use "typing--gruppo--.ml";;


(* Test per inferenza dei tipi *)
let testaInferenza assertExp expected =
  (solveConstraints (snd (getConstraints assertExp))) = expected;;

(* Input: espressione, valore atteso di valutazione (true andata a buon fine, false errore) *) 
(* Asserts per Sum *)
testaInferenza (Sum(Eint(2), True)) false;;
testaInferenza (Sum(Eint(2), Eint(3))) true;;
testaInferenza (Sum(Eint(4),And(False,True))) false;;
(* Asserts per Times *)
testaInferenza (Times(Eint(3),False)) false;;
testaInferenza (Times(Eint(3), Eint(6))) true;;
(* Asserts per And *)
testaInferenza (And(True, False)) true;;
testaInferenza (And(Eint 2, True)) false;;
(* Asserts per Not *)
testaInferenza (Not(True)) true;;
testaInferenza (Not(Eint 1)) false;; (*[è sbagliato, perchè?] non è più sbagliata, perchè? TODO *)
testaInferenza (Not(And(True,False))) true;;
testaInferenza (Not(And(Eint 2, True))) false;;
(* Asserts per Less *)
testaInferenza (And(Less(Eint 3, Eint 2),True)) true;;
testaInferenza (And(Less(True, Eint 3),False)) false;;
(* TODO testare tutto il resto *)
