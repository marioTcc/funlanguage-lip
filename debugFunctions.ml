#use "typing--gruppo--.ml";;


(* Test per inferenza dei tipi *)
let testaInferenza assertExp expected =
  (solveConstraints (snd (getConstraints assertExp))) = expected;;

 
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
(* TODO testare tutto il resto *)
