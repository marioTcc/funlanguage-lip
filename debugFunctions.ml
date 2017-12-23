#use "typing--gruppo--.ml";;

(* Test per inferenza dei tipi *)
let rec testaInferenza asserts =  match asserts with
    [] -> true
  | hd::tl -> if solveConstraints( snd( getConstraints (fst hd))) = snd hd then testaInferenza tl else false;;

let asserts = 
(* Asserts per Sum *)
[(Sum(Eint(2), True),false); (Sum(Eint(2), Eint(3)),true); (Sum(Eint(4),And(False,True)),false);
(* Asserts per Times *)
(Times(Eint(3),False),false); (Times(Eint(3), Eint(6)),true)
(* Asserts per Not NON FUNGE*)
(*(Not(Eint 3),false);(Not(True),true)*)];;


testaInferenza asserts;;
