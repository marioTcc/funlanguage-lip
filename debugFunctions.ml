#use "typingg10.ml";;

(* Vari tests *)
let giuste = 
  [Eint 2, TInt; 
   Echar 'c', TChar; 
   True, TBool; 
   False, TBool; 
   Empty, TVar "_";
   Sum(Eint 2, Eint 3),TInt;
   Diff(Eint 2, Eint 3), TInt;
   Times(Eint 2, Eint 3), TInt; 
   And(True, False), TBool;
   Or(False, True), TBool; 
   Not(True), TBool;
   Less(Eint 2, Eint 3), TBool; 
   Less(Sum(Eint 2, Eint 3), Eint 3), TBool; 
   Less(Diff(Eint 2, Eint 3), Eint 6), TBool;
   Eq(Eint 2, Eint 4), TBool; 
   Eq(Eint 3, Eint 3), TBool; 
   Eq(Echar 'c', Echar 'd'), TBool; 
   Eq(True, False), TBool;
   Eq(Epair(Eint 2, Echar 'c'), Epair(Eint 3214, Echar 'd')), TBool;
   Eq(Cons(Eint 2, Empty), Cons(Eint 3, Empty)), TBool;
   Eq(Let(Ide "x", Echar 'c', Val (Ide "x")), Echar 'c'), TBool; 
   Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Eint 3), TBool;
   Eq(Ifthenelse(True, Eint 2, Eint 3), Ifthenelse(False, Eint 3, Eint 4)), TBool;
   Cons(Eint 3, Empty), TList[TInt]; 
   Cons(Echar 'c', Empty), TList[TChar]; 
   Cons(True, Empty), TList[TBool]; 
   Cons( Eint 2, Cons(Eint 3, Empty)), TList[TInt];
   Cons(True, Cons(False, Empty)), TList[TBool]; Cons(Echar 'c', Cons(Echar 'd', Empty)), TList[TChar];
   Cons(Cons(Eint 2, Empty), Cons(Cons(Eint 3, Empty), Empty)), TList[TList[TInt]];
   Cons(Cons(Let(Ide "x", Echar 'c', Val (Ide "x")), Empty), Cons(Empty, Empty)), TList[TList[TChar]];
   Cons(Cons(Fun(Ide "x", Val(Ide "x")), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty)), TList[TFun(TInt, TInt)];
   Head(Empty), TVar "_"; 
   Head(Cons(Eint 3, Empty)), TInt; 
   Head(Cons(Echar 'c', Empty)), TChar;
   Head(Cons(True, Empty)), TBool; 
   Head(Cons( Eint 2, Cons(Eint 3, Empty))), TInt;
   Head(Cons(True, Cons(False, Empty))), TBool; 
   Head(Cons(Echar 'c', Cons(Echar 'd', Empty))), TChar;
   Tail(Empty), TList[TVar "_"]; 
   Tail(Cons(Eint 3, Empty)), TList[TInt]; 
   Tail(Cons(Echar 'c', Empty)), TList[TChar];
   Tail(Cons(True, Empty)), TList[TBool]; 
   Tail(Cons( Eint 2, Cons(Eint 3, Empty))), TList[TInt];
   Tail(Cons(True, Cons(False, Empty))),TList[TBool];
   Tail(Cons(Echar 'c', Cons(Echar 'd', Empty))), TList[TChar];
   Epair(Eint 2, Eint 3), TPair(TInt, TInt); 
   Epair(Echar 'c', Echar 'd'), TPair(TChar, TChar); 
   Epair(True, False), TPair(TBool, TBool);
   Epair(Sum(Eint 2, Eint 3), Or(True, False)), TPair(TInt, TBool); 
   Fst(Epair(Eint 2, Eint 3)), TInt; 
   Fst(Epair(Echar 'c', Echar 'd')), TChar; 
   Fst(Epair(True, False)), TBool; 
   Fst(Epair(Sum(Eint 2, Eint 3), Or(True, False))), TInt;
   Snd(Epair(Eint 2, Eint 3)), TInt; 
   Snd(Epair(Echar 'c', Echar 'd')),TChar; 
   Snd(Epair(True, False)),TBool; 
   Snd(Epair(Sum(Eint 2, Eint 3), Or(True, False))), TBool;
   Ifthenelse(True, Sum(Eint 1, Eint 2), Diff(Eint 11, Eint 3)), TInt;
   Ifthenelse(False, True, False), TBool;
   Ifthenelse(True, Echar 'c', Echar 'd'), TChar;
   Let(Ide "x", Eint 2, Sum(Val(Ide "x"),Eint 3)), TInt;
   Let(Ide "x", True, And(Val(Ide "x"), False)), TBool;
   Let(Ide "x", Echar 'c', Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TChar;
   Let(Ide "x", Fun(Ide "y", Sum(Val(Ide "y"), Eint 3)), Appl(Val (Ide "x"), Eint 2)), TInt;
   Fun(Ide "x", Val (Ide "x")), TFun(TVar "_", TVar "_");
   Fun(Ide "x", Sum(Val(Ide "x"), Eint 2)), TFun(TInt,TInt);
   Fun(Ide "x", And(Val(Ide "x"), True)), TFun(TBool,TBool);
   Fun(Ide "x", Ifthenelse(Eq(Val(Ide "x"), Echar 'c'), Echar 'd', Echar 'f')), TFun(TChar,TChar);
   Fun(Ide "x", Appl(Fun(Ide "y", Val (Ide "y")), Val( Ide "x"))), TFun(TVar "_",TVar "_");
   Appl(Fun(Ide "x", Val (Ide "x")), Eint 2), TInt;
   Appl(Fun(Ide "x", Val (Ide "x")), True), TBool;
   Appl(Fun(Ide "x", Val (Ide "x")), Echar 'c'), TChar;
   Rec(Ide "y", (Fun(Ide "x", Sum(Val (Ide "x"), Appl(Val (Ide "y"), Diff(Val (Ide "x"), Eint 1)))))), TInt;
   Rec(Ide "y", (Fun(Ide "x", And(Val (Ide "x"), Appl(Val (Ide "y"), False))))), TBool
];;
	
let sbagliate = [
Eq(Epair(Eint 2, Echar 'c'),Epair(Echar 'd', Eint 3));
  Eq(Cons(Eint 2, Empty), Cons(Echar 'c', Empty));
  Eq(Ifthenelse(True, Echar 'c', Echar 'd'), Ifthenelse(False, Eint 3, Eint 4));
  Eq(Let(Ide "x", Eint 3, Sum(Val(Ide "x"), Eint 3)), Echar 'c');
Cons(Cons(Fun(Ide "x", And(Val(Ide "x"), True)), Empty), Cons(Cons(Fun(Ide "x", Sum(Val(Ide"x"), Eint 3)), Empty), Empty))];;


(* !!! quello che segue è ancora da verificare !!! *)

(*---------------------------------------------------------------------------
Test per typeinf
-------------------------------------------------------------------------------*)
let testGiuste l = 
  let testaGiustaExpr expr expected index = match typeinf expr, expected with
      (TVar n1, TVar n2) -> index + 1
    | (TFun(TVar n1,TVar n2), TFun(TVar n3,TVar n4)) -> index +1
    | (TPair(TVar n1, TVar n2),TPair(TVar n3, TVar n4)) -> index +1
    | (TList [TVar n1], TList [TVar n2]) -> index +1
    | (TInt,TInt) | (TBool, TBool) | (TChar,TChar) -> index +1
    | _ -> failwith""
  in let check = List.fold_left (fun acc x -> try testaGiustaExpr (fst x) (snd x) acc with _ -> acc)
       0 l
  in if check = List.length l then TVar "Tutto OK" else TVar ("Errore in "^ string_of_int check);;
testGiuste giuste;;





let testSbagliate l = let resList = List.fold_right 
  (fun x acc -> 
     (try typeinf x with _ -> TVar "OK")::acc     ) l []
in List.fold_right 
     (fun x acc -> if acc = true then
        match x with TVar "OK" -> true
          | _ -> false
      else false) resList true;;
testSbagliate sbagliate;; (* Deve dare true, se tutto va bene, false se qualcosa che doveva rompersi non si è rotto *)
