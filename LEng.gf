concrete LEng of L = open Prelude, FormalTwo in {

lincat
  Typ = Str ;
  -- TermPrec ;
  Exp = Str ;
  Var = Str ;
  [Var] = Str ;
  Decl = Str ;

lin

--Tarr : Typ -> Typ -> Typ ;
  Tarr t1 t2 = t1 ++ "to" ++ t2 ;
  -- Tarr =  "->" ; -- A -> Set
  -- want the function to be included in the type "the function type from natural numbers s to natural numbers" the
--Tnat : Typ ;
  Tnat = "the natural numbers" ;

--Evar : Var -> Exp ;
  Evar v = v ;
--Elam : Var -> Typ -> Exp -> Exp ;
  Elam v t e = "function taking" ++ v ++ "in" ++ t ++ "to" ++ e ;
--Eapp : Exp -> Exp -> Exp ;
  -- Eapp e1 e2 = e1 ++ e2 ;
  Eapp e1 e2 = "apply" ++ e1 ++ "to" ++ e2 ;


--Ezer    : Exp ;
  Ezer = "zero" ;
--Esuc    : Exp -> Exp ;
  Esuc e = "the successor of" ++ e ;

--Enatrec : Var -> Var -> Exp -> Exp -> Exp ->  Exp ;
  Enatrec v1 v2 step base n = "the recursor over" ++ n ++ ". In the base case  we take 0 to" ++ base ++ ". In the case of a successor, we take" ++ v2 ++ "to" ++ step ++ "." ;

    -- Enatrec v1 v2 step base n = mkPrec 3 ("rec" ++ usePrec 4 n ++ "{ 0 =>" ++ usePrec 4 base ++ "| suc" ++ v1 ++  "with" ++ v2 ++ "=>" ++ usePrec 4 step ++ "}") ;


  -- define addtion by recursion on x, taking 0
  -- the first arguement [anaphoric]
  -- recurse over n, taking 0 to 0 and the successor of v2 to



  --alternatively for agda
  --Enatrec : Exp -> Exp -> Exp -> Exp ;
  -- Enatrec step base n = mkPrec 3 ("rec" ++ usePrec 4 step ++ usePrec 4 base ++ usePrec 4 n) ;


  F = "f" ;
  G = "g" ;
  X = "x" ;
  Y = "y" ;
  Z = "z" ;
  Under = "_" ;

--IntV : Int -> Var ;
  IntV s = s.s ;

  --EXT

  Elams vs t e = "function taking" ++ vs ++ "in" ++ t ++ "to" ++ e ;

  BaseVar v1 v2 = v1 ++ "and" ++ v2 ;
  ConsVar v vs = v ++ "," ++ vs ;

--TypDef : Decl -> Decl -> Decl ;
  DtypDef d1 d2 = d1 ++ "and" ++ d2 ; -- \n fails, breaks the (l . p . l)

--Typ : Var -> Typ -> Decl ;
  Dtyp v t = v ++ "has the type of" ++ t ;
  -- type of vs type taking naturals to naturals
  -- be in
  -- be a

--Def : Var -> Exp -> Decl ;
  Ddef v e = v ++ "is defined to be" ++ e ;

  Double = "double" ;
  Plus = "the sum of" ;
  Times = "the product of" ;

}
