concrete LEng of L = open Prelude, FormalTwo in {

lincat
  Typ = Str ;
  -- TermPrec ;
  Exp = Str ;
  Var = Str ; 
  [Var] = Str ;
  Decl = Str ;
   -- =

  -- oper ExpVar : Type = { s : Str ; arityNum : _uknownParam_ } ;
  -- lincat Var = ExpVar ;
  -- lincat Exp = ExpVar ;
  -- Eapp e1 e2 = case e1.arityNum of { _ => _ }

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

-- --Enatrec : Var -> Var -> Exp -> Exp -> Exp ->  Exp ;
--   Enatrec v1 v2 step base n = "the recursor over" ++ n ++ ". In the base case, 0, we take" ++ base ++ ". In the case of a successor, we take some number" ++ v2 ++ "to" ++ step ++ "." ; --to its successor


-- --EnatrecLam : Exp -> Exp -> Exp ->  Exp ;
--   EnatrecLam base step n = _ ;

  Enatrec v1 v2 step base n = "We proceed by cases on the first arguement," ++ n ++ ". In the base case, if " ++ n ++ "is 0, we return" ++ base ++ ". In the case of" ++ n ++ "being a successor, we return" ++ step ++ "." ; --to its successor

  -- Multiplication a binary function, taking two nats, x and y, and returning a nat.
  -- We proceed by cases on the first arguement, x.
  -- If x is zero, return zero, i.e. zero times anything is zero.
  --   -- If the first arguement is zero, return zero. -- anaphora
  -- If x is non-zero, return the sum of y and the product of x and y.
  --   -- If x is a successor,  -- alterantive to


  -- suc suc == suc . suc == successor applied twice == apply the successor twice to some number

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
--TypDef : Var -> Typ -> Exp -> Decl ; for anaphoric, then unify via pgf
  DtypDef d1 d2 = d1 ++ "and" ++ d2 ; -- \n fails, breaks the (l . p . l)

  -- anaphoric


  -- a : b

  --   or

  -- a = b'

  --   or

  -- a = b'
  -- a : b'




--Typ : Var -> Typ -> Decl ;
  Dtyp v t = v ++ "is a term with" ++ t ++ "as its type" ;
  -- type of vs type taking naturals to naturals
  -- be in
  -- be a

--Def : Var -> Exp -> Decl ;
  Ddef v e = v ++ "is defined to be" ++ e ;

  Double = "double" ;
  Plus = "the sum" ;
  Times = "the product" ;


  -- EDouble : Exp -> Exp ;
  -- EPlus : Exp -> Exp -> Exp ;
  -- ETimes : Exp -> Exp -> Exp ;

}
