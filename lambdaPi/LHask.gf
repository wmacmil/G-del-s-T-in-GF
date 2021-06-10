concrete LHask of L = open Prelude, FormalTwo in {

lincat
  Exp = TermPrec ;
  Var = Str ;
  -- Decl = Str ;
  Tele = Str ;
  [Tele] = Str ;
  [Var] = Str ;
  LTele = Str ;
  [LTele] = Str ;

lin

--Earr : Typ -> Typ -> Typ ;
  Earr = infixr 1 "->" ; -- A -> Set
--Epi : [Tele] -> Exp -> Exp ;
  Epi ts e = mkPrec 1 (ts ++ "->" ++ usePrec 1 e) ;
--Eid : Exp -> Exp -> Exp -> Exp ;
  Eid e1 e2 e3 = mkPrec 3 (usePrec 4 e1 ++ usePrec 4 e2 ++ "==" ++ usePrec 3 e3) ;
--Eid2 : Exp -> Exp -> Exp ;
  Eid2 e1 e2 = mkPrec 3 (usePrec 4 e1 ++ "==" ++ usePrec 3 e2) ;
  -- why not just infixr instead of l?

  Enat = constant "nat" ;
  Euni = constant "Set" ;

  Erefl = constant "refl" ;

--Evar : Var -> Exp ;
  Evar v = constant v ;
--Elam : LTele -> Exp -> Exp ;
  Elam ts e = mkPrec 0 ("\\" ++ ts ++ "->" ++ top e) ;
--Eapp : Exp -> Exp -> Exp ;
  Eapp = infixl 8 "" ;

--Ezer    : Exp ;
  Ezer = constant "zero" ;
--Esuc    : Exp -> Exp ;
  -- Esuc e = mkPrec 4 ("suc" ++ usePrec 4 e) ;
  Esuc = prefix 8 "suc" ; -- mkPrec 4 ("suc" ++ usePrec 4 e) ;
  EsucEta = constant "suc" ;
--Enatind : Exp -> Exp -> Exp -> Exp -> Exp ;
  Enatind motive base ind n = mkPrec 3 ("natind" ++ usePrec 4 motive ++ usePrec 4 base ++ usePrec 4 ind ++ usePrec 4 n) ;

  -- TeleC : Exp -> Exp -> Tele ; -- ( x : Set ) -- ( y : x -> Set )" -- ( x : f y z )"
  TeleC v vs e = "(" ++ v ++ vs ++ ":" ++ top e ++ ")" ;

  BaseTele = "" ;
  ConsTele x xs = x ++ xs ;

  BaseVar = "" ;
  ConsVar x xs = x ++ xs ;

  BaseLTele x = x ;
  ConsLTele x xs = x ++ xs ;

--LTeleC : Exp -> Exp -> LTele ;
  LTeleC vs e2 = "(" ++ vs ++ ":" ++ top e2 ++ ")" ;


  Assoc = "assoc" ;
  Double = "double" ;

  Plus = "plus" ;
  Times = "times" ;
  Ap = "ap" ;

  --IntV : Int -> Var ;
  IntV s = s.s ;

  F = "f" ;
  G = "g" ;
  N = "n" ;
  X = "x" ;
  X' = "x'" ;
  Y = "y" ;
  Y' = "y'" ;
  Z = "z" ;
  Z' = "z'" ;
  Und = "_" ;

  -- p "( x y z : nat ) -> ( plus x ( plus y z ) ) -> ( plus ( plus x y ) z )"

  -- p "\\ ( x y z : nat ) -> natind ( \\ ( f : nat ) -> ( ( plus f ( plus y z ) ) == ( plus ( plus f y ) z ) ) ) refl ( \\ ( f : nat ) -> \\ ( g :  ( ( plus f ( plus y z ) ) == ( plus ( plus f y ) z ) ) ) -> ap suc g ) x" | tt

  -- p "( \\ ( f : nat ) -> \\ ( g : ( ( plus f ( plus y z ) ) == ( plus ( plus f y ) z ) ) -> ap suc g ) )"

  -- p "\\ ( x y z : nat ) -> natind ( \\ ( f : nat ) -> ( ( plus f ( plus y z ) ) == ( plus ( plus f y ) z ) ) ) refl ( \\ ( f : nat ) -> \\ ( g :  ( ( plus f ( plus y z ) ) == ( plus ( plus f y ) z ) ) ) -> ap suc g ) x"

  -- associativity-plus-ind : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
  -- associativity-plus-ind m n p = natind {λ n' → ((n' + n) + p) ≡ ((n' + (n + p)))} refl (λ n₁ x → ap suc x) m

-- --DtypDef : Decl -> Decl -> Decl ;
--   DtypDef d1 d2 = d1 ++ ";" ++ d2 ; -- \n fails, breaks the (l . p . l)
-- --Typ : Var -> Typ -> Decl ;
--   Dtyp v t = v ++ ":" ++ t.s ;
-- --Def : Var -> Exp -> Decl ;
--   Ddef v e = v ++ "=" ++ e.s ;

  -- infix 40 _==_ _<_ _>_ _≤_ _≥_
  -- infixl 60 _+_ _-_
  -- infixl 70 _*_
  -- infix  90 -_

  -- EDouble e = cupric 4 ("2*" ++ usePrec 4 e) ;
  EDouble = prefix 8 "2*" ; -- mkPrec 4 ("2*" ++ usePrec 4 e) ;
  EPlus = infixl 5 "+" ;
  ETimes = infixl 6 "*" ;
  -- what is the default
  ECong e1 e2 = mkPrec 4 ("cong" ++ usePrec 1 e1 ++ usePrec 1 e2) ;

  -- one can express the associativity property as a list, and therefore generalize it, but this knowledge kind of needs to be known via something like a free monoid construction

  -- the type inference can make a big difference in NL output
  -- p "\\ ( x y z : nat ) -> natind ( \\ ( x' : nat ) -> ( x' + y + z == x' + ( y + z ) ) ) refl ( \\ ( x' : nat ) -> \\ ( y' :  ( x' + y + z == x' + ( y + z ) ) ) -> cong suc y' ) x" 


  -- 1153 , now broken because its misinterpreting the tele, but this is actually consistent with cubicaltt, see lem5 in bool.ctt
  --anyways, i just made it a list of vars instead

}
