concrete LEng of L = open Prelude, FormalTwo in {

lincat
  Exp = Str ;
  Var = Str ;
  -- Decl = Str ;
  Tele = Str ;
  [Tele] = Str ;
  [Var] = Str ;

lin

--Earr : Typ -> Typ -> Typ ;
  Earr e1 e2 = e1 ++ "to" ++ e2 ;
--Epi : [Tele] -> Exp -> Exp ;
  Epi ts e = "for every" ++ ts ++ e ;
--Eid : Exp -> Exp -> Exp -> Exp ;
  Eid e1 e2 e3 = e2 ++ "is equal to" ++ e3 ++ "at type" ++ e1 ; --type theoretic reading
--Eid2 : Exp -> Exp -> Exp ;
  Eid2 e1 e2 = e1 ++ "is equal to" ++ e2 ;
  Enat = "the natural numbers" ;
  Euni = "type" ;


  Erefl = "reflexivity" ;

--Evar : Var -> Exp ;
  Evar v = v ;
--Elam : Tele -> Exp -> Exp ;
  -- Elam ts e = "function taking" ++ ts ++ "to" ++ e ; -- program
  Elam ts e = "suppose" ++ ts ++ "." ++ e ; --proof
--Eapp : Exp -> Exp -> Exp ;
  Eapp e1 e2 = "apply" ++ e1 ++ "to" ++ e2 ;

--Ezer    : Exp ;
  Ezer = "zero" ;
--Esuc    : Exp -> Exp ;
  Esuc e = "the successor of" ++ e ;
--Enatind : Exp -> Exp -> Exp -> Exp -> Exp ;
  Enatind motive base ind n = "We proceed by induction over" ++ n ++ ". We therefore wish to prove :" ++ motive ++ ". In the base case, suppose" ++ n ++ "equals zero. we know this by" ++ base ++ ". In the inductive case, suppose" ++ n ++ "is the successor. Then one has one has" ++ ind ;

    -- mkPrec 3 ("natind" ++ usePrec 4 motive ++ usePrec 4 base ++ usePrec 4 ind ++ usePrec 4 n) ;

  -- TeleC : Exp -> Exp -> Tele ; -- ( x : Set ) -- ( y : x -> Set )" -- ( x : f y z )"
  TeleC v vs e = v ++ "," ++ vs ++ "are in" ++ e ; -- is for single

  BaseTele = "" ;
  ConsTele x xs = x ++ xs ;

  BaseVar = "" ;
  ConsVar x xs = x ++ xs ;

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


}
