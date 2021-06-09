concrete LHask of L = open Prelude, FormalTwo in {


lincat
  Typ = TermPrec ;
  Exp = TermPrec ;
  Var = Str ;
  [Var] = Str ;
  Decl = Str ;

lin

--Tarr : Typ -> Typ -> Typ ;
  Tarr = infixr 1 "->" ; -- A -> Set
--Tnat : Typ ;
  Tnat = constant "nat" ;

--Evar : Var -> Exp ;
  Evar v = constant v ;
--Elam : Var -> Typ -> Exp -> Exp ;
  Elam v t e = mkPrec 0 ("\\" ++ parenth (v ++ ":" ++ usePrec 0 t) ++ "->" ++ usePrec 0 e) ;
--Eapp : Exp -> Exp -> Exp ;
  Eapp = infixl 2 "" ;

--Ezer    : Exp ;
  Ezer = constant "zero" ;
--Esuc    : Exp -> Exp ;
  Esuc e = mkPrec 4 ("suc" ++ usePrec 4 e) ;

--Enatrec : Var -> Var -> Exp -> Exp -> Exp ->  Exp ;
  Enatrec v1 v2 step base n = mkPrec 3 ("rec" ++ usePrec 4 n ++ "{ 0 =>" ++ usePrec 4 base ++ "| suc" ++ v1 ++  "with" ++ v2 ++ "=>" ++ usePrec 2 step ++ "}") ;

  -- why does this work with the last usePrec as 2

  -- alternatively for agda
  -- Enatrec : Exp -> Exp -> Exp -> Exp ;
  EnatrecLam step base n = mkPrec 3 ("Nrec" ++ usePrec 4 step ++ usePrec 4 base ++ usePrec 4 n) ;


  F = "f" ;
  G = "g" ;
  X = "x" ;
  Y = "y" ;
  Z = "z" ;
  Under = "_" ;

--IntV : Int -> Var ;
  IntV s = s.s ;

  --EXT

  Elams vs t e = mkPrec 0 ("\\" ++ parenth (vs ++ ":" ++ usePrec 0 t) ++ "->" ++ usePrec 0 e) ;

  BaseVar v1 v2 = v1 ++ v2 ;
  ConsVar v vs = v ++ vs ;

--DtypDef : Decl -> Decl -> Decl ;
  DtypDef d1 d2 = d1 ++ ";" ++ d2 ; -- \n fails, breaks the (l . p . l)

--Typ : Var -> Typ -> Decl ;
  Dtyp v t = v ++ ":" ++ t.s ;
--Def : Var -> Exp -> Decl ;
  Ddef v e = v ++ "=" ++ e.s ;

  Double = "double" ;
  Plus = "plus" ;
  Times = "times" ;
  Fact = "factorial" ;
  Acker = "ackerman" ;


  -- infixl 6 _+_ _-_
  -- infixl 7 _*_
  -- As a result, the precedence of -> is lower than any precedence you may declare with infixl and infixr.

  -- how to append fixity

  -- Double = "double" ;
  -- Plus = "+" ;
  -- Times = "*" ;
  -- Fact = "!" ;
  -- Acker = "ackerman" ;


  -- double' : ℕ → ℕ
  -- double' n = ℕrec (λ x y → suc (suc y)) 0 n
  -- p "\\ ( x : nat ) -> rec ( \\ ( _ y : nat ) -> suc suc y ) 0 x"
-- p "\\ ( x : nat ) -> rec ( \\ ( _ y : nat ) -> suc suc y ) 0 x"

  -- n1 +' n2 = ℕrec (λ _ x₁ → suc x₁) n1 n2
  -- _+'_ : ℕ → ℕ → ℕ
  -- n1 +' n2 = ℕrec (λ _ x₁ → suc x₁) n1 n2

  -- p "\\ ( x y : nat ) -> Nrec ( \\ ( _ z : nat ) -> suc z ) x y"

  -- p -cat=Decl "* : nat -> nat -> nat ; * = \\ ( x y : nat ) -> rec x { 0 => 0 | suc _ with z => + y z }"
  -- p "times : nat -> nat -> nat ; times = \\ ( x y : nat ) -> rec x { 0 => 0 | suc _ with z => plus y z }"

  -- _*'_ : ℕ → ℕ → ℕ
  -- _*'_ n1 n2 = ℕrec (λ n3 n4 → n1 +' n4) 0 n2

  -- all scratchwork

  -- Multiplication a binary function, taking two nats, x and y, and returning a nat.
  --   We proceed by cases on the first arguement, x.
  --   If x is zero, return zero, i.e. zero times anything is zero.
  --   -- If the first arguement is zero, return zero. -- anaphora
  --   If x is non-zero, return the sum of y and the product of x and y.
  --   -- If x is a successor,  -- alterantive to


  -- p "times : nat -> nat -> nat ; times = \\ ( x y : nat ) -> rec x { 0 => 0 | suc _ with z => plus y z }"

  -- p "times : nat -> nat -> nat ; times = \\ ( x y : nat ) -> rec x { 0 => 0 | suc _ with z => y }"

    -- fixity is fucked - only if 
    -- p "times : nat -> nat -> nat ; times = \\ ( x y : nat ) -> rec x { 0 => 0 | suc _ with z => ( ( y ) ( z ) ) }"

  -- p -cat=Exp "\\ ( x y : nat ) -> plus x y"


  -- p "plus : nat -> nat -> nat ; plus = \\ ( x y : nat ) -> rec x { 0 => y | suc _ with z => suc z }"

  -- p "double : nat -> nat ; double = \\ ( x : nat ) -> rec x { 0 => 0 | suc _ with y => suc suc y }"
  --   DtypDef (Dtyp Double (Tarr Tnat Tnat)) (Ddef Double (Elam X Tnat (Enatrec Under Y (Esuc (Esuc (Evar Y))) (Evar (IntV 0)) (Evar X))))

  --harper
  -- Enatrec e1 x y e2 e3 = "rec" ++ e1 ++ ++ "|" ++ "zero" ++ "->" ++ e2 ++ "|" ++ suc ++ x ++ "->" ++

  -- double' : ℕ → ℕ
  -- double' n = ℕrec (λ x y → suc (suc y)) 0 n
  -- p "\\ ( x : nat ) -> rec ( \\ ( _ y : nat ) -> suc suc y ) 0 x"

  -- n1 +' n2 = ℕrec (λ _ x₁ → suc x₁) n1 n2

  -- p "\\ ( f : nat ) -> \\ ( z : nat ) -> rec ( \\ ( x : nat ) -> \\ ( y : nat ) -> suc y ) f z"
  --   Elam F Tnat (Elam Z Tnat (Enatrec (Elam X Tnat (Elam Y Tnat (Esuc (Evar Y)))) (Evar F) (Evar Z)))

  -- p "\\ ( f z : nat ) -> rec ( \\ ( x y : nat ) -> suc y ) f z" | tt -- equivalently


}
