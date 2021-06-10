abstract L = {

flags startcat = Exp ;

cat
  Exp ;
  Var ;
  -- Decl ; --Ext
  Tele ;
  LTele ;
  [Var]{0} ;
  [Tele]{0} ;
  [LTele]{1} ;


fun
  -- previously types
  Earr : Exp -> Exp -> Exp ;
  Epi : [Tele] -> Exp -> Exp ;
  Eid : Exp -> Exp -> Exp -> Exp ;
  Eid2 : Exp -> Exp -> Exp ;
  Enat : Exp ;
  Euni : Exp ;

  Evar : Var -> Exp ;
  Elam : [LTele] -> Exp -> Exp ;
  Eapp : Exp -> Exp -> Exp ;

  Erefl : Exp ;
  -- Eidind : Exp -> Exp -> Exp -> Exp -> Exp -> Exp ;

  Ezer : Exp ;
  Esuc : Exp -> Exp ;
  EsucEta : Exp ; -- how does this effect the NL expression, as well as thinking how to compute between it and Esuc at PGF layer
  Enatind : Exp -> Exp -> Exp -> Exp -> Exp ;

  F : Var ;
  G : Var ;
  N : Var ;
  X : Var ;
  X' : Var ;
  Y : Var ;
  Y' : Var ;
  Z : Var ;
  Z' : Var ;
  Und : Var ;
  IntV : Int -> Var ;

  TeleC : Var -> [Var] -> Exp -> Tele ;
  -- TeleC : Exp -> Exp -> Tele ; -- ( x : Set ) -- ( y : x -> Set )" -- ( x : f y z )"
  -- LTeleC : Exp -> Exp -> LTele ;
  LTeleC : [Var] -> Exp -> LTele ;

  -- DtypDef : Decl -> Decl -> Decl ;
  -- Dtyp : Var -> Typ -> Decl ;
  -- Ddef : Var -> Exp -> Decl ;

  Double : Var ;
  Plus : Var ;
  -- Plus : Exp -> Exp -> Exp
  Times : Var ;
  Assoc : Var ;
  Ap : Var ;

  -- trying extended logical framework, type signatures should be inferrable from context

  EDouble : Exp -> Exp ;
  EPlus : Exp -> Exp -> Exp ;
  ETimes : Exp -> Exp -> Exp ;
  ECong : Exp -> Exp -> Exp ;

  -- think of Kant and intensional vs extensional : defining what associative is
  -- p "\\ ( x y z : nat ) -> natind ( \\ ( f : nat ) -> ( f + x + y == f + ( x + y ) ) ) refl ( \\ ( f : nat ) -> \\ ( g :  ( ( plus f ( plus y z ) ) == ( plus ( plus f y ) z ) ) ) -> cong suc g  ) x" 

}
