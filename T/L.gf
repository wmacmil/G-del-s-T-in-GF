abstract L = {

flags startcat = Exp ;

cat
  Typ ;
  Exp ;
  Var ;
  [Var]{2} ; --Ext
  Decl ; --Ext

fun


  Tarr : Typ -> Typ -> Typ ;
  Tnat : Typ ;

  Evar : Var -> Exp ;
  Elam : Var -> Typ -> Exp -> Exp ;
  Eapp : Exp -> Exp -> Exp ;

  Ezer : Exp ;
  Esuc : Exp -> Exp ;

  EnatrecLam : Exp -> Exp -> Exp ->  Exp ;

  -- in this case, we internalize the lambda expression via bound variables given explicitly in the AST
  Enatrec : Var -> Var -> Exp -> Exp -> Exp ->  Exp ;

  F : Var ;
  G : Var ;
  X : Var ;
  Y : Var ;
  Z : Var ;
  Under : Var ;

  IntV : Int -> Var ;

-- extended

  Elams : [Var] -> Typ -> Exp -> Exp ;

  DtypDef : Decl -> Decl -> Decl ;

  Dtyp : Var -> Typ -> Decl ;
  Ddef : Var -> Exp -> Decl ;

  Double : Var ;
  Plus : Var ;
  Times : Var ;
  Fact : Var ;
  Acker : Var ;


  --
  EDouble : Exp -> Exp ;
  EPlus : Exp -> Exp -> Exp ;
  ETimes : Exp -> Exp -> Exp ;

  -- Plus : Exp -> Exp -> Exp -- Plus e1 e2 = "the sum of" ++ e1 ++ "and" ++ e2"

}
