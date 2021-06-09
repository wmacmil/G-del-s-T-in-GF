resource FormalTwo = open Prelude in {

  -- to replace the old library Precedence

  -- param Con = CInfix | CPrefix | CPostfix | CNone ;

  oper
    TermPrec : Type = {s : Str ; p : Prec} ;
    -- TermPrecCon : Type = {s : Str ; p : Prec ; c : Con } ;

    mkPrec : Prec -> Str -> TermPrec = \p,s ->
      {s = s ; p = p } ;
      -- {s = s ; p = p ; c = CNone } ;

    top : TermPrec -> Str = usePrec 0 ;

    constant : Str -> TermPrec = mkPrec highest ;

    infixl : Prec -> Str -> (_,_ : TermPrec) -> TermPrec = \p,f,x,y ->
      mkPrec p (usePrec p x ++ f ++ usePrec (nextPrec p) y) ;
    infixr : Prec -> Str -> (_,_ : TermPrec) -> TermPrec = \p,f,x,y ->
      mkPrec p (usePrec (nextPrec p) x ++ f ++ usePrec p y) ;
    infixn : Prec -> Str -> (_,_ : TermPrec) -> TermPrec = \p,f,x,y ->
      mkPrec p (usePrec (nextPrec p) x ++ f ++ usePrec (nextPrec p) y) ;

    prefix : Prec -> Str -> TermPrec -> TermPrec = \p,f,x ->
      mkPrec p (f ++ usePrec p x) ;
    postfix : Prec -> Str -> TermPrec -> TermPrec = \p,f,x ->
      mkPrec p (usePrec p x ++ f) ;

-- auxiliaries, should not be needed so much

    usePrec : Prec -> TermPrec -> Str = \p,x ->
      case lessPrec x.p p of {
        True => parenth x.s ;
        False => parenthOpt x.s
      } ;

    parenth : Str -> Str = \s -> "(" ++ s ++ ")" ;
    parenthOpt : Str -> Str = \s -> variants {s ; "(" ++ s ++ ")"} ;
    -- possibly comment this out to change it

--.
-- low-level things: don't use

    Prec : PType = Predef.Ints 5 ;

    highest = 5 ;

    lessPrec : Prec -> Prec -> Bool = \p,q ->
      case <<p,q> : Prec * Prec> of {
        <3,5> | <2,5> | <4,5> => True ;
        <3,4> | <2,3> | <2,4> => True ;
        <1,1> | <1,0> | <0,0> => False ;
        <1,_> | <0,_>         => True ;
        _ => False
        } ;

    nextPrec : Prec -> Prec = \p -> case <p : Prec> of {
      5 => 5 ;
      n => Predef.plus n 1
      } ;

}
