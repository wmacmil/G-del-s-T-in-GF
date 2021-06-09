resource FormalTwo = open Prelude in {

  -- to replace the old library Precedence

  oper
    TermPrec : Type = {s : Str ; p : Prec} ;

    mkPrec : Prec -> Str -> TermPrec = \p,s ->
      {s = s ; p = p} ;

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

    Prec : PType = Predef.Ints 9 ;

    highest = 9 ;

    lessPrec : Prec -> Prec -> Bool = \p,q ->
      case <<p,q> : Prec * Prec> of {
        <3,9> | <2,9> | <4,9> | <5,9> | <6,9> | <7,9> | <8,9> => True ;
        <3,8> | <2,8> | <4,8> | <5,8> | <6,8> | <7,8> => True ;
        <3,7> | <2,7> | <4,7> | <5,7> | <6,7> => True ;
        <3,6> | <2,6> | <4,6> | <5,6> => True ;
        <3,5> | <2,5> | <4,5> => True ;
        <3,4> | <2,3> | <2,4> => True ;
        <1,1> | <1,0> | <0,0> => False ;
        <1,_> | <0,_>         => True ;
        _ => False
      } ;

    nextPrec : Prec -> Prec = \p -> case <p : Prec> of {
      9 => 9 ;
      n => Predef.plus n 1
      } ;

    -- Prec : PType = Predef.Ints 5 ;

    -- highest = 5 ;

    -- lessPrec : Prec -> Prec -> Bool = \p,q ->
    --   case <<p,q> : Prec * Prec> of {
    --     <3,5> | <2,5> | <4,5> => True ;
    --     <3,4> | <2,3> | <2,4> => True ;
    --     <1,1> | <1,0> | <0,0> => False ;
    --     <1,_> | <0,_>         => True ;
    --     _ => False
    --     } ;

    -- nextPrec : Prec -> Prec = \p -> case <p : Prec> of {
    --   5 => 5 ;
    --   n => Predef.plus n 1
    --   } ;

}
