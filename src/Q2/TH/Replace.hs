-- |Contains type class `ExpReplaceable` that can be used to edit expressions.
module Q2.TH.Replace(Action(Replace, Continue, Abort), ExpReplaceable(expReplace)) where

  import Control.Applicative
  import Control.Monad(ap, liftM)
  import Language.Haskell.TH

  -- |Action to be carried out with sub-expression.
  data Action a =
                  -- |Continue using given sub-expression and do not visit its sub-expressions.
                  Replace a
                  -- |Continue using given sub-expression and visit its sub-expressions.
                | Continue a
                  -- |Do not continue.
                | Abort

  -- |Priorities when chaining: Abort > Replace > Continue.
  instance Monad Action where
    return = Continue
    (>>=) (Replace x) f   = let result = f x in
                            case result of
                              Replace _ -> result
                              Continue x' -> Replace x'
                              Abort -> Abort
    (>>=) (Continue x) f  = f x
    (>>=) Abort _         = Abort

  -- |Derived from Monad instance.
  instance Functor Action where
    fmap = liftM

  -- |Derived from Monad instance.
  instance Applicative Action where
    pure = return
    (<*>) = ap

  -- |Applies single Action step to Maybe.
  apMaybe :: (a -> Action a) -> Maybe a -> Action (Maybe a)
  apMaybe _ Nothing  = Continue Nothing
  apMaybe f (Just x) = do x' <- f x
                          Continue (Just x')

  -- |Applies single Action step to List.
  apList :: (a -> Action a) -> [a] -> Action [a]
  apList _ []     = Continue []
  apList f (x:xs) = do x' <- f x
                       xs' <- apList f xs
                       Continue (x':xs')

  -- |Type whose sub-expressions can be rewritten.
  class ExpReplaceable a where
    -- |Recursively applies given function to all sub-expressions, possibly rewriting them.
    expReplace :: (Exp -> Action Exp) -> a -> Maybe a
    expReplace r e = case (_expReplace r e) of
                       Replace x  -> Just x
                       Continue x -> Just x
                       Abort      -> Nothing

    -- | `expReplace` implementation
    _expReplace :: (Exp -> Action Exp) -> a -> Action a

  -- |Allows rewriting sub-expressions of a Maybe.
  instance (ExpReplaceable a) => ExpReplaceable (Maybe a) where
    _expReplace r m = (_expReplace r) `apMaybe` m

  -- |Allows rewriting sub-expressions of a list.
  instance (ExpReplaceable a) => ExpReplaceable [a] where
    _expReplace r xs = (_expReplace r) `apList` xs

  -- |Allows rewriting sub-expressions of an expression.
  instance ExpReplaceable Exp where
    _expReplace r e = let action = r e
                      in case action of
                          Replace _                 -> action
                          Continue (VarE _)         -> action
                          Continue (ConE _)         -> action
                          Continue (LitE _)         -> action
                          Continue (AppE f v)       -> AppE <$> (rr f) <*> (rr v)
                          Continue (InfixE x op y)  -> InfixE <$> (rr x) <*> (rr op) <*> (rr y)
                          Continue (UInfixE x op y) -> UInfixE <$> (rr x) <*> (rr op) <*> (rr y)
                          Continue (ParensE x)      -> ParensE <$> (rr x)
                          Continue (LamE ps x)      -> LamE ps <$> (rr x)
                          Continue (TupE xs)        -> TupE <$> (rr xs)
                          Continue (UnboxedTupE xs) -> UnboxedTupE <$> (rr xs)
                          Continue (CondE x y z)    -> CondE <$> (rr x) <*> (rr y) <*> (rr z)
                          Continue (LetE d x)       -> LetE d <$> (rr x)
                          Continue (CaseE x ms)     -> CaseE <$> (rr x) <*> (return ms)
                          Continue (DoE s)          -> DoE <$> (rr s)
                          Continue (CompE _)        -> action
                          Continue (ArithSeqE _)    -> action
                          Continue (ListE xs)       -> ListE <$> (rr xs)
                          Continue (SigE x t)       -> SigE <$> (rr x) <*> (return t)
                          Continue (RecConE _ _)    -> action
                          Continue (RecUpdE x fs)   -> RecUpdE <$> (rr x) <*> (return fs)
                          Abort -> action
                      where rr :: (ExpReplaceable a) => a -> Action a
                            rr = _expReplace r

  -- |Allows rewriting sub-expressions of a declaration.
  instance ExpReplaceable Dec where
    _expReplace r d = case d of
                        FunD n cs   -> FunD n <$> (rr cs)
                        ValD p b ds -> ValD p <$> (rr b) <*> (rr ds)
                        SigD n t    -> Continue (SigD n t)
                        -- XXX: do not consider other stuff for now, the above is enough for q2's purposes.
                        other       -> Continue other
                      where rr :: (ExpReplaceable a) => a -> Action a
                            rr = _expReplace r

  -- |Allows rewriting sub-expressions of a clause.
  instance ExpReplaceable Clause where
    _expReplace r (Clause p b ds) = Clause p <$> (rr b) <*> (rr ds)
                                    where rr :: (ExpReplaceable a) => a -> Action a
                                          rr = _expReplace r

  -- |Allows rewriting sub-expressions of a body.
  instance ExpReplaceable Body where
    _expReplace r b = case b of
                        GuardedB xs -> GuardedB <$> (rt `apList` xs)
                        NormalB e   -> NormalB <$> (rr e)
                      where rr :: (ExpReplaceable a) => a -> Action a
                            rr = _expReplace r
                            rt :: (Guard, Exp) -> Action (Guard, Exp)
                            rt (g, e) = do e' <- rr e
                                           return (g, e')

  -- |Allows rewriting sub-expressions of a statement.
  instance ExpReplaceable Stmt where
    _expReplace r s = case s of
                        BindS p e -> BindS p <$> (rr e)
                        LetS ds   -> LetS <$> (rr ds)
                        NoBindS e -> NoBindS <$> (rr e)
                        ParS sss  -> ParS <$> (rr sss)
                      where rr :: (ExpReplaceable a) => a -> Action a
                            rr = _expReplace r
