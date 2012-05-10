-- |Contains function `replace` that can be used to edit expressions.
module Q2.TH.Replace(Action(Replace, Continue, Abort), replace) where

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
    (>>=) (Replace x) f = let result = f x in
                          case result of
                            Replace _ -> result
                            Continue x' -> Replace x'
                            Abort -> Abort
    (>>=) (Continue x) f = f x
    (>>=) Abort _ = Abort

  -- |Derived from Monad instance.
  instance Functor Action where
    fmap = liftM

  -- |Derived from Monad instance.
  instance Applicative Action where
    pure = return
    (<*>) = ap

  -- |Applies single Action step to Maybe.
  apMaybe :: (a -> Action a) -> Maybe a -> Action (Maybe a)
  apMaybe _ Nothing = Continue Nothing
  apMaybe f (Just x) = do x' <- f x
                          Continue (Just x')

  -- |Applies single Action step to List.
  apList :: (a -> Action a) -> [a] -> Action [a]
  apList _ [] = Continue []
  apList f (x:xs) = do x' <- f x
                       xs' <- apList f xs
                       Continue (x':xs')

  -- |Converts Action to Maybe.
  toMaybe :: Action a -> Maybe a
  toMaybe (Replace x) = Just x
  toMaybe (Continue x) = Just x
  toMaybe Abort = Nothing

  -- |Recursively applies given function to all sub-expressions
  --  of a given expression, possibly rewriting them.
  replace :: (Exp -> Action Exp) -> Exp -> Maybe Exp
  replace r e = toMaybe (_replace r e)

  -- |replace() implementation.
  _replace :: (Exp -> Action Exp) -> Exp -> Action Exp
  _replace r e = let action = r e
                 in case action of
                   Replace _ -> action
                   Continue (VarE _) -> action
                   Continue (ConE _) -> action
                   Continue (LitE _) -> action
                   Continue (AppE f v) -> AppE <$> (rr f) <*> (rr v)
                   Continue (InfixE x op y) -> InfixE <$> (rr `apMaybe` x) <*> (rr op) <*> (rr `apMaybe` y)
                   Continue (UInfixE x op y) -> UInfixE <$> (rr x) <*> (rr op) <*> (rr y)
                   Continue (ParensE x) -> ParensE <$> (rr x)
                   Continue (LamE ps x) -> LamE ps <$> (rr x)
                   Continue (TupE xs) -> TupE <$> (rr `apList` xs)
                   Continue (UnboxedTupE xs) -> UnboxedTupE <$> (rr `apList` xs)
                   Continue (CondE x y z) -> CondE <$> (rr x) <*> (rr y) <*> (rr z)
                   Continue (LetE d x) -> LetE d <$> (rr x)
                   Continue (CaseE x ms) -> CaseE <$> (rr x) <*> (return ms)
                   Continue (DoE _) -> action
                   Continue (CompE _) -> action
                   Continue (ArithSeqE _) -> action
                   Continue (ListE xs) -> ListE <$> (rr `apList` xs)
                   Continue (SigE x t) -> SigE <$> (rr x) <*> (return t)
                   Continue (RecConE _ _) -> action
                   Continue (RecUpdE x fs) -> RecUpdE <$> (rr x) <*> (return fs)
                   Abort -> action
                 where rr = _replace r
