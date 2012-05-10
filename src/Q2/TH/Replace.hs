module Q2.TH.Replace(replace) where

  import Control.Applicative
  import Control.Monad(ap, liftM)
  import Language.Haskell.TH

  data Action a = Replace a
                | Continue a
                | Abort

  instance Monad Action where
    return = Continue
    (>>=) (Replace x) f = let result = f x in
                          case result of
                            Replace _ -> result
                            Continue x' -> Replace x'
                            Abort -> Abort
    (>>=) (Continue x) f = f x
    (>>=) Abort _ = Abort

  instance Functor Action where
    fmap = liftM

  instance Applicative Action where
    pure = return
    (<*>) = ap

  apMaybe :: (a -> Action a) -> Maybe a -> Action (Maybe a)
  apMaybe _ Nothing = Continue Nothing
  apMaybe f (Just x) = do x' <- f x
                          Continue (Just x')

  apList :: (a -> Action a) -> [a] -> Action [a]
  apList _ [] = Continue []
  apList f (x:xs) = do x' <- f x
                       xs' <- apList f xs
                       Continue (x':xs')

  toMaybe :: Action a -> Maybe a
  toMaybe (Replace x) = Just x
  toMaybe (Continue x) = Just x
  toMaybe Abort = Nothing

  -- Executes given function over all sub-expressions,
  -- possibly rewriting them in process.
  replace :: (Exp -> Action Exp) -> Exp -> Maybe Exp
  replace r e = toMaybe (_replace r e)

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
