-- |Miscellaneous functions based on `replace`.
module Q2.TH.ReplaceUtils(mapLit) where

  import Data.Maybe
  import Language.Haskell.TH
  import Q2.TH.Replace

  -- |Applies given function to all integer literals.
  mapLit :: (Integer -> Integer) -> Exp -> Exp
  mapLit f = fromJust . replace (\s -> case s of
                                         LitE (IntegerL x) -> Replace (LitE (IntegerL (f x)))
                                         other -> Continue other)
