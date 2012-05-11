-- |Miscellaneous functions based on `replace`.
module Q2.TH.ReplaceUtils(incLit, mapLit) where

  import Data.Maybe
  import Language.Haskell.TH
  import Q2.TH.Replace

  -- |Applies given function to all integer literals.
  mapLit :: ExpReplaceable a => (Integer -> Integer) -> a -> a
  mapLit f = fromJust . expReplace (\s -> case s of
                                            LitE (IntegerL x) -> Replace (LitE (IntegerL (f x)))
                                            other -> Continue other)

  -- |Increments all integer literals.
  incLit :: ExpReplaceable a => a -> a
  incLit = mapLit (1+)
