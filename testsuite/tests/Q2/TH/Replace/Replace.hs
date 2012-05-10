module Main where

  import Control.Monad
  import Q2.TH.ReplaceUtils
  import System.Exit
  import Test.QuickCheck.Test

  checkOnePlusTwo :: IO Result
  checkOnePlusTwo = quickCheckResult ($( [| 1 + 2 |] >>= (return . (mapLit (1+))) ) == 5)

  main :: IO ()
  main = do onePlusTwo <- checkOnePlusTwo
            unless (isSuccess onePlusTwo) exitFailure
