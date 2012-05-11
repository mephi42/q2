module Main where

  import Control.Monad
  import Q2.TH.ReplaceUtils
  import System.Exit
  import Test.QuickCheck.Test

  [d|

    -- |1 + 2 == 3, isn't it?
    onePlusTwo :: Int
    onePlusTwo = 1 + 2

    |] >>= (return . incLit)

  checkOnePlusTwo :: IO Result
  checkOnePlusTwo = quickCheckResult ( onePlusTwo == 5 )

  main :: IO ()
  main = do onePlusTwoR <- checkOnePlusTwo
            unless (isSuccess onePlusTwoR) exitFailure
