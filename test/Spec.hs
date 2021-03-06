module Main where

import Test.HUnit
--import Test.Framework as TF (testGroup, Test)
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Lib


main = defaultMain tests

tests :: [TF.Test]
tests
 =  [ testCase "1+1=2" (1+1 @?= 2) ]
