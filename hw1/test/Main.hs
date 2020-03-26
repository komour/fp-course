module Main where

import           Test.Tasty
import           AllTests (allTestsTree)

main :: IO ()
main =
  allTestsTree >>= \unitTests ->
    let allTests = testGroup "hw1-tests" [unitTests]
     in defaultMain allTests
