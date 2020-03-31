module Main where

import           Test.Tasty
import           AllTests (allTestsTree)

main :: IO ()
main =
  allTestsTree >>= \unitTests ->
    let allTests = testGroup "hw1-second-chance-tests" [unitTests]
     in defaultMain allTests
