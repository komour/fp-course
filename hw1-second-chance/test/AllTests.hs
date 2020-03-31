module AllTests where

import           Block6

import           Data.Char        (isUpper)
import           Test.Tasty
import           Test.Tasty.Hspec

allTestsTree :: IO TestTree
allTestsTree = testSpec "hw1-second-chance-tests" allTests

allTests :: Spec
allTests = describe "Block6-Task2" $ do
            it "eof" $ do
              runParser eof "" `shouldBe` Just ((), "")
              runParser eof "abc" `shouldBe` Nothing
            it "ok" $ do
              runParser ok "ABC" `shouldBe` Just ((), "ABC")
              runParser ok "" `shouldBe` Just ((), "")
            it "satisfy" $ do
              runParser (satisfy isUpper) "ABC" `shouldBe` Just ('A', "BC")
              runParser (satisfy isUpper) "abc" `shouldBe` Nothing
            it "element" $ do
              runParser (element 'x') "xyz" `shouldBe` Just ('x',"yz")
              runParser (element '\'') "\'" `shouldBe` Just ('\'', "")
              runParser (element 'a') "xyz" `shouldBe` Nothing
            it "stream" $ do
              runParser (stream "abc") "abcdef" `shouldBe` Just ("abc","def")
              runParser (stream "abc") "abxc" `shouldBe` Nothing

