module AllTests where

import           Block1.Task1
import           Block1.Task2
import           Block1.Task3
import           Block2.Task2
import           Block3.Task1
import           Block3.Task2
import           Block4.Task1
import           Block5.Task1

import           Test.Tasty
import           Test.Tasty.Hspec

allTestsTree :: IO TestTree
allTestsTree = testSpec "hw1-tests" allTests

allTests :: Spec
allTests = do
  describe "Block1.Task1" $ do
    it "isWeekend" $ do
      isWeekend Sunday `shouldBe` True
      isWeekend Monday `shouldBe` False
      isWeekend Tuesday `shouldBe` False
      isWeekend Wednesday `shouldBe` False
      isWeekend Thursday `shouldBe` False
      isWeekend Friday `shouldBe` False
      isWeekend Saturday `shouldBe` True
    it "nextDay" $ do
      nextDay Sunday `shouldBe` Monday
      nextDay Monday `shouldBe` Tuesday
      nextDay Tuesday `shouldBe` Wednesday
      nextDay Wednesday `shouldBe` Thursday
      nextDay Thursday `shouldBe` Friday
      nextDay Friday `shouldBe` Saturday
      nextDay Saturday `shouldBe` Sunday
    it "daysToParty" $ do
      daysToParty Monday `shouldBe` 4
      daysToParty Sunday `shouldBe` 5
      daysToParty Friday `shouldBe` 0
    it "afterDays" $ do
      afterDays Sunday 0 `shouldBe` Sunday
      afterDays Sunday 1 `shouldBe` Monday
      afterDays Sunday 2 `shouldBe` Tuesday
      afterDays Sunday 5 `shouldBe` Friday
  describe "Block1.Task2" $ do
    it "multiply" $ do
      Z `mul` Z `shouldBe` Z
      S (S Z) `mul` S (S Z) `shouldBe` S (S (S (S Z)))
    it "subtract" $ do
      Z `sub` S Z `shouldBe` Z
      S (S Z) `sub` S Z `shouldBe` S Z
    it "toInt" $ do
      toInt Z `shouldBe` 0
      toInt (S (S Z)) `shouldBe` 2
    it "fromInt" $ do
      fromInt 0 `shouldBe` Z
      fromInt 2 `shouldBe` (S (S Z))
    it "(==)" $ do
      Z == Z `shouldBe` True
      Z == S (S Z) `shouldBe` False
    it "compare" $ do
      S Z < Z `shouldBe` False
      Z <= S (S Z) `shouldBe` True
    it "isEven" $ do
      isEven Z `shouldBe` True
      isEven (S (S (S Z))) `shouldBe` False
    it "div" $ do
      Z `ndiv` S Z `shouldBe` Z
      S (S (S (S Z))) `ndiv` S (S Z) `shouldBe` S (S Z) 
    it "mod" $ do
      S (S (S (S Z))) `nmod` S (S (S Z)) `shouldBe` S Z
      S (S (S (S Z))) `nmod` S (S Z) `shouldBe` Z
  describe "Block1.Task3" $ do
    it "isEmpty" $ do
      isEmpty Leaf `shouldBe` True
      isEmpty (Node Leaf ['1'] Leaf) `shouldBe` False
    it "getSize" $ do
      getSize Leaf `shouldBe` 0
      getSize (Node (Node Leaf ['a'] Leaf) ['c'] Leaf) `shouldBe` 2
    it "find" $ do
      find "1" Leaf `shouldBe` False
      find 'a' (Node (Node Leaf ['a'] Leaf) ['c'] Leaf) `shouldBe` True
    it "insert" $ do
      insert "1" Leaf `shouldBe` Node Leaf ["1"] Leaf
      insert 'a' Leaf `shouldBe` Node {leftBranch = Leaf, joint = "a", rightBranch = Leaf}
    it "fromList" $ do
      fromList ['1'] `shouldBe` Node Leaf ['1'] Leaf
      fromList ['6', '9'] `shouldBe` Node (Node Leaf ['6'] Leaf) ['9'] Leaf
    it "delete" $ do
      delete "1337" Leaf `shouldBe` Leaf
      delete 'a' (Node (Node Leaf ['a'] Leaf) ['c'] Leaf) `shouldBe` Node Leaf "c" Leaf
  describe "Block2.Task2" $ do
    it "splitOn" $ do
      splitOn '/' "path/to/file" `shouldBe` getNonEmpty ["path", "to", "file"]
      splitOn 't' "path/to/file" `shouldBe` getNonEmpty ["pa", "h/", "o/file"]
    it "joinWith" $ do
      joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
      joinWith 't' ["pa", "h/", "o/file"] `shouldBe` "path/to/file"
  describe "Block3.Task1" $ do
    it "maybeConcat" $ do
      maybeConcat [Just ["69"], Nothing] `shouldBe` ["69"]
      maybeConcat [Just ["1", "3"], Nothing, Just ["3"], Nothing, Just ["7"]] `shouldBe` ["1", "3", "3", "7"]
    it "eitherConcat" $ do
      eitherConcat [Left ["3"], Right ["1","2","3"], Left ["5"], Right ["4","5"]] `shouldBe` (["3","5"],["1","2","3","4","5"])
      eitherConcat [Left ['a'], Right [], Left [], Right ['b']] `shouldBe` (['a'], ['b'])
  describe "Block3.Task2" $ do
    it "Semigroup NonEmpty" $ do
      ('a' :| ['b']) <> ('c' :| ['d']) `shouldBe` 'a' :| "bcd"
      ('1' :| ['1', '4']) <> ('8' :| ['8']) `shouldBe` '1' :| "1488"
    it "Semigroup Name" $ do
      Name "root" <> Name "server" `shouldBe` Name "root.server"
      Name "" <> Name "server" `shouldBe` Name "server" 
  describe "Block4.Task1" $ do
    it "stringSum \"  69  -42  \"" $
      stringSum "  69  -42  " `shouldBe` Just 27
    it "stringSum \" --1337 (⌒ ω ⌒)\"" $
      stringSum " --1337 (⌒ ω ⌒)" `shouldBe` Nothing
    it "stringSum \"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ ʕ•ᴥ•ʔ ლ(╹◡╹ლ) \"" $
      stringSum "(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ ʕ•ᴥ•ʔ ლ(╹◡╹ლ) " `shouldBe` Nothing
  describe "Block5.Task1" $ do
    it "eval success" $ do
      eval (Const 1337) `shouldBe` Right 1337
      eval (BinaryOp Sub (Const 77) (Const 8)) `shouldBe` Right 69
    it "eval failure" $ do
      eval (BinaryOp Pow (Const 12) (Const (-12))) `shouldBe` Left PowNegate
      eval (BinaryOp Div (Const 12) (Const 0)) `shouldBe` Left DivideByZero

