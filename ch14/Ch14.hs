module Ch14 where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is alwyas\
       \ greater than x" $ do 
      property $ \x -> x + 1 > (x :: Int)

-- sample (arbitrary :: Gen Int)

oneToThree :: Gen Int
oneToThree = elements [1..3]

-- sample oneToThree
-- sample' oneToThree

genBool :: Gen Bool 
genBool = choose (False, True)

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char 
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do 
  a <- arbitrary 
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary 
  c <- arbitrary
  return (a,b,c)

-- sample genTuple
-- sample (genTuple :: Gen (Int, Float))

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Right b, Left a]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary 
  frequency [ (1, return Nothing),
              (3, return (Just a))]

prop_add :: Int -> Bool
prop_add x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_add