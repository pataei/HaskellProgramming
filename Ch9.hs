module Ch9 where

import Data.Char

eft :: (Ord a, Enum a) => a -> a -> [a]
eft a a' 
  | compare a a' == LT = [a] ++ eft (succ a) a'
  | compare a a' == EQ = [a]
  | otherwise = []


eftBool :: Bool -> Bool -> [Bool]
eftBool = eft 
  -- | compare b b' == LT = [b] ++ eftBool (not b) b'
  -- | compare b b' == EQ = [b]
  -- | otherwise = []

-- [True .. False] ==> []
-- [False .. True] ==> [False,True]
-- [False .. False] ==> [False]
-- [True .. True] ==> [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft
-- eftOrd LT EQ = [LT, EQ]
-- eftOrd EQ GT = [EQ, GT]
-- eftOrd LT GT = [LT, EQ, GT]
-- eftOrd x y 
--   | x == y = [x]
--   | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt = eft
-- eftInt i j 
--   | i <= j = [i] ++ eftInt (i+1) j
--   | i == j = [i]
--   | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar = eft
-- eftChar c c' 
--   | compare c c' == LT = [c] ++ eftChar (succ c) c'
--   | compare c c' == EQ = [c]
--   | otherwise = []

myWords :: String -> [String]
myWords s 
  | s /= "" = takeWhile (/= ' ') s : myWords rest
  | otherwise = []
    where 
      rest = dropWhile (== ' ') $ dropWhile (/= ' ') s

breakString :: Char -> String -> [String]
breakString c s 
  | s /= "" = takeWhile (/= c) s : breakString c rest
  | otherwise = []
    where 
      rest = dropWhile (== c) $ dropWhile (/= c) s

myLines :: String -> [String]
myLines = breakString '\n'

filterArticles :: String -> [String]
filterArticles s = filter (not . flip elem ["the", "a"]) words
  where words = myWords s

myZip :: [a] -> [b] -> [(a,b)]
-- myZip (a:as) (b:bs) = (a,b) : myZip as bs
-- myZip [] _ = []
-- myZip _ [] = []
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs
myZipWith _ [] _ = []
myZipWith _ _ [] = []

cap :: String -> String
cap s = toUpper (head s) : tail s
-- cap (s:ss) = toUpper s : ss
-- cap [] = []

capFirst :: String -> Char
capFirst = toUpper . head
-- capFirst s = toUpper $ head s

-- doesn't work right!!
cipher :: Int -> String -> String
cipher i s = map chr code
  where 
    nums = map ord s
    nums' = map (\x -> x-96) nums
    code = map (\x -> (mod (x+i) 26) + 96) nums'


decipher :: Int -> String -> String
decipher i = cipher (-i) 