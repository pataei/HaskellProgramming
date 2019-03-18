module Ch10 where

-- why doesn't this work:
-- foldl ((++) . show) "" [1..3]
-- b ~ String
-- t a ~ [Int]
-- f ~ Int -> String -> String

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) 
  (secondsToDiffTime 34123)), DbNumber 9001, DbString "Hello, world!",
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate ((DbDate d):ds) = d : filterDbDate ds
filterDbDate (_:ds) = filterDbDate ds
filterDbDate [] = []
-- db = filter (\x -> if )

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber ((DbNumber n):ds) = n : filterDbNumber ds
filterDbNumber (_:ds) = filterDbNumber ds
filterDbNumber [] = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr max minTime times
  where
    times = filterDbDate db
    minTime = UTCTime (fromGregorian 1000 1 1) (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 nums
  where
    nums = filterDbNumber db

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral s / fromIntegral (length nums)
  where 
    s = sumDb db
    nums = filterDbNumber db

fibs = 1 : scanl (+) 1 fibs

fac :: [Int]
fac = scanl (*) 1 [2..]

stops = "pdtkg"
vowels = "aeiou"

svs = [x:y:z:[] | x <- stops, y <- vowels, z <- stops]
svs_p = filter (\s -> if head s == 'p' then True else False) svs

nouns = [stops, stops]
verbs = [vowels, vowels]
nvn = [x++y++z | x <- nouns, y <- verbs, z <- nouns]

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\v b -> x == v || b) False
-- myElem x = any (\v -> x == v)

myReverse :: [a] -> [a]
myReverse = foldr (\x bs -> bs ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\v bs -> f v : bs ) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x bs -> if f x then x : bs else bs) [] 

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x bs -> f x ++ bs) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy =  undefined
-- myMaximumBy f = foldr () 

-- myMaximumBy (\_ _ -> GT) [1..10] ==> 1
-- myMaximumBy (\_ _ -> LT) [1..10] ==> 10
-- myMaximumBy compare [1..10] ==> 10

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined