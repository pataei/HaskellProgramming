module Ch7 where

tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd $ divMod (fst (divMod x 10)) 10

hunsDigit' :: Integral a => a -> a
hunsDigit' x = snd $ divMod (fst (divMod x 100)) 100

foldBool :: a -> a -> Bool -> a
foldBool x y b 
  | b==False = x
  | b==True = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b = case b of 
                    False -> x
                    True -> y

g :: (a -> b) -> (a,c) -> (b,c)
g f (a,c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show 

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' x = (read ((show x) :: String))

-- here's how you call it:
-- (roundTrip'' 435) :: Int
-- roundTrip'' "lsdkf" :: String
