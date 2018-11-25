module Ch8 where

tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

digits :: Int -> [Int]
digits n = case n > 10 of 
  True -> digits (fst d) ++ [snd d]
  _    -> [n]
    where d = divMod n 10 


