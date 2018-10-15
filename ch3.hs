module Ch3 where

-- area d = pi * (r * r)
-- r = d / 2
-- error: variable not in scope

area' d = pi * (r * r)
  where r = d / 2

ega :: [Char] -> [Char] -> [Char]
ega = (++)

egb d t s = drop d $ take t s

egc d s = drop d s

thirdLetterString s = egb 2 3 s

thirdLetter s = s !! 2

letterIndex i s = s !! i

reverse' :: String -> String
reverse' s = concat [awesome, is, " ", curry]
  where curry   = take 5 s 
        is      = egb 5 8 s
        awesome = drop 9 s

-- "curry is awesome"
-- "awesome is curry"