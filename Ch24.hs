{-# LANGUAGE OverloadedStrings #-}
module Ch24 where

import Text.Trifecta

stop :: Parser a 
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneeof = one >> eof
onetwo = one >> char '2'
onetwo' = onetwo >> stop
onetwothree = string "123"

ex1 :: Parser Integer
ex1 = do {i <- integer; eof; return i}

testParser :: Show a => Parser a -> IO ()
testParser p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop==>"
  testParser (stop :: Parser Char)
  pNL "one==>"
  testParser one
  pNL "oneeof==>"
  testParser oneeof
  pNL "one'==>"
  testParser (one' :: Parser Char)
  pNL "onetwo==>"
  testParser onetwo
  pNL "onetwo'==>"
  testParser (onetwo' :: Parser Char)
  pNL "onetwothree==>"
  testParser onetwothree