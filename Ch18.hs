module Ch18 where

import Control.Monad (join)

bind :: Monad m => m a -> (a -> m b) -> m b
bind x f = join $ fmap f x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x 
     then [x*x,x*x]
     else []


data Sum a b = Fst a
             | Snd b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Fst a) = Fst a
  fmap f (Snd b) = Snd (f b)

instance Applicative (Sum a) where
  pure x = Snd x
  Fst f <*> Fst x = Fst x
  Snd f <*> Snd x = Snd $ f x
  Snd f <*> Fst x = Fst x
  Fst f <*> Snd x = Fst f

instance Monad (Sum a) where
  return = pure
  Fst x >>= f = Fst x
  Snd x >>= f = f x




