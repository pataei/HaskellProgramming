module Ch18 where

import Control.Monad (join, ap)

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

data Nope a = NopeDotJpg 

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = undefined
  (>>=) = undefined

data PhbtEither b a = Left a 
                    | Right b

instance Applicative (PhbtEither b) where
  pure x = Left x
  

instance Monad (PhbtEither b) where
  return = undefined
  (>>=) = undefined

data Identity a = Identity a
  deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity 
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = Identity $ f x

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x l) = Cons (f x) $ fmap f l

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = undefined
    -- fmap f x | f <- fs, x <- xs]

instance Monad List where
  return = pure
  xs >>= f = undefined

-- jm :: Monad m => m (m a) = m a
-- jm ms = undefined
