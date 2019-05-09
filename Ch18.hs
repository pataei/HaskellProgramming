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
  return _ = NopeDotJpg
  _ >>= _ = NopeDotJpg

data PhbtEither b a = L a 
                    | R b

instance Functor (PhbtEither b) where
  fmap f (L x) = L $ f x
  fmap _ (R x) = R x

instance Applicative (PhbtEither b) where
  pure x = L x
  L f <*> L x = L $ f x
  L f <*> R x = R x
  R f <*> R x = R x
  R f <*> L x = R f


instance Monad (PhbtEither b) where
  return = pure
  L x >>= f = f x
  R x >>= f = R x

data Identity a = Identity a
  deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity 
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x l) = Cons (f x) $ fmap f l

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f fs <*> Cons x xs = Cons (f x) (app (fmap f xs) (fs <*> xs))
  -- fs <*> xs = tolist [f x | f <- fs, x <- xs]

app :: List a -> List a -> List a
app xs Nil = xs
app Nil ys = ys
app (Cons x xs) ys = Cons x (app xs ys)

tolist :: [a] -> List a 
tolist [] = Nil
tolist (x:xs) = Cons x $ tolist xs

instance Monad List where
  return = pure
  Nil >>= f = Nil
  xs >>= f = lconcat $ fmap f xs

lconcat :: List (List a) -> List a
lconcat Nil = Nil
lconcat (Cons x xs) = app x (lconcat xs)

joinm :: Monad m => m (m a) -> m a
joinm ms = ms >>= id
  -- do s <- ms
  --          s
  -- do s <- ms
  --          s' <- s
  --          return s'

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f mx = mx >>= return . f
-- l1 f mx = mx >>= (\x -> return $ f x)

l2 :: Monad m  => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = mx >>= (\x -> my >>= (\y -> return $ f x y))

am :: Monad m => m a -> m (a -> b) -> m b
am ma f = f <*> ma 

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = do r <- f x
                  rs <- meh xs f
                  return (r:rs)

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
