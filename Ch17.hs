module Ch17 where

-- import Prelude hiding (List)
import Control.Applicative
import Data.List (elemIndex)

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- added = fmap (+3) (lookup 3 $ zip [1..3] [4..6])
-- added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])
added = pure (+3) <*> (lookup 3 $ zip [1..3] [4..6])

y = lookup 3 $ zip [1..3] [4..6]
z = lookup 2 $ zip [1..3] [4..6]
-- tupled = (,) <$> y <*> z
-- tupled = ((,) <$> y) <*> z
-- tupled = fmap (,) y <*> z
-- tupled = pure (,) <*> y <*> z
tupled = liftA2 (,) y z


x = elemIndex 3 [1..5]
y' = elemIndex 4 [1..5]
max' = max
maxed = max' <$> x <*> y

xs = [1..3]
ys = [4..6]
x' = lookup 3 $ zip xs ys
y'' = lookup 2 $ zip xs ys
-- summed = pure sum <*> ((,) <$> x' <*> y'')
summed = sum <$> ((,) <$> x' <*> y'')

newtype Identity a = Identity a
  deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> (Identity x) = Identity $ f x

newtype Constant a b = Constant {getConstant :: a}
  deriving (Show, Eq, Ord)


instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant f <*> Constant x = Constant $ mappend f x


test1 = const <$> Just "hellow" <*> pure "World"
test2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "tireness" <*> pure [1..3]


-- using checkers to check the properties of monoid:
data Bull = Fools | Twoo 
  deriving (Show, Eq)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

-- we need to define EqProp for our custom datatype. 
-- this is straightforward bc checkers exports a func
-- called eq which reuses the pre-existing Eq instance 
-- for the datatype.
instance EqProp Bull where (=-=) = eq 

main :: IO ()
main = do quickBatch (semigroup Twoo)
          quickBatch (monoid Twoo)
          let xs = [("b", "lskdj", 1::Int)]
          quickBatch $ applicative xs


-- data List a = Nil | Cons a (List a)
--   deriving (Eq, Show)

-- instance Functor List where
--   fmap _ Nil = Nil
--   fmap f (Cons x xs) = Cons (f x) $ fmap f xs

-- instance Applicative List where
--   pure x = Cons x Nil
--   -- Nil <*> Nil = Nil
--   Nil <*> _ = Nil
--   Cons f fs <*> Nil = Nil
--   Cons f fs <*> es = append (fmap f es) $ fs <*> es

-- append :: List a -> List a -> List a
-- append Nil ys = ys
-- append (Cons x xs) ys = Cons x $ xs `append` ys

newtype ZipList' a = ZipList' ([a])
  deriving (Show, Eq)

-- take' :: Int -> List a -> List a
-- take' _ Nil = Nil
-- take' i (Cons x xs) = Cons x $ take' (i-1) xs

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where 
      xs' = let (ZipList' l) = xs in take 3000 l
      ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' []
  -- pure (Cons x xs) = ZipList' (Cons x xs)
  ZipList' f <*> (ZipList' xs) = ZipList' $ zipWith (\x y -> x y) f xs
-- f <*> xs

ziplistex = ZipList' [(+9),(*2),(+8)]
ziplistex' = ZipList' [1..3]


data Valid err a = Fail err
                 | Succ a 
  deriving (Eq, Show)

instance Functor (Valid e) where
  fmap f (Fail err) = Fail err
  fmap f (Succ a) = Succ $ f a

instance Monoid e => Applicative (Valid e) where
  pure _ = Fail mempty
  Fail f <*> Fail e = Fail $ mappend f e
  Fail f <*> Succ a = Fail mempty
  Succ f <*> Fail e = Fail mempty
  Succ f <*> Succ a = Succ $ f a
























