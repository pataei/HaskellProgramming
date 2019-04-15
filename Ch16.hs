{-# LANGUAGE RankNTypes #-}
-- the flag is used for higher-kinded types in Nat
module Ch16 where

import Test.QuickCheck
import Test.QuickCheck.Function

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
-- f has kind: * -> *
-- fmap operator: <$> 
--   because at the end of the day fmap is a kind of func app.

-- laws:
-- identity
-- fmap id = id

-- composition
-- fmap (f . g) = fmap f . fmap g

-- g is a function
-- fmap f g = f . g

-- you can stack fmaps if you're stacking data constructors 
-- by composing them, i.e., fmap . fmap . fmap . ...

a :: [Int]
a = fmap (+1) $ read "[1]"

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Int -> Int
c = (*2) . (\x -> x - 2)

d :: Int -> String
d = fmap ((return '1' ++ ) . show) (\x -> [x, 1..3])

-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = fmap read $ ("123" ++) $ fmap show ioi
--     in fmap (*3) changed
-- the original:
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123" ++) show ioi
--     in (*3) changed

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- how to test:
-- *Ch16> :{
-- *Ch16| let f :: [Int] -> Bool
-- *Ch16|     f x = functorIdentity x
-- *Ch16| :}
-- *Ch16> quickCheck f 

functorCompose :: (Functor f, Eq (f c)) => 
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- how to test:
-- *Ch16> let c = functorCompose (+1) (*2)
-- *Ch16> let li x = c (x :: [Int])
-- *Ch16> quickCheck li

functorCompose' :: (Functor f, Eq (f c)) => 
  f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- how to test:
type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- *Ch16> quickCheck (fc' :: In
-- *Ch16> quickCheck (fc' :: IntFC )

-- impl functor inst for following:
-- use quickcheck properties to validate them:

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

-- *Ch16> :{ 
-- *Ch16| let ff :: Four Int Bool String Int -> Bool
-- *Ch16|     ff x = functorIdentity x
-- *Ch16| :}
-- *Ch16> quickCheck ff
-- +++ OK, passed 100 tests.

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' x y z b) = Four' x y z $ f b

data Possibly a = LolNope
                | Yeppers a 
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap _ LolNope = LolNope

data Sum a b = First a 
             | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = sumMap

sumMap :: (b -> c) -> Sum a b -> Sum a c
sumMap f (Second b) = Second $ f b
sumMap _ (First a) = First a


-- newtype Constant a b = Constant {getConstant :: a}
--   deriving (Eq, Show)

-- instance Functor (Constant m) where
--   fmap _ (Constant v) = Constant v

data Wrap f a = Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- fmap (+1) (Wrap (Just 1)) ==> Wrap (Just 2)

-- if we don't want to change the values in a structure and just
-- change the structure then we basically want natural 
-- transformation. which can be simulated by the following:
-- in natural trans we care as much about what we don't want as we
-- care about what we do want. we'll get to this in applicatives.
type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]


-- if you really want to get around the fact that functors are
-- unique to a data type:
-- however flip tup a b is different than tup a b even if it's 
-- only there to provide for different functor behavior!
data Tup a b = Tup a b 
  deriving (Eq, Show)

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

-- instance Functor (Flip Tup a) where
--   fmap f (Flip (Tup a b)) = Flip $ Tup (f a) b
