module Ch15 where

-- import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

-- laws:
-- -- left identity:
-- mappend mempty x = x

-- -- right identity:
-- mappend x mempty = x

-- -- associativity:
-- mappend x (mappend y z) = mappend (mappend x y) z

-- mconcat = foldr mappend mempty

-- instance Monid b => Monoid (a -> b)
-- keep in mind that you don't need the type constraint
-- for phantom types e.g.: data Test a = T | F


data Optional a = Nada | Only a 
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) = undefined 
  -- sconcat = undefined
  -- stimes = undefined

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = optionalMap

optionalMap :: Monoid a => Optional a -> Optional a -> Optional a
optionalMap Nada x = x
optionalMap x Nada = x
optionalMap (Only x) (Only y) = Only (mappend x y)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada), 
                         (1, fmap (First' . Only) arbitrary)]
instance Semigroup (First' a) where
  (<>) = firstMappend

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = firstMappend

firstMappend :: First' a -> First' a -> First' a
firstMappend (First' f@(Only a)) _                = First' f
firstMappend (First' Nada) (First' a)    = First' a
-- firstMappend (First' Nada) (First' Nada) = First' Nada

type FirstMappend = 
  First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do 
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-- class Semigroup a where
--   (<>) :: a -> a -> a

-- associativity law:
-- (a <> b) <> c = ((a <> b) <> c)

-- data NonEmpty a = a :| [a]
--   deriving (Eq, Show)












