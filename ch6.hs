module Ch6 where

data TisAnInteger = TisAn Integer 

instance Eq TisAnInteger where 
  (==) (TisAn i) (TisAn i') = i==i'

data TowIntegers = Two Integer Integer

instance Eq TowIntegers where
  (==) (Two i j) (Two i' j') = i==i' && j==j'

data StringInt = TisAnInteger Int 
               | TisAString   String 

instance Eq StringInt where
  (==) (TisAnInteger i) (TisAnInteger i') = i==i'
  (==) (TisAString s)   (TisAString s')   = s==s'
  (==) _ _ = False

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x==x' && y==y'

data EitherOr a b = Hello a 
                  | Bye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x==x'
  (==) (Bye y)   (Bye y')   = y==y'
  (==) _ _ = False
