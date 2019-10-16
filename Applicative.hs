module DemoApplicative where

import Control.Applicative (liftA,liftA2)


data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr a1 a2 a3) = Tr (f a1) (f a2) (f a3)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr f1 f2 f3) <*> (Tr a1 a2 a3) = Tr (f1 a1) (f2 a2) (f3 a3)

newtype ZipList a = ZipList {getZipList :: [a]}
    deriving Show

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)

(>$<) :: (a -> b) -> [a] -> [b]
fn >$< arg = getZipList $ fn <$> ZipList arg

(>*<) :: [(a -> b)] -> [a] -> [b]
fn >*< arg = getZipList $ ZipList fn <*> ZipList arg

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1.0)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> (divideList' xs)


newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap = liftA

instance Functor (Arr3 e1 e2 e3) where
  fmap = liftA
  
instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\_ _ -> x)
  (<*>) (Arr2 g) (Arr2 h) = Arr2 $ liftA2 (<*>) g h

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\_ _ _ -> x)
  (<*>) (Arr3 g) (Arr3 h) = Arr3 $ (liftA2 . liftA2) (<*>) g h
