{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Demo where 
import Data.List
import Control.Monad
import Data.Traversable
import Data.Functor.Compose

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps:: f (g a)}
    deriving (Eq, Show)


instance (Functor f, Functor g) => Functor (f |.| g) where 
    -- fmap :: (a->b) -> (f |.| g) a -> (f |.| g) b
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x 
{-
x :: f (g a)
phi :: g a -> g b 
fmap pfi x :: f (g b)
h :: (a -> b)
fmap h :: g a -> g b
-}
-- Functor Laws
{-
(1) fmap id container == container       ->
    fmap id container == id container    ->
    fmap id == id
-}

-- | First law of Functor proof
-- 
-- >>> law1 $ Just "asd"
-- True
law1 x =
        fmap id (Cmps x) == (Cmps $ fmap (fmap id) x) 
    &&  (Cmps $ fmap (fmap id) x) == (Cmps $ fmap id x)
    &&  (Cmps $ fmap id x)  == (Cmps $ id x)
    &&  (Cmps $ id x) == Cmps x

-- |
-- 
-- >>> law2 id id $ Just "asd"
-- True
--
-- >>> law2 succ pred $ Just "asd"
-- True
law2 h1 h2 x =
    fmap h2 (fmap h1 (Cmps x))  == fmap (h2 . h1) (Cmps x)


instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    -- pure :: a -> (|.|) f g a
    pure = Cmps . pure . pure
    -- (<*>) :: (|.|) f g (a -> b) -> (|.|) f g a -> (|.|) f g b
    Cmps h <*> Cmps x = Cmps $ fmap (<*>) h <*> x
    {-
    h :: f (g (a->b))
    x :: f (g a)
    (<*>)                :: g (a -> b) -> (g a -> g b)
    fmap (<*>)           :: f (g (a->b)) -> f (g a -> g b)
    fmap (<*>) h ""                         f (g a -> g b)
    (fmap (<*>) h) <*> x :: f (g b)
    -}


instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    -- foldMap :: (a -> m) -> (f |.| g) a -> m
    foldMap f (Cmps a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (f|.|g) where
    traverse f (Cmps x) = Cmps <$> traverse (traverse f) x


data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l v r) = Branch (fmap f l ) (f v) (fmap f r)

instance Foldable Tree where 
    foldr _ ini Nil = ini
    -- foldr f ini (Branch l v r) = foldr f (f v (foldr f ini r)) l
    foldr f ini (Branch l v r) = (\i -> foldr f i l) . f v . (\i -> foldr f i r) $ ini

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch l v r) = Branch <$> traverse f l <*> f v <*> traverse f r

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

-- | traverse in posorder 
-- >>> tree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- >>> foldMapDefault (\x->[x]) $ PostO tree
-- [1,3,2,5,4]

instance Foldable Preorder where
    foldr _ ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l v r)) = f v $ foldr f (foldr f ini (PreO r)) (PreO l)

instance Foldable Postorder where
    foldr _ ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l v r)) = foldr f (foldr f (f v ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
    foldMap f (LevelO Nil)            = mempty
    foldMap f (LevelO (Branch l v r)) = 
        f v `mappend` mconcat (unfoldr next [l, r]) where
            next [] = Nothing
            next (Nil:xs) = next xs
            next ts = Just $ (\(a,b) -> (mconcat a, mconcat b)) $ unzip $ (\b -> 
                case b of
                    Nil -> (mempty, [Nil])
                    (Branch l v r) -> (f v, [l, r])) <$> ts
                    

-- | tree tests
-- >>> tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
-- >>> foldr (:) [] tree
-- [1,2,3,4]
-- >>> foldr (:) [] $ PreO tree
-- [3,1,2,4]
-- >>> foldr (:) [] $ PostO tree
-- [2,1,4,3]
-- >>> foldr (:) [] $ LevelO tree
-- [3,1,4,2]


data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr a1 a2 a3) = Tr (f a1) (f a2) (f a3)

instance Foldable Triple where
    foldr f ini (Tr a1 a2 a3) = foldr f ini [a1,a2,a3]
    
instance Traversable Triple where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse f (Tr a1 a2 a3) = Tr <$> (f a1) <*> (f a2) <*> (f a3)


data Result a = Ok a | Error String deriving (Eq,Show)

instance Traversable Result where
    traverse f (Ok r)    = Ok <$> f r
    traverse f (Error e) = pure (Error e)
    
instance Functor Result where
    fmap = fmapDefault

instance Foldable Result where
    foldMap = foldMapDefault

-- Traverse Laws
-- 1) traverse Identity == Identity
-- prop> \x -> traverse Identity x == Identity x
--
-- | 2) traverse (Compose . fmap g2 . g1) == Compose . fmap (traverse g2) . traverse g1
-- >>> (Compose $ fmap (traverse Right) (traverse Just "ASD")) == (traverse (Compose . fmap Right . Just) "ASD")
-- True

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf

instance Traversable OddC where
    traverse f (Un a)        = Un <$> f a
    traverse f (Bi a1 a2 a3) = Bi <$> f a1 <*> f a2 <*> traverse f a3
    
instance Functor OddC where
    fmap = fmapDefault

instance Foldable OddC where
    foldMap = foldMapDefault

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x1 x2 xx) yy zz = Bi x1 x2 $ concat3OC xx yy zz
concat3OC (Un x) yy zz = concat2OC x yy zz

concat2OC :: a -> OddC a -> OddC a -> OddC a
concat2OC x (Un y) zz = Bi x y zz
concat2OC x (Bi y1 y2 yy) zz = Bi x y1 $ concat2OC y2 yy zz
 
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x1) = x1
concatOC (Bi sth1 sth2 sth3) = concat3OC sth1 sth2 (concatOC sth3)
 

 
instance Applicative OddC where
  pure x1 = Un x1
  (<*>) (Un fx1) x1 = ((<$>) fx1 x1)
  (<*>) (Bi fx1 fx2 fx3)  sth = concatOC $ Bi ((<$>) fx1  sth) ((<$>) fx2  sth) (Un ((<*>) fx3  sth))
 
instance Monad OddC where
  (>>=) sth f =  concatOC(fmap f sth)


  
-- Phantom type o_O
newtype Temperature a = Temperature Double 
    deriving (Num, Show, Eq, Fractional)

data Celsius
data Fahrenheit 
data Kelvin


comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8*c +32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature c) =  Temperature (c -273.15)