module Demo where 

newtype IntList = IList [Int] deriving (Show)

example = IList [1,2]

data IntList' = IList' [Int] deriving (Show)

ignore :: IntList -> String
ignore (IList _) = "HELO"


ignore' :: IntList' -> String
ignore' (IList' _) = "HELO"


-- paramtrized type
newtype Identity a = Identity {runIdentity :: a} 
    deriving (Eq, Ord)


-- newtype Xor = Xor { getXor :: Bool }
--     deriving (Eq,Show)

-- instance Monoid Xor where
--     mempty = Xor False
--     x `mappend` y = if getXor x && (not . getXor) y then Xor True
--                     else mempty



newtype First a = First {getFirst :: Maybe a} deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where 
    mempty = First Nothing
    mappend = (<>)

instance Semigroup (First a) where
    (First Nothing) <> m = m
    fst <> _ = fst


newtype Endo a = Endo { appEndo :: a->a}
instance Monoid (Endo a) where
    mempty = Endo id
    mappend = (<>)

instance Semigroup (Endo a) where
    (Endo f1) <> (Endo f2) = Endo (f1 . f2)
