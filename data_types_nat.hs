
data Nat = Zero | Suc Nat deriving (Show, Eq)


negative :: String -> a
negative f = error ("Data.Nat." ++ f ++ ": would be negative")


instance Enum Nat where
    succ                   = Suc
    pred                   = nat (negative "pred") id


instance Num Nat where
    (+) n       = foldNat n succ
    (*) n       = foldNat 0 (+n)
    (-) n       = nat n ((-) $! nat (negative "-") id n)
    negate      = nat 0 (const $ negative "negate")
    abs         = id
    signum      = nat 0 (const 1)
    fromInteger = unfoldNat $ \n -> case n of n | n < 0     -> negative "fromInteger"
                                                | n > 0     -> Just (n - 1)
                                                | otherwise -> Nothing


toNatural  = foldNat 0 succ
-- | Shallow deconstruction. Returns the first argument if @Zero@, applies the second argument to the inner value if @Succ@.
nat :: r -> (Nat -> r) -> Nat -> r
nat z s Zero     = z
nat z s (Suc n) = s n

-- | Returns the first argument if @Zero@, applies the second argument recursively for each @Succ@.
foldNat :: r -> (r -> r) -> Nat -> r
foldNat z s = nat z (s . foldNat z s)

-- | Build a @Nat@ from a seed value: the first argument should return the next seed value
--   if the building is to continue, or @Nothing@ if it is to stop.  A @Succ@ is added at each iteration.
unfoldNat :: (a -> Maybe a) -> a -> Nat
unfoldNat f a = maybe Zero (succ . unfoldNat f) (f a)



add :: Nat -> Nat -> Nat
add = (+)
mul :: Nat -> Nat -> Nat
mul = (*)

-- add :: Nat -> Nat -> Nat
-- add n = foldNat n Suc

-- mul :: Nat -> Nat -> Nat
-- mul n = foldNat 0 (+n)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

fac :: Nat -> Nat
fac Zero = (Suc Zero)
fac n = n * (fac (pred n))