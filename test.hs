module Test where 
import Data.Char
import Data.Function


dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1)**2 + (y2 - y1)**2

factorial :: Integer -> Integer
factorial n 
    | n >= 0 = let
            helper :: Integer -> Integer -> Integer
            helper acc 0 = acc
            helper acc n = helper (acc * n) (n - 1)
        in helper 1 n
    | otherwise = error "invalid args" 


-- Lazy factorial
factorial' :: Integer -> Integer
factorial' n 
        | n >= 0 = helper 1 n
        | otherwise = error "invalid args" 
    where 
        helper :: Integer -> Integer -> Integer
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
    
-- 
factorial'' :: Integer -> Integer
factorial'' n 
        | n >= 0 = helper 1 n
        | otherwise = error "invalid args" 
    where 
        helper :: Integer -> Integer -> Integer
        helper acc 0 = acc
        helper acc n = (helper $! (acc * n)) (n - 1)


fibonacci :: Integer -> Integer
fibonacci n
        | n == 0    = 0
        | n == 1    = 1
        | n == (-1) = 1
        | otherwise = helper n 0 1
    where 
        helper :: Integer -> Integer -> Integer -> Integer 
        helper n acc i
            | n == 0    = acc
            | n > 0     = helper (n - 1) i (acc + i)
            | n < 0     = helper (n + 1) i (acc - i)

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
    let 
        d = sqrt $ b**2 -4 *a *c
        aTwice = 2 * a 
        x1 = (-b - d) / aTwice
        x2 = (-d + d) / aTwice
    in (x1, x2)

rootsDiff :: Double -> Double -> Double -> Double
rootsDiff a b c = 
    let 
        (x1, x2) = roots a b c 
    in x2 - x1

seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = let 
        helper :: Integer -> Integer -> Integer -> Integer -> Integer
        helper n3 _ _ 2 = n3
        helper n3 n2 n1 n = helper (n3+n2 - 2*n1) n3 n2 (n-1)
        in helper 3 2 1 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (toInteger s, toInteger l) 
    where
        str = show $ abs x
        l = length $ str
        s = sum [ digitToInt x| x<-str ]


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h*( borderHalf + s) where 
    h = (b - a)/999
    borderHalf = (f a + f b)/2
    s = sum $ [f x| x <- [a, a+h ..b]]




multSecond = g `on` h
g x y = x*y
h y = let (x1, x2) = y in x2

sumFstFst = (+) `on` (\pp -> fst $ fst pp)

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z) 


class Printable a where 
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"
    
instance (Printable a, Printable b) => Printable (a,b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++")" 
