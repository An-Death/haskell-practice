module Demo where

data B = T | F deriving (Show, Eq, Read, Enum)

not' :: B -> B
not' T = F
not' F = T


-- charToInt :: Char -> Int
-- charToInt x | x `elem` ['0'..'9'] = read [x]
charToInt :: Char -> Int
charToInt x = fromEnum x - 48

data Color = Red | Green | Blue

instance Show Color where
    show Red  = "Red"
    show Green  = "Green"
    show Blue  = "Blue"


stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'


data LogLevel = Error | Warning | Info
-- GHCi> cmp Error Warning
-- GT
-- GHCI> cmp Info Warning
-- LT

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (ord x)  (ord y) where
   ord Error = 3
   ord Warning = 2
   ord Info = 1

lessThanError :: LogLevel -> Bool
lessThanError lvl = 
    case cmp lvl Error of
        LT -> True
        _ -> False

---
data Point = Point Double Double deriving (Show)

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x^2 + y^2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x2 - x1)^2 + (y2-y1)^2

---
data Roots = Roots Double Double | None deriving(Show)

roots :: Double -> Double -> Double -> Roots
roots a b c 
    | discr >=0 = Roots x1 x2
    | otherwise = None
    where   
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2*a)
        d = sqrt discr
        discr = b^2 - 4*a*c


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False


(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- (***) f g p = (f $ fst p, g $ snd p)
(***) f g ~(x, y) = (f x, g y)


data Person = Person { firstName :: String, lastName :: String, age :: Int} 
    deriving (Show, Eq)

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x


