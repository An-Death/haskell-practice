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
