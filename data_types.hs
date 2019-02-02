module Demo where

import Data.Either
import Data.Char(isDigit)
import Data.List ( any, all, sortOn, elemIndex, isSubsequenceOf)
import Data.List.Split (splitOn)
import Data.Time.Clock
import Data.Time.Format
import Text.Printf

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
    deriving (Show, Eq)

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

updateAge :: Int -> Person -> Person
updateAge newAge person = person {age = newAge}

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

fullName :: Person -> String
fullName (Person {lastName = ln, firstName = fn}) = fn ++ " " ++ ln

-- abbrFirstName :: Person -> Person
-- abbrFirstName p = p {firstName = abbreviation p}
--     where 
--         abbreviation (Person {firstName = fn})
--             | length fn <= 2 = fn
--             | otherwise = head fn :"."
abbrFirstName :: Person -> Person
abbrFirstName p@(Person (x:y:s) _ _) = p {firstName = x : "."}
abbrFirstName p = p


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving(Show)
-- Input : "firstName = John\nlastName = Connor\nage = 30"
-- split lines by "\n"
-- If parse failed -> ParsingError
-- If one of required field for Person not in input -> IncompleteDataError
-- If age not a number -> IncorrectDataError (value age)
-- All field outside Person should be ignored
parsePerson :: String -> Either Error Person
parsePerson  = recreatePerson . validateFields . filterField . parse 
    where
        delim = "\n"
        personFields = ["lastName", "firstName", "age"]
        personAge = personFields!!2
        containDelim = isSubsequenceOf delim
        parse :: String -> Either Error [String]
        parse s 
            | s == "" = Left ParsingError
            | not . containDelim $ s = Left ParsingError
            | otherwise = Right (splitOn delim s)
        
        
        filterField :: Either Error [String] -> Either Error [String]
        filterField (Right dt) = Right fields
            where
                fields = filter (\x -> any ($ x) (map isSubsequenceOf personFields)) dt
        filterField err = err

        
        validateFields :: Either Error [String] -> Either Error [String]
        validateFields (Right fields)
                | length fields < 3 = Left IncompleteDataError
                | all (not . isDigit) age = Left (IncorrectDataError age) 
                | otherwise = Right fields
                where
                    age = (!!2). words . head . filter (isSubsequenceOf personAge) $ fields 
        validateFields err = err
        
        recreatePerson ::Either Error [String] -> Either Error Person
        recreatePerson (Right fields) = Right (Person {age=read (f!!0), lastName=f!!1, firstName=f!!2})
            where
                f = cleanFields fields
                cleanFields = map snd . reverse . sortOn fst . map cleanField
                cleanField field = (pos, value) where
                    dt = words field
                    value = dt!!2
                    pos = elemIndex (dt!!0) personFields
        recreatePerson (Left err) = Left err
        

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

-----------------------4.3 Record Syntax -------------------------

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel ::LogLevel, message :: String}

instance Show LogEntry where
    show (LogEntry ts level message) = printf "%s: %s: %s" (timeToString ts) (logLevelToString level) (message)

logLevelToString :: LogLevel -> String
logLevelToString = show 

logEntryToString :: LogEntry -> String
logEntryToString = show

data Coord a = Coord a a

distance' :: Coord Double -> Coord Double -> Double
distance' (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1)^2 + (y2-y1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) =  f x1 x2 + f y1 y2 where
    f a b = abs $ a-b

-- Center of interesting Cell 
-- should be  lenCell * X + 1/2*lenCell
-- other words if X and Y is Int and thay mean the positon of bottom-left point of Cell
-- lenCell * (X + 0.5)
-- getCenter 2.2 (Coord 2 1) = Coord 5.5 3.3 
getCenter :: Double -> Coord Int -> Coord Double
getCenter lenCell (Coord x y) = Coord (f x) (f y) where 
    f :: Int -> Double
    f = (lenCell *) . inc05 . fromIntegral
    inc05 = (0.5 +)

getCell :: Double -> Coord Double -> Coord Int
getCell lenCell (Coord x y) = Coord (f x)  (f y) where 
    f :: Double -> Int
    f = floor . (/ lenCell)


-- roots' :: Double -> Double -> Double -> Either [Char] (Double, Double)
-- roots' a b c 
--     | discr >=0 = Right x1 x2
--     | otherwise = Left "discr is negative. Roots not found"
--     where   
--         x1 = helper (-d)
--         x2 = helper d
--         helper x = (-b + x) / (2*a)
--         d = sqrt discr
--         discr = b^2 - 4*a*c



findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) 
    | isDigit x = Just x
    | otherwise =  findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX x = case findDigit x of
    Just a -> a 
    Nothing -> 'X'




data List a = Nil | Cons a (List a) deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a xs) = a:fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil


data Tree a = Leaf a | Node  (Tree a) (Tree a) 
              deriving (Show, Eq) 

-- Get the height of a tree.
height :: Tree a -> Integer
height (Leaf _)             = 0
height (Node left right) = 1 + max (height left) (height right)

size :: Tree a -> Integer
size  (Leaf _)             = 0
size  (Node left right) = 1 + (size left) + (size right)

flattenTree (Leaf a) = [a]
flattenTree (Node left right) = (++) (flattenTree left)  (flattenTree right)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go t = (length ft, sum ft)
    ft = flattenTree t


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr -- | Expr :-: Expr
    deriving (Eq, Show)

expr1 = Val 2 :+: Val 3 :*: Val 4
expr2 = (Val 2 :+: Val 3) :*: Val 4

eval :: Expr -> Int
eval (Val n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e1
expand (e :*: (e1 :+: e2)) = expand e  :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2)         = expand e1 :+: expand e2
expand (e1 :*: e2)         = expand e1 :*: expand e2
expand e                   = e