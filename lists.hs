{-# LANGUAGE FlexibleContexts #-}
module Lists where 

import Data.Char
import Data.List

-- Создание списка элементов заданной длины
-- nTimes 'z'3 -> "zzz"
nTimes:: a -> Int -> [a]
nTimes v times
    | times > 0 = let 
        helper :: [a] -> a -> Int -> [a]
        helper acc v 0 = acc
        helper acc v times = helper (v:acc) v $ times-1
    in helper [] v times
    | otherwise = []


-- | Имплементации получения первого элемента пары и списка 
--   
-- Образец пары :: ((,) x y)
--
-- > :t fst' :: (t, t1) ->t
-- 
-- >>> fst' ("first", "second")
-- "first"
fst' :: (x, y) -> x
fst' ((,) x y) = x

-- Образец списка :: ((:) x xs)
-- :t head' :: [a] -> a
head' :: [a] -> a
head' ((:) x _) = x

tail' :: [a] -> [a]
tail' (_ : xs) = xs

second' :: [a] -> a
second' (_ : xs) = head xs

-- pattern matching variant 
second'' :: [a] -> a
second'' (_ : x : _) = x

third' :: [a] -> a
third' (_: _: x : _ ) = x

len' :: [a] -> Int
len' []     = 0
len' (x:xs) = 1 + len' xs
-- tail recur optimized

inc :: Int -> Int
inc x = x + 1

-- | sily realisation of length. I'm just learning
-- >>> len'' []
-- 0
-- >>> len'' [1,2,3]
-- 3
-- >>> len'' [0..99]
-- 100
len'' :: [a] -> Int
len'' []     = 0
len'' (_:xs) = let 
        helper :: Int -> [a] -> Int
        helper acc [] = acc
        helper acc (_:xs) = helper (inc acc) xs
    in helper 1 xs


-- | sums of all lists
-- >>> sum3 [1,2,3] [4,5] [6]
-- [11,7,3]
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 a b c = (head' a + head' b + head' c) : sum3 (tail' a) (tail' b) (tail' c) where
    tail' [] = []
    tail' l = tail l
    head' [] = 0
    head' l = head l
  
-- | filter by two predicates 
-- >>> filterDisj (< 10) odd [7,8,10,11,12]
-- [7,8,11]
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g l = filter anyOf' l where
    anyOf' x =  f x || g x

-- | quick sort implementation
-- prop> qsort xs == (qsort . qsort) (xs :: [Int])
-- >>> qsort [1,3,2,5]
-- [1,2,3,5]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort l =  qsort' (filter (< h) l) (filter (>= h) l)
            where h = head l
qsort' :: Ord a => [a] -> [a] -> [a]
qsort' [] (x : xs) = qsort' [x] xs
qsort' fs ss = qsort fs ++ qsort ss

-- | Возвращает список с обратным порядком элементов
-- >>> reverse [1,2,3]
-- [3,2,1]
reverse :: [a] -> [a]
reverse = undefined

-- | Реалиация Data.List.permutations кустарным способом
-- >>> perms [1,2,3]
-- [[1,2,3],[2,3,1],[3,1,2],[1,3,2],[3,2,1],[2,1,3]]
rotations :: Int -> [a] -> [[a]]
rotations len xs = take len (iterate (\(y:ys) -> ys ++ [y]) xs)

perms :: [a] -> [[a]]
perms []        = [[]]
perms [x]       = [[x]]
perms (x:[y])   = [[x,y],[y,x]]
perms (x:y:[z]) = [[x,y,z],[y,z,x],[z,x,y],[x,z,y],[z,y,x],[y,x,z]]
perms il@(x:xs) = concatMap ((rotations len).(x:)) (perms xs)
                  where len = length il

-- | Take? -_-
-- >>> take' 0 [1,2,3]
-- []
-- >>> take' 1 [1,2,3]
-- [1]
-- >>> take' 10 [0..]
-- [0,1,2,3,4,5,6,7,8,9]
take' :: Int -> [a] -> [a]
take' _ [] = error "Not valid"
take' 0 x = []
take' n (x:xs) = x:take' (n-1) xs


-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Integer deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 10) <$> arbitrary

-- | reqursive fibo
-- prop> \(Small n) -> fibo n == fibo (n + 2) - fibo (n + 1)
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)


fibStream' :: [Integer]
fibStream' = [fibo i | i<- [0..]]

-- | Fast and lazy implementation of fibo
-- >>> last $ take 11 fibStream
-- 55
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

data Odd = Odd Integer 
  deriving (Eq, Show)


instance Enum Odd where
    succ a = addEven a 2
    pred a = addEven a (-2)

    -- toEnum a = Odd $ toInteger  (div a 2 + 0.5)
    -- fromEnum a = toInteger a

    enumFrom a = iterate succ a
    -- enumFromThen a b 
    -- enumFromTo a b = [Odd x | x <- [c..d], odd x] where 
    --     c =  a::Integer
    --     d = b::Integer
    -- enumFromThenTo a b c = [a,b..c]

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"


-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, 
            test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests

coins :: Num a => [a]
coins = [2,3,7]
-- My own first try 
-- Not realy beatiful 
change :: (Ord a, Num a) => a -> [[a]]
change c = filter ((c==) . sum ) $ concatMap (\x -> helper' [x] c ) coins
helper' acc c
    | sum acc >= c = acc:[]
    | sum acc < c = concatMap (\x -> helper' (acc++[x]) c) coins

-- After some hours of optimizatios
change' :: (Ord a, Num a) => a -> [[a]]
change' n | n < 0     = []
          | n == 0    = [[]]
          | otherwise = [ x : xs | x <- coins, xs <- change' (n - x) ]
        

-- foldr from right to left
-- foldr f ini [1,2,3] ~>> 1 `f` (2 `f` (3 `f` ini))
foldr' :: (a -> b -> b) -> b -> [a] -> b 
foldr' f acc []      = acc
foldr' f acc (x:xs)  = x `f` foldr f acc xs

-- accumulate sum of power2  on all ellements witch > 0
sumPositiveSquares :: [Integer] -> Integer
sumPositiveSquares = foldr' f 0 where   
    f x s | x>0 = x^2 + s
          | otherwise = s

-- foldl from left to right
-- foldl f ini [1,2,3] ~>> ((ini `f` 1) `f` 2) `f` 3
-- f(f(f ini 1) 2) 3
-- not recomended to use, because on large list create large lazy func
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f acc [] =  acc
-- foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- copy of Data.List (foldl')
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f acc [] =  acc
foldl'' f acc (x:xs) = acc' `seq` foldl'' f acc' xs 
    where  acc' = f acc x


meanList :: [Double] -> Double
meanList = awg' . foldr f (0,0) where 
    f x (len, sum) = (len +1, sum+x)
    awg' (len, sum) = sum / len

-- left only values from even index's of list
-- GHCi> evenOnly [1..10]
-- [2,4,6,8,10]
-- GHCi> evenOnly ['a'..'z']
-- "bdfhjlnprtvxz"
evenOnly :: [a] -> [a]
evenOnly (x:y:xs) = y : evenOnly xs
evenOnly _ = []

-- returl last elem of list by using foldl1
lastElem :: [a] -> a
lastElem = foldl1 seq


-- implement of scanl
scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' f ini [] = [ini]
scanl'' f ini (x:xs) = ini:scanl'' f (ini `f` x) xs

facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1..]

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0


-- implement of scanr
scanr'' :: (a -> b -> b) -> b -> [a] -> [b]
scanr'' _ ini [] = [ini]
scanr'' f ini (x:xs) = (x `f` q):qs where
    qs@(q:_) = scanr'' f ini xs


unfold' :: (b -> (a, b)) -> b -> [a]
unfold' f ini = let (x, ini') = f ini in 
    x:unfold' f ini'

unfoldr' :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr' f ini = helper (f ini) where
    helper (Just (x, ini')) = x:unfoldr' f ini'
    helper Nothing = []

-- 
revRange :: (Char,Char) -> [Char]
revRange (f, l) = unfoldr' g l
    where g x 
              | x < f = Nothing
              | otherwise = Just (x, pred x)
          