module Hackerrank where
import Data.Char (toLower, chr, isAlpha, ord, isLower)
import Data.List (sort, map, elemIndex, maximumBy, nub, nubBy, permutations, transpose, elemIndices)
import qualified Data.Set as Set
-- https://www.hackerrank.com/challenges/mars-exploration/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign

marsExploration = (foldr (\x acc -> acc + (f x))  0) . listOf
    where 
        pat = "SOS"
        f = missMatches pat
        listOf = (filter (pat /=) ) . (chunks (length pat))
        missMatches :: String -> String -> Int
        missMatches pat str = (foldr (\(l,r) acc -> (+) acc $ fromEnum (r/=l)) 0) $ zip pat str
            
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = chunk : left
    where
        (chunk, l) = splitAt n xs
        left = chunks n l

-- https://www.hackerrank.com/challenges/pangrams/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign
-- pangram = return True if string contains at least one symbol of acii alphabet case insensitive 

alpha = map chr [x | x <- [97 .. 97+25]]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

-- 
isPangram :: String -> Bool
isPangram = (alpha == ) . sort . (filter isAlpha) . removeDuplicates . ( map toLower )
--
isPangram' :: String -> Bool
isPangram' s = containsAllSymbols s
    where 
        containsAllSymbols = (alpha ==) .  prepare 
        prepare = sort . (filter isAlpha) . removeDuplicates . ( map toLower )


pangrams s 
    | isPangram s = "pangram"
    | otherwise   = "not pangram"


-- https://www.hackerrank.com/challenges/two-characters/forum
--- see GO implementations with matrics

-- https://www.hackerrank.com/challenges/caesar-cipher-1/problem

-- simpleCesar shftN = chr . (+shftN) . ord
-- But we need cypher only alpha chars
lowerAlpha = ['a' .. 'z']
upperAlpha = ['A' .. 'Z']

caesar n c 
    | isAlpha c = shift n alphabet !! index
    | otherwise = c
    where 
        index = case (elemIndex c alphabet) of 
            (Just n) -> n
        alphabet = if isLower c then lowerAlpha else upperAlpha



simpleCesar n = Data.List.map (caesar n) 

shift :: Int -> [a] -> [a]
shift _ [] = []
shift 0 xs = xs
shift n (x:xs) = shift (n-1) (xs ++ [x])

-- https://www.hackerrank.com/challenges/hackerrank-in-a-string/problem?h_r=next-challenge&h_v=zen
-- find hackerrank substrings
-- Return YES or NO
hackerrankInString :: String -> String
hackerrankInString s = case hackerrank s of 
    [] -> "YES"
    _ -> "NO"
    where 
        hackerrank = foldl hack pattern
        pattern = "hackerrank"
        hack [] _      = []
        hack (x:xs) c  
            | x == c = xs
            | otherwise = x:xs


-- https://www.hackerrank.com/challenges/weighted-uniform-string/problem
-- Weighted Uniform Strings
-- A uniform string consists of a single character repeated zero or more times. 
-- For example, ccc and a are uniform strings, but bcb and cd are not
-- each letter count = [a->1 .. z->26]


type UniformString = String

charValue :: Char -> Int
charValue c 
    | ord c > ord 'z' = error $ "invalid char " ++ [c]
    | ord c < ord 'a' = error $ "invalid char " ++ [c]
    | otherwise = ord c - ord 'a' + 1

allUniformStrings :: String -> [UniformString]
allUniformStrings = foldr f [] 
    where
        f ::  Char -> [String] -> [String]
        f b []  = [[b]]
        f b (x:xs)
            | head x == b = (b:x) : xs
            | otherwise   =  [b]:(x:xs)


weightedUniformStrings :: String -> [Int] -> [String]  -- [Yes/No]
weightedUniformStrings s queries = map (show' . contains') queries
    where 
        us =  allUniformStrings s
        useq = map (\x -> let fst = (charValue $ head x) in (fst, fst*length x)) us
        contains' x = any (contain x) useq
        contain x (fst,lst)
            | x `mod` fst == 0 =  x <= lst
            | otherwise = False
        show' True = "Yes"
        show' False = "No"


--https://www.hackerrank.com/challenges/funny-string/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign
-- Complete the funnyString function below.
funnyString s 
        | cs s == (cs . reverse) s = "Funny"
        | otherwise                = "Not Funny"
        where
            cs :: String -> [Int]
            cs [] = []
            cs (_:[]) = []
            cs (x:y:xs) = abs (ord x - ord y) : cs (y:xs)

-- https://www.hackerrank.com/challenges/gem-stones/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign&h_r=next-challenge&h_v=zen

gemstones :: [String] -> Int
gemstones = length . (foldr1 Set.intersection) . (Prelude.map Set.fromList)

--https://www.hackerrank.com/challenges/alternating-characters/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen

-- alternatingCharacters :: String -> Int
-- alternatingCharacters = snd . (foldr (\x (lstChar,acc) -> if lstChar == x then (lstChar, acc+1) else (x,acc)) (' ', 0))

alternatingCharacters :: String -> Int
alternatingCharacters = snd . (foldr cnt (' ', 0))
    where 
        cnt x (lstChar,acc) = case lstChar == x of
            True  -> (lstChar, acc+1) 
            False -> (x,acc)

-- https://www.hackerrank.com/challenges/the-love-letter-mystery/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign
theLoveLetterMystery = shifts . splitHalf
    where
        splitHalf s = splitAt ((length s + 1) `div` 2) s
        shifts (l, r) =  foldr ((+) . \(a,b) -> abs $ ord a - ord b) 0 $ zip l (reverse r)


-- https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles/problem

base = [[if r + c >= 33 && c - r <= 31 then '1' else '_' | c <- [1..63]] | r <- [1..32]]
dropEven s = map fst $ filter snd $ zip s $ cycle [False, True]
iter s = top ++ bot where
    half = map dropEven $ dropEven s
    padding = replicate 16 '_'
    top = map (\x -> padding ++ x ++ padding) half
    bot = map (\x -> x ++ "_" ++ x) half

solve n = (iterate iter base) !! n


appendAndDelete s t k = case sL <= k + i*2 && bothOddOrEven || kGtSum of 
    True  -> "Yes"
    False -> "No"
    where
        sL = length s + length t
        kGtSum = k > sL
        bothOddOrEven = sL`mod`2 == k`mod`2 
        i = length $ takeWhile (\x -> length x == 2 && head x == last x) $ transpose [s, t]


-- squares :: (Floating a, Num a) => a -> a -> [Integer]
squares a b = [x | x <- [ startFrom a .. startFrom b], x^2 >=a, x^2<=b ]
        where 
            startFrom = toInteger. round .sqrt . fromInteger

-- Complete the cutTheSticks function below.
cutTheSticks :: [Int] -> [Int]
cutTheSticks [] = []
cutTheSticks arr = length arr : (cutTheSticks $ shift arr )
    where 
        shift xs = filter (/=0) . map (abs . (minimum xs -) ) $ xs


nonDivisibleSubset k xs = (+) (fromEnum isOneDevisibleNumber) $ foldr counts 0 [1..k`div`2]
    where
        remainders = [length $ x `elemIndices` dk |x<-[0..k-1]]
        dk = map (\x ->x`mod`k) xs
        isOneDevisibleNumber = remainders!!0 > 0
        counts x count 
            | x == k-x = count +1
            |otherwise = count + max (remainders!!x) (remainders!!(k-x))


repeatedString ['a'] n = n
repeatedString s n   = if strDivN == 0 then allA else allA + countA (take (strDivN) s)
    where
        strDivN  = n  `mod` (length s)
        countA s = length $ 'a' `elemIndices` s
        allA     = (countA s) * (n `div` length s)

    