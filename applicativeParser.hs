{-# LANGUAGE OverloadedStrings #-}
module ApplicativeParser where 

import Data.Char (isLower, isDigit, digitToInt)
import Control.Applicative
import Control.Monad
-- import Text.Parsec 


-- getList :: Parsec String u [String]
-- getList = (many1 digit) `sepBy` (char ';')

-- testList s expected = 
--     case parse getList "" s of 
--         (Left e)  ->  error $ show e
--         (Right v) -> print $ ["1", "234", "56"] == v

-- -- 
-- Applicative Parser naiv implementation 
-- type Parcer a = String -> Either String [(a, String)]
-- type Parcer a = String -> [(a, String)]
newtype Parser a = Parser {apply:: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . apply p

-- | Parse fist Char simbol of input stream or empty list on error
-- >>> apply anyChar "ABCD"
-- [('A',"BCD")]
anyChar :: Parser Char
anyChar = Parser f where
    f ""     = []
    f (c:cs) =  [(c,cs)]

instance Functor Parser where
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap f p = Parser func where 
        func s = [(f a, s')| (a,s') <- apply p s]

instance Applicative Parser where
    pure a = Parser func where
        func s = [(a, s)]

    -- | (<*>):: Parser (a->b) -> Parser a -> Parser b 
    -- | Testing apply instance
    -- >>> apply ((,) <$> anyChar <*> anyChar) "ABCDE"
    -- [(('A', 'B'), "CDE")]
    pf <*> pv = Parser func where
        func s = [(g a, s'') | (g, s') <- apply pf s, (a, s'') <- apply pv s']


-- | Satisfy parser by pred
--
-- >>> apply (satisfy (=='A')) "ABC"
-- [('A',"BC")]
-- 
-- >>> apply (satisfy (== 'A')) "BCA"
-- []
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser f where
    f "" = []
    f (c:cs) 
        | pred c    = [(c,cs)]
        | otherwise = []


-- | Is Lower parser
-- 
-- >>> apply lower "aBC"
-- [('a',"BC")]
--
-- >>> apply lower "ABC"
-- []
lower :: Parser Char
lower = satisfy isLower

-- | Parser by eq to char
--
-- >>> apply (char 'A') "ABC"
-- [('A',"BC")]
-- 
-- >>> apply (char 'A') "BCA"
-- []
char :: Char -> Parser Char
char c = satisfy (== c)


-- | Digit parser
-- 
-- >>> apply digit "123"
-- [(1,"23")]
--
-- >>> apply digit "a1"
-- []
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit


-- | Parse math mult like: "2*2" 
-- 
-- >>> parse multiplacation "2*2"
-- 4
-- 
-- >>> apply multiplacation "22*2"  --  ^ == [] // Not support yet
-- []
multiplacation :: Parser Int
multiplacation = (*) <$> digit <* char '*' <*> digit

{-
    Laws of Alternative 
    1) Right distributibity of <*> 
    (f <|> g ) <*> a === (f <*> a) <|> (g <*> a) // just look on (<|>) as math (+) and (<*>) as math (*)
    Left distributibity not required
    2) Right absorption for <*>
    empty <*> a === empty                        // like empty == 0 => 0 * _ == 0
    3) Left distributibity of fmap
    f <$> (a <|> b) === (f <$> a) <|> (f <$> b)
    4) Left absorption for fmap
    f <$> empty === empty
-}

-- |
--
-- >>> apply (char 'A' <|> char 'B') "ABC"
-- [('A',"BC")]
instance Alternative Parser where
    empty = Parser f where
        f _ = []  -- always returns error

    p <|> q = Parser f where
        f s = let ps = apply p s
            in  if null ps 
                then apply q s
                else ps


-- | parse all lowers letters
-- 
-- >>> apply lowers "abCdef"
-- [("ab","Cdef")]
--
-- >>> apply lowers "Abcdef"
-- [("","Abcdef")]
lowers :: Parser String
lowers = (:) <$> lower <*> lowers <|> pure ""


-- | generic parser many
-- 
-- >>> apply (many' lower) "abcD"
-- [("abc","D")]
many' :: Parser a -> Parser [a]
many' p = (:) <$> p <*> many' p <|> pure []

-- | parse at least one 
--
-- >>> apply (many1' lower) "Abcd"
-- []
--
-- >>> apply (many1' lower) "aBcd"
-- [("a","Bcd")]
many1' :: Parser a -> Parser [a]
many1' p = (:) <$> p <*> many' p

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
instance Functor Prs where
    fmap f p = Prs func where 
        func s = case (runPrs p s) of 
            Nothing -> Nothing
            (Just (c, cs)) -> Just ( (f c, cs))
  
instance Applicative Prs where
    pure a =  Prs func where
            func s = Just (a, s)
    (<*>) pf pv = Prs func where
            func s = do
                (f, s') <- runPrs pf s
                (a, s'') <- runPrs pv s'
                pure (f a, s'')

instance Alternative Prs where
    empty = Prs f where
        f _ = Nothing -- always returns error

    p <|> q = Prs f where
        f s = let ps = runPrs p s
            in  if null ps 
                then runPrs q s
                else ps

-- charE 'A' >>= \a -> charE 'B' >>= \b -> return (a,b)
instance Monad Prs where
    return = pure
    (>>=) pa f = Prs func where
        func s = do
            (a, s') <- runPrs pa s
            (b, s'') <- runPrs (f a) s'
            return (b, s'')
   

satisfyP :: (Char -> Bool) -> Prs Char
satisfyP pred = Prs f where
    f "" = Nothing
    f (c:cs) 
        | pred c    = Just (c,cs)
        | otherwise = Nothing
                
charP :: Char -> Prs Char
charP c = satisfyP (== c)
                
anyChrP :: Prs Char
anyChrP = Prs f where 
    f "" = Nothing
    f (c:cs) = Just (c,cs)
    
manyP :: Prs a -> Prs [a]
manyP p = (:) <$> p <*> manyP p <|> pure []

-- |
-- 
-- >>> runPrs (many1P $ charP 'A') "AAABCDE"
-- Just ("AAA","BCDE")
--
-- >>> runPrs (many1P $ charP 'A') "BCDE"
-- Nothing
many1P :: Prs a -> Prs [a]
many1P p = (:) <$> p <*> manyP p <|> empty

-- | parse Math.mult from string input
-- 
-- >>> runPrs mult "14*3"
-- Just (42,"")
--
-- >>> runPrs mult "64*32"
-- Just (2048,"")
--
-- >>> runPrs mult "77*0"
-- Just (0,"")
--
-- >>> runPrs mult "2*77AAA"
-- Just (154,"AAA")
mult :: Prs Int
mult = (*) <$> natP <* charP '*' <*> natP


-- | parse natural numbers 
-- 
-- >>> runPrs natP "14"
-- Just (14,"")
--
-- >>> runPrs natP ""
-- Nothing
--
-- >>> runPrs natP "77AAA"
-- Just (77,"AAA")
natP :: Prs Int
natP = read <$> many1P (satisfyP isDigit)


newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP f = PrsEP func where 
    func pos [] = (pos+1, Left ("pos " ++ (show $ pos+1) ++ ": unexpected end of input"))
    func pos (c:cx) = 
        let pos' = pos +1 in 
            if f c then
                (pos', Right (c, cx))
            else
                (pos', Left ("pos " ++ (show pos') ++ ": unexpected " ++ [c]))
    
instance Functor PrsEP where
    fmap f = PrsEP . (fmap . fmap . fmap . fmap $ \(a, s) -> (f a, s)) . runPrsEP

instance Applicative PrsEP where
    pure x = PrsEP $ \i s -> (i, Right (x, s))
    pf <*> px = PrsEP $ \i s -> case runPrsEP pf i s of
                (i', Left e) -> (i', Left e)
                (i', Right (f, s')) -> runPrsEP (f <$> px) i' s'

-- | 
--  
-- >>> runPrsEP empty 0 "ABCDEFG"
-- (0,Left "pos 0: empty alternative")
--
-- >>> parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE"
-- Left "pos 3: unexpected E"
--
-- >>> parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE"
-- Left "pos 3: unexpected E"
-- 
-- >>> parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF"
-- Left "pos 2: unexpected E"
instance Alternative PrsEP where
    empty = PrsEP $ \pos _ -> (pos, Left $ "pos " ++ (show pos) ++ ": empty alternative")

    (<|>) p1 p2 = PrsEP func where
        func pos s = if pos1 > pos2  then r1 else r2
            where 
                r1@(pos1, p1') = runPrsEP p1 pos s
                r2@(pos2, p2') = runPrsEP p2 pos s

charEP c = satisfyEP (== c)
anyEP = satisfyEP (const True)
testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

pythags = do
    z <- [1..]
    x <- [1..z]
    y <- [x..z]
    guard (x^2+y^2 == z^2)
    return (x,y,z)