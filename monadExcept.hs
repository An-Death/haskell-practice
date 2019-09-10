module MonadExcept where

import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (MonadPlus (mplus, mzero), ap, guard, liftM, msum)
import Text.Read (readEither)

newtype Except e a = Except {runExcept :: Either e a}
    deriving (Show)

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
    fmap = liftM

instance Applicative (Except e) where
    pure = return
    (<*>) = ap

instance Monad (Except e) where
    --  return :: a -> Except e a
    return a = Except (Right a)
    --  (>>=) :: Except e a -> (a -> Except e b) -> Except e b
    m >>= k =
        case runExcept m of
            Left e -> Except $ Left e
            Right a -> k a

instance Monoid e => Alternative (Except e) where
    empty = mzero
    (<|>) = mplus

instance Monoid e => MonadPlus (Except e) where
    mzero = Except (Left mempty)
    Except x `mplus` Except y = Except
        $ case x of
            Left e -> either (Left . mappend e) Right y
            r -> r

throwE :: e -> Except e a
throwE = except . Left

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
catchE m h =
    case runExcept m of
    Left e -> h e
    Right a -> except (Right a)

{-
do {action1; action2; action3 } `catchE` handler

Law:
catchE h (throwE e) == h e

-}
data DivByZero = ErrZero String | ErrOther
    deriving (Eq, Show)

(/?) :: Double -> Double -> Except DivByZero Double
x /? 0 = throwE $ ErrZero (show x ++ "/0;")
x /? y = return $ x / y

example0 :: Double -> Double -> Except DivByZero String
example0 x y = action `catchE` handler
    where
    action = do
        q <- x /? y
        return $ show q
    handler = \err -> return $ show err

data ReadError = EmptyInput | NoParse String
    deriving (Show)

-- | Stepic Task 3.1.8
--
-- >>> runExcept (tryRead "5" :: Except ReadError Int)
-- Right 5
-- >>> runExcept (tryRead "5" :: Except ReadError Double)
-- Right 5.0
-- >>> runExcept (tryRead "5zzz" :: Except ReadError Int)
-- Left (NoParse "5zzz")
-- >>> runExcept (tryRead "" :: Except ReadError (Bool, ()))
-- Left EmptyInput
-- >>> runExcept (tryRead "wrong" :: Except ReadError (Bool, ()))
-- Left (NoParse "wrong")
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead x = (except $ readEither x) `catchE` (\_ -> throwE $ NoParse x)

data SumError = SumError Int ReadError
    deriving (Show)

-- | Stepic Task 3.1.9
--
-- >>> runExcept $ trySum ["10", "20", "30"]
-- Right 60
-- >>> runExcept $ trySum ["10", "20", ""]
-- Left (SumError 3 EmptyInput)
-- >>> runExcept $ trySum ["10", "two", "30"]
-- Left (SumError 2 (NoParse "two"))
trySum :: [String] -> Except SumError Integer
trySum = fmap sum . traverse f . enumirate
    where
    enumirate = zip [1 ..]
    f (pos, s) = SumError pos `withExcept` tryRead s

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f (Except e) = case e of
    (Left e) -> Except (Left (f e))
    (Right a) -> Except (Right a)

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
    deriving (Eq, Show)

newtype SimpleError = Simple {getSimple :: String}
    deriving (Eq, Show)

instance Semigroup SimpleError

instance Monoid SimpleError where
    mempty = Simple ""
    Simple l `mappend` Simple r = Simple $ l ++ r

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
lie2se (ErrNegativeIndex) = Simple "[negative index]"

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs i
    | i < 0 = throwE ErrNegativeIndex
    | otherwise = helper xs i
    where
    helper [] n = throwE $ ErrIndexTooLarge i
    helper (x : _) 0 = except $ Right x
    helper (_ : xs) i = helper xs (i - 1)

newtype Validate e a = Validate {getValidate :: Either [e] a}
    deriving (Show)

instance Functor (Validate e) where
    fmap f = Validate . fmap f . getValidate

instance Applicative (Validate e) where
    pure = Validate . pure
    
    Validate (Right f) <*> vx = f <$> vx
    vf <*> Validate (Right x) = ($ x) <$> vf
    Validate (Left es1) <*> Validate (Left es2) = Validate $ Left (es1 `mappend` es2)

collectE :: Except e a -> Validate e a
collectE = either (Validate . Left . (: [])) pure . runExcept

validateSum :: [String] -> Validate SumError Integer
validateSum = fmap sum . traverse f . enumirate
    where
    enumirate = zip [1 ..]
    f (i, s) = collectE $ withExcept (SumError i) $ tryRead s
