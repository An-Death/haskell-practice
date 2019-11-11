{-# LANGUAGE FlexibleContexts #-}
module TreeSum where 

import Text.Read (readEither)
import Control.Monad.Writer 
import Control.Monad.Trans.Except
import Control.Monad.Error.Class
import Data.Foldable (traverse_)


data Tree a = Leaf a | Fork (Tree a) a (Tree a)
data ReadError = EmptyInput | NoParse String
  deriving Show


-- | https://stepik.org/lesson/38580/step/13
-- >>> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
-- (Just (NoParse "oops"),3)
-- >>> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
-- (Nothing,34)
tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead "" = throwE EmptyInput
tryRead x = (except $ readEither x) `catchE` (\_ -> throwE $ NoParse x)

treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go  = go' tryRead 
        

tryRead' :: (Read a, MonadError ReadError m) => String -> m a
tryRead' s
    | null s              = throwError EmptyInput
    | otherwise           = liftEither $ case readEither s of 
        Left _  -> Left $ NoParse s
        Right v -> Right v

go' tryReadf s = do
    i <- tryReadf s
    lift $ tell $ Sum i
    return ()

-- | https://stepik.org/lesson/45331/step/3?unit=23645
-- | based on mtl based on mtl
-- >>> treeSum' $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
-- Left (NoParse "oops")
-- >>> treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
-- Right 34
treeSum' :: Tree String -> Either ReadError Integer
treeSum' t = sum <$> traverse tryRead' t