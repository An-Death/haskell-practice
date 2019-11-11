{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ValidatePassword where

import Control.Monad (msum, guard, when) 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Char (isNumber, isPunctuation)

-- Maybe impl
askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s

-- https://stepik.org/lesson/38580/step/8?unit=20505

newtype PwdError = PwdError String

instance Show PwdError where
    show (PwdError s) = "Incorrect input: " ++ s

instance Semigroup PwdError where
    (<>) = mappend

instance Monoid PwdError where
    mempty = PwdError ""
    mappend l r = PwdError $ show l ++ "\n" ++ show r

type PwdErrorIOMonad = ExceptT PwdError IO


askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

-- the code above cannot be changed by the condition of the task.

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
    s <- liftIO getLine
    validatePwd s `catchE` \e -> do
        liftIO $ print e
        throwE e


validatePwd :: String -> PwdErrorIOMonad String
validatePwd s
    | length s < 8              = throwE $ PwdError "password is too short!"
    | not $ any isNumber s      = throwE $ PwdError "password must contain some digits!"
    | not $ any isPunctuation s = throwE $ PwdError "password must contain some punctuation!"
    | otherwise = return s
