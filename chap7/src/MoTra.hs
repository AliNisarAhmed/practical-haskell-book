module MoTra where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type DbConnString = String

data Config = Config {dbConn :: DbConnString, currentUser :: Int}
  deriving (Eq, Ord, Show)

data UserDetails = UserDetails {name :: String, email :: String}
  deriving (Eq, Ord, Show)

defaultUserDetails = UserDetails "Ali Ahmed" "abc@xyz.com"

initialState = UserDetails "" ""

defaultConfig :: Config
defaultConfig = Config "SQLServerConnectionString" 12

myMonad :: ReaderT Config [] Int
myMonad = do
  userId <- asks currentUser
  conn <- asks dbConn
  return userId

myMonad2 :: WriterT [String] (ReaderT Config IO) Int
myMonad2 = do
  userId <- lift $ asks currentUser
  tell ["fetched userId"]
  tell ["returning the userId"]
  return userId

getCurrentUserDetails :: DbConnString -> Int -> UserDetails
getCurrentUserDetails _ _ = defaultUserDetails

myMonad3 :: StateT UserDetails (WriterT [String] (ReaderT Config IO)) Int
myMonad3 = do
  userId <- lift $ asks currentUser
  tell ["fetched userId"]
  conn <- lift $ asks dbConn
  tell ["fetched db Connection"]
  put (getCurrentUserDetails conn userId)
  tell ["Store user details in state"]
  return userId
