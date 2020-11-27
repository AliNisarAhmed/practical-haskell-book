{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Aeson hiding (json)
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Db
import GHC.Generics
import Network.HTTP.Types
import SchemaDef
import qualified Text.Digestive as TD
import qualified Text.Digestive.Util as TDU
import Web.Spock
import Web.Spock.Config
import Web.Spock.Digestive

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ Db.createSqlitePool "example.db" 10
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ Db.runSqlPool (do Db.runMigration migrateAll) pool
  runSpock 3000 (spock spockCfg app)

type Api = SpockM Db.SqlBackend () () ()

type ApiAction a = SpockAction Db.SqlBackend () () a

runSQL ::
  (HasSpock m, SpockConn m ~ Db.SqlBackend) =>
  Db.SqlPersistT (LoggingT IO) a ->
  m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ Db.runSqlConn action conn

app :: Api
app = do
  get "products" $ do
    products <- runSQL $ Db.selectList [] [Db.Asc ProductId] :: ApiAction [Db.Entity Product]
    json products
  get "clients" $ do
    clients <- runSQL $ Db.selectList [] [Db.Asc ClientId] :: ApiAction [Db.Entity Client]
    json clients
  get ("clients" <//> var) $ \(clientId :: Integer) -> do
    client <- runSQL $ Db.get $ ClientKey (Db.SqlBackendKey $ fromIntegral clientId)
    case client of
      Just c -> json c
      Nothing -> errorJson 404 "Not found"
  get ("product" <//> var) $ \(productId :: Integer) -> do
    product <- runSQL $ Db.get $ ProductKey (Db.SqlBackendKey $ fromIntegral productId)
    case product of
      Just p -> json p
      Nothing -> errorJson 404 "Not found"
  post "products" $ do
    (v, mProd) <- runForm "product" productForm
    liftIO $ print mProd
    case mProd of
      Nothing -> errorJson 400 "Bad Request"
      Just p -> json p
  post "countries" $ do
    (v, mCountry) <- runForm "countryForm" countryForm
    liftIO $ print mCountry
    case mCountry of
      Nothing -> errorJson 400 "Bad Request"
      Just country -> json country

errorJson :: Int -> T.Text -> ApiAction ()
errorJson code message =
  json $
    object
      [ "result" .= String "failure",
        "error" .= object ["code" .= code, "message" .= message]
      ]

--- Forms

countryForm :: Monad m => TD.Form String m CountryRequest
countryForm =
  CountryRequest <$> "name" TD..: TD.text Nothing
    <*> "send" TD..: TD.bool (Just True)

productForm :: Monad m => TD.Form String m Product
productForm =
  Product <$> "name" TD..: TD.string Nothing
    <*> "description" TD..: TD.string Nothing
    <*> "price" TD..: TD.validate isANumber (TD.string Nothing)
    <*> "inStock" TD..: TD.check "Must be >= 0" (>= 0) (TD.validate isANumber (TD.string Nothing))

isANumber :: (Num a, Read a) => String -> TD.Result String a
isANumber = maybe (TD.Error "Not a number") TD.Success . TDU.readMaybe

data CountryRequest = CountryRequest {name :: T.Text, send :: Bool}
  deriving (Show, Read, ToJSON, FromJSON, Generic)