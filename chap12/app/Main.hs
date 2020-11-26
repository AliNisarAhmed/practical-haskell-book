{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Db
import Network.HTTP.Types
import SchemaDef
import Web.Spock
import Web.Spock.Config

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
  get ("products") $ do
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

errorJson :: Int -> T.Text -> ApiAction ()
errorJson code message =
  json $
    object
      [ "result" .= String "failure",
        "error" .= object ["code" .= code, "message" .= message]
      ]