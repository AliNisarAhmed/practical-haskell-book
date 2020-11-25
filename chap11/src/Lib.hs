{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Gender
import SchemaDef

exampleConn :: IO ()
exampleConn = runNoLoggingT $
  withSqliteConn "example.db" $ \conn ->
    liftIO $
      flip runSqlPersistM conn $ do
        runMigration migrateAll

-- spain <- insert $ Country "Spain"
-- _client1 <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain (Just 32) (Just Male)
-- return ()

-- exampleConn :: IO ()
-- exampleConn = runNoLoggingT $
--   withSqliteConn "example.db" $ \conn -> do
--     runMigration migrateAll

-- query1 :: IO ()
-- query1 = runSqlite "example.db" $ do
--   client <- getClientByInfo "Alejandro" "Serrano" "Home Town, 1" "Spain"
--   liftIO $ putStrLn $ show client
--   return ()

getPurchaseClient p = get (purchaseClient p)

getClientById n = get $ ClientKey (SqlBackendKey $ fromIntegral n)

getClientByInfo fn ln addr cnName = do
  cn <- getBy $ UniqueCountryName cnName
  case cn of
    Just (Entity cId _) -> do
      cl <- getBy $ UniqueClient fn ln addr cId
      case cl of
        Just (Entity _ client) -> return $ Just client
        Nothing -> return Nothing
    Nothing -> return Nothing

query1 :: IO ()
query1 = runSqlite "example.db" $ do
  client <- getClientByInfo "Alejandro" "Serrano" "Home Town, 1" "Spain"
  liftIO $ putStrLn $ show client
  return ()

getAdultsOfSpainAndGermany :: ReaderT SqlBackend IO [Entity Client]
getAdultsOfSpainAndGermany = do
  es <- getBy $ UniqueCountryName "Spain"
  de <- getBy $ UniqueCountryName "Germany"
  let countries = map entityKey (catMaybes [es, de])
  selectList [ClientCountry <-. countries, ClientAge >=. Just 18] []

countAdultsOfSpainAndGermany :: ReaderT SqlBackend IO Int
countAdultsOfSpainAndGermany = do
  sp <- getBy $ UniqueCountryName "Spain"
  de <- getBy $ UniqueCountryName "Germany"
  let countries = map entityKey (catMaybes [sp, de])
  count [ClientCountry <-. countries, ClientAge >=. Just 18]

getAdultsOfSpainAndUS :: ReaderT SqlBackend IO [Entity Client]
getAdultsOfSpainAndUS = do
  Just (Entity spId _) <- getBy $ UniqueCountryName "Spain"
  Just (Entity usId _) <- getBy $ UniqueCountryName "United States"
  selectList ([ClientCountry ==. spId, ClientAge >=. Just 18] ||. [ClientCountry ==. usId, ClientAge >=. Just 21]) []

getProductsPage n = selectList [] [Asc ProductPrice, LimitTo 10, OffsetBy ((n -1) * 10)]

-- Esqueleto

getPeopleOver25 :: ReaderT SqlBackend IO [Entity Client]
getPeopleOver25 = E.select $
  E.from $ \client -> do
    E.where_ (client ^. ClientAge E.>. E.just (E.val 25))
    E.orderBy [E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName)]
    return client

getPeopleOver25FromSpainOrGermany :: ReaderT SqlBackend IO [Entity Client]
getPeopleOver25FromSpainOrGermany =
  E.select $
    E.from $ \(client, country) -> do
      E.where_
        ( client ^. ClientAge E.>. E.just (E.val 25)
            E.&&. country ^. CountryName `E.in_` E.valList ["Spain", "Germany"]
            E.&&. client ^. ClientCountry E.==. country ^. CountryId
        )
      E.orderBy [E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName)]
      return client

getPeopleOver25FromSpainOrGermanyJoin :: ReaderT SqlBackend IO [Entity Client]
getPeopleOver25FromSpainOrGermanyJoin =
  E.select $
    E.from $ \(client `E.InnerJoin` country) -> do
      E.on (client ^. ClientCountry E.==. country ^. CountryId)
      E.where_
        ( client ^. ClientAge E.>. E.just (E.val 25)
            E.&&. country ^. CountryName `E.in_` E.valList ["Spain", "Germany"]
        )
      E.orderBy
        [E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName)]
      return client

getMoneyByClient :: ReaderT SqlBackend IO [(Entity Client, E.Value (Maybe Double))]
getMoneyByClient =
  E.select $
    E.from $ \(client `E.LeftOuterJoin` purchase) -> do
      E.on (client ^. ClientId E.==. purchase ^. PurchaseClient)
      E.groupBy (client ^. ClientId)
      let s = E.sum_ (purchase ^. PurchaseAmount)
      return (client, s)

-- This is slow
capitalizeNamesSlow :: MonadIO m => SqlPersistT m ()
capitalizeNamesSlow = do
  clients <- selectList [] []
  mapM_
    ( \(Entity ident client) ->
        let c : rest = clientFirstName client
         in replace ident $
              client {clientFirstName = (toUpper c) : rest}
    )
    clients

discount :: ReaderT SqlBackend IO ()
discount = do
  updateWhere [ProductPrice <=. 10000] [ProductPrice *=. 0.9]
  updateWhere [ProductPrice >. 10000] [ProductPrice *=. 0.97]

betterDiscount :: ReaderT SqlBackend IO ()
betterDiscount = E.update $ \product -> do
  let totalAmount :: E.SqlExpr (E.Value (Maybe (Maybe Double)))
      totalAmount = E.subSelect $
        E.from $ \purchase -> do
          E.where_ $ product ^. ProductId E.==. purchase ^. PurchaseProduct
          E.groupBy (purchase ^. PurchaseProduct)
          return $ E.sum_ (purchase ^. PurchaseAmount)
  E.where_ $ E.isNothing totalAmount E.||. totalAmount E.<. E.just (E.just $ E.val 10)
  E.set product [ProductPrice E.*=. E.val 0.9]

--- Delete records

cleanProductStock :: MonadIO m => SqlPersistT m ()
cleanProductStock = deleteWhere [ProductInStock ==. 0]

cleanProductStock2 :: ReaderT SqlBackend IO ()
cleanProductStock2 = E.delete $
  E.from $ \product -> do
    E.where_ $ product ^. ProductInStock E.==. E.val 0 E.&&. (E.notExists $ E.from $ \purchase -> E.where_ (purchase ^. PurchaseProduct E.==. product ^. ProductId))