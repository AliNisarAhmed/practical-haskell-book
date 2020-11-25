{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SchemaDef where

import Data.Int (Int64)
import Database.Persist.TH
import Gender

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

	Country
		name String
		UniqueCountryName name
		deriving Show
	Client
		firstName String
		lastName String
		address String
		country CountryId
		age Int Maybe
		gender Gender Maybe
		UniqueClient firstName lastName address country
		deriving Show
	Product
		name String
		description String
		price Double
		inStock Int64
		UniqueProductName name
		deriving Show
	Purchase
		client ClientId
		product ProductId
		number Int
		amount Double
		deriving Show
	|]