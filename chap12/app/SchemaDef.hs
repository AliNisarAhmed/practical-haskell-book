{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Data.Aeson ()
import Data.Int (Int64)
import Database.Persist.TH
import Gender

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

	Country json
		name String
		canWeSend Bool default=True
		UniqueCountryName name
		deriving Show
	Client json
		firstName String
		lastName String
		address String
		country CountryId
		age Int Maybe
		gender Gender Maybe
		UniqueClient firstName lastName address country
		deriving Show
	Product json
		name String
		description String
		price Double
		inStock Int64
		UniqueProductName name
		deriving Show
	Purchase json
		client ClientId
		product ProductId
		number Int
		amount Double
		deriving Show
	|]