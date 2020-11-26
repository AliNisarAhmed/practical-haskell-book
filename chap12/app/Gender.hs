{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Gender where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

data Gender
  = Male
  | Female
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

derivePersistField "Gender"
