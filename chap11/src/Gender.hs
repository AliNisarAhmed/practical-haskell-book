{-# LANGUAGE TemplateHaskell #-}

module Gender where

import Database.Persist.TH

data Gender
	= Male
	| Female
	deriving (Eq, Read, Show)

derivePersistField "Gender"
