{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Text
import GHC.Generics

-- from the article: https://www.williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

data Foo = Foo
  { field1 :: Int,
    field2 :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

jsonString :: LB.ByteString
jsonString = "{ \"field1\":27, \"field2\": \"hello\" }"

maybeFoo :: Maybe Foo
maybeFoo = decode jsonString

myFoo :: Foo
myFoo =
  Foo
    { field1 = 909,
      field2 = "take your time"
    }

customValue :: Value
customValue =
  object
    [ "list_price" .= (150000 :: Int),
      "sale_price" .= (143000 :: Int),
      "description" .= ("2-bedroom townhouse" :: String)
    ]

---- implementing custom parser

data Person = Person
  { firstName :: String,
    lastName :: String
  }
  deriving (Show)

instance ToJSON Person where
  toJSON Person {firstName, lastName} =
    object
      [ "first_name" .= firstName,
        "last_name" .= lastName
      ]

instance FromJSON Person where
  parseJSON = withObject "Person" $ \obj -> do
    firstName <- obj .: "first_name"
    lastName <- obj .: "last_name"
    return $ Person {firstName, lastName}

karlJSON :: LB.ByteString
karlJSON = "{\"first_name\": \"Karl\", \"last_name\": \"Popper\"}"

-- optional fields

data Item = Item
  { name :: String,
    description :: Maybe String
  }
  deriving (Show)

instance ToJSON Item where
  toJSON Item {name, description} = object ["name" .= name, "description" .= toJSON description]

--   toJSON Item {name, description = Just d} = object ["name" .= name, "description" .= d]
--   toJSON Item {name, description = Nothing} = object ["name" .= name]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \obj -> do
    name <- obj .: "name"
    description <- obj .:? "description"
    return $ Item {name, description}

item1 :: Item
item1 = Item {name = "Ali Ahmed", description = Just "Developer"}

item2 :: Item
item2 = Item {name = "Ali Ahmed", description = Nothing}

item1String :: LB.ByteString
item1String = "{ \"name\": \"Ali Ahmed\", \"description\": \"Developer\"}"

item2String :: LB.ByteString
item2String = "{ \"name\": \"Ali Ahmed\"}"

--- Parsing Enum Types

data UserType
  = User
  | Admin
  | CustomerSupport
  deriving (Show)

instance ToJSON UserType where
  toJSON v =
    case v of
      User -> String "user"
      Admin -> String "admin"
      CustomerSupport -> String "customer_support"

instance FromJSON UserType where
  parseJSON = withText "UserType" $ \text ->
    case text of
      "user" -> return User
      "admin" -> return Admin
      "customer_support" -> return CustomerSupport
      _ -> fail "String is not one of know enum values"

--- Weird JSON formats

data APIResult
  = JSONData Value
  | ErrorMessage Text
  deriving (Show)

instance FromJSON APIResult where
  parseJSON = withObject "APIResult" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then fmap JSONData (obj .: "data")
      else fmap ErrorMessage (obj .: "error_msg")

goodData :: LB.ByteString
goodData = "{\"ok\": true, \"data\": {\"foo\": 2}}"

badData :: LB.ByteString
badData = "{\"ok\": false, \"error_msg\":\"no_credentials\"}"

---

-- e.g. our API sends us data like
--
-- {
--   "element1": 42,
--   "element2": -20,
--   "element3": 1000
-- }
--
-- instead of [42, -20, 1000]

data JSONHashList a = HashList [a]
  deriving (Show)

instance (ToJSON a, Show a) => ToJSON (JSONHashList a) where
  toJSON (HashList lst) = object $ fmap (\(i, v) -> elementText i .= toJSON v) indexList
    where
      indexList = L.zip [1 ..] lst
      elementText :: Int -> Text
      elementText n = pack "element" <> (pack $ show n)

instance FromJSON a => FromJSON (JSONHashList a) where
  parseJSON = withObject "JSONHashList" $ \obj ->
    let kvs = HM.toList obj
        sorted = L.sortOn fst kvs
        vals = fmap snd sorted
        parsed = mapM parseJSON vals
     in fmap HashList parsed

weirdListData :: LB.ByteString
weirdListData = "{\"element1\":42,\"element2\":-20,\"element3\":1000}"

hashList :: JSONHashList Int
hashList = HashList [42, -20, 1000]

-- parsing directly from Value

value :: Value
value = object ["first_name" .= ("Ali" :: Text), "last_name" .= ("Ahmed" :: Text)]

p1 :: Maybe Person
p1 = parseMaybe parseJSON value :: Maybe Person

p2 :: Either String Person
p2 = parseEither parseJSON value :: Either String Person

--- Multiple parsing functions for a single type

snakeCaseParser :: Value -> Parser Person
snakeCaseParser = withObject "Person" $ \obj -> do
  firstName <- obj .: "first_name"
  lastName <- obj .: "last_name"
  return $ Person firstName lastName

pascalCaseParser :: Value -> Parser Person
pascalCaseParser = withObject "Person" $ \obj -> do
  firstName <- obj .: "FirstName"
  lastName <- obj .: "LastName"
  return $ Person firstName lastName

snakeCasePerson :: Value
snakeCasePerson =
  object
    [ "first_name" .= ("Ali" :: String),
      "last_name" .= ("Ahmed" :: String)
    ]

pascalCasePerson :: Value
pascalCasePerson =
  object
    [ "FirstName" .= ("Ali" :: String),
      "LastName" .= ("Ahmed" :: String)
    ]

-- use parseMaybe and parseEither with above to pass in parser of choice

--- Parsing directly into built in types

tupleizeFields :: Value -> Either String (Int, Bool)
tupleizeFields = parseEither $
  withObject "<fields>" $ \obj -> do
    field1 <- obj .: "field1"
    field2 <- obj .: "field2"
    return (field1, field2)

tupleJSON :: Value
tupleJSON =
  object
    [ "field1" .= (955 :: Int),
      "field2" .= True
    ]

-- we can go straight from ByteString to parsed data

tupleizeFieldsBS :: LB.ByteString -> Either String (Int, Bool)
tupleizeFieldsBS input = do
  object <- eitherDecode input
  let parser =
        ( \obj -> do
            field1 <- obj .: "field1"
            field2 <- obj .: "field2"
            return (field1, field2)
        )
  parseEither parser object

-- parsing nested fields e.g. { contact_info: { email: <string> } }

nested :: Value -> Parser String
nested = withObject "ContactInfo" $ \obj -> do
  contact <- obj .: "contact_info"
  contact .: "email"

n1 :: Value
n1 = object ["contact_info" .= object ["email" .= ("aliahmed@abc.com" :: Text)]]

-- we can define out own operator to handle nested data

(.->) :: FromJSON a => Parser Object -> Text -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

nestedParser :: Value -> Parser (String, String)
nestedParser = withObject "ContactInfo" $ \obj -> do
  email <- obj .: "contact_info" .-> "email"
  state <- obj .: "contact_info" .-> "address" .-> "state"
  return (email, state)

nestedObject :: Value
nestedObject =
  object
    [ "contact_info"
        .= object
          [ "email" .= ("williamyaoh@gmail.com" :: Text),
            "address"
              .= object
                [ "state" .= ("OK" :: Text),
                  "zip_code" .= ("74008" :: Text)
                ]
          ]
    ]