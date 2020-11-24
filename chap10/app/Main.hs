{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import Lib

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show, Eq, Ord, Read)

data Client i
  = GovOrg {clientId :: i, clientName :: String}
  | Company {clientId :: i, clientName :: String, person :: Person, duty :: String}
  | Individual {clientId :: i, person :: Person}
  deriving (Show, Eq, Ord, Read)

person1 = Person "Ali" "Ahmed"

c1 = Company 3 "Punchcard" person1 "Developer"

listOfClients :: [Client Int]
listOfClients = [GovOrg 1 "NASA", Individual 2 (Person "Samrah" "Akber"), c1]

main :: IO ()
main = someFunc

saveClients :: FilePath -> [Client Int] -> IO ()
saveClients fpath clients =
  runConduitRes $
    L.sourceList clients
      .| L.map clientsToText
      .| L.concatMap (\x -> [x, "\n"]) -- write '\n' between clients
      .| CT.encode CT.utf8
      .| B.sinkFile fpath

clientsToText :: Client Int -> Text
clientsToText (GovOrg i n) = "client(gov," <> escapeString (show i) <> "," <> escapeString n <> ")"
clientsToText (Company i n p d) =
  "client(com," <> escapeString (show i) <> "," <> escapeString n <> "," <> personToText p <> "," <> escapeString d <> ")"
clientsToText (Individual i p) =
  "client(ind," <> escapeString (show i) <> "," <> personToText p <> ")"

personToText :: Person -> Text
personToText (Person f l) = "person(" <> escapeString f <> "," <> escapeString l <> ")"

escapeString :: String -> Text
escapeString = T.replace "\n" "\\n" . T.replace "," "\\," . T.replace "(" "\\(" . T.replace ")" "\\)" . T.pack

--- Using builders to do the above more efficiently

clientToText2 :: Client Int -> B.Builder
clientToText2 (GovOrg i n) =
  "client(gov,"
    <> B.decimal i
    <> B.singleton ','
    <> B.fromText (escapeString n)
    <> B.singleton ')'
clientToText2 (Company i n p d) =
  "client(com,"
    <> B.decimal i
    <> B.singleton ','
    <> B.fromText (escapeString n)
    <> B.singleton ','
    <> personToText2 p
    <> B.singleton ','
    <> B.fromText (escapeString d)
    <> B.singleton ')'
clientToText2 (Individual i p) =
  "client(ind,"
    <> B.decimal i
    <> B.singleton ','
    <> personToText2 p
    <> B.singleton ')'

personToText2 :: Person -> B.Builder
personToText2 (Person f l) =
  "person("
    <> B.fromText (escapeString f)
    <> B.singleton ','
    <> B.fromText (escapeString l)
    <> B.singleton ')'

saveClients2 :: FilePath -> [Client Int] -> IO ()
saveClients2 fpath clients =
  runConduitRes $
    L.sourceList clients
      .| L.map clientToText2
      .| L.map (LT.toStrict . B.toLazyText)
      .| L.concatMap (\x -> [x, "\n"])
      .| CT.encode CT.utf8
      .| B.sinkFile fpath

--- Exercise

data Product = Product {id :: Int, name :: String, price :: Double, description :: String}
  deriving (Eq, Ord, Show)

data Purchase = Purchase {client :: Client Int, products :: [Product]}
  deriving (Eq, Ord, Show)

p1 = Product 1 "Pillow" 65.0 "Cervical Pillow from Amazon"

p2 = Product 2 "Book" 30.25 "Book By Sean Carroll"

pur1 = Purchase c1 [p1, p2]

productsToText :: [Product] -> B.Builder
productsToText [] = ""
productsToText ((Product i n p d) : xs) =
  "prod("
    <> B.decimal i
    <> B.singleton ','
    <> B.fromText (escapeString n)
    <> B.singleton ','
    <> B.realFloat p
    <> B.singleton ','
    <> B.fromText (escapeString d)
    <> B.singleton ')'
    <> B.singleton ','
    <> productsToText xs

purchaseToText :: Purchase -> B.Builder
purchaseToText (Purchase c prods) =
  "purchase("
    <> clientToText2 c
    <> B.singleton ','
    <> B.singleton '['
    <> productsToText prods
    <> B.singleton ']'
    <> B.singleton ')'

---- Parsing --------

data GreetingYear = GreetingYear Text Int
  deriving (Eq, Ord, Show)

greetingYearParser = GreetingYear <$> (string "hello" <|> string "bye") <*> decimal

greetingYearParserS2 :: Parser GreetingYear
greetingYearParserS2 =
  (\g _ y -> GreetingYear g y)
    <$> (string "hello" <|> string "bye")
    <*> char ' '
    <*> decimal

greetingYearParserS :: Parser GreetingYear
greetingYearParserS = GreetingYear <$> (string "hello" <|> string "bye") <* char ' ' <*> decimal

aChar :: Parser Char
aChar =
  (const ',') <$> (string "\\,")
    <|> (const '\n') <$> (string "\\n")
    <|> (const '(') <$> (string "\\(")
    <|> (const ')') <$> (string "\\)")
    <|> satisfy (notInClass ",\n()")

aString = many aChar

aPerson :: Parser Person
aPerson = Person <$ string "person(" <*> aString <* char ',' <*> aString <* char ')'

aClient :: Parser (Client Int)
aClient =
  GovOrg <$ string "client(gov," <*> decimal <* char ',' <*> aString <* char ')'
    <|> Company <$ string "client(com," <*> decimal <* char ',' <*> aString <* char ',' <*> aPerson <* char ',' <*> aString <* char ')'
    <|> Individual <$ string "client(ind," <*> decimal <* char ',' <*> aPerson <* char ')'

--  Exercise 10-2

aProd :: Parser Product
aProd = Product <$ string "prod(" <*> decimal <* char ',' <*> aString <* char ',' <*> double <* char ',' <*> aString <* char ')'

aPurchase :: Parser Purchase
aPurchase = Purchase <$ string "purchase(" <*> aClient <* char ',' <* char '[' <*> sepBy aProd (char ',') <* (option ' ' $ char ',') <* char ']' <* char ')'

parseClients :: Parser [Client Int]
parseClients = sepBy aClient (char '\n')

loadClients :: FilePath -> IO [Client Int]
loadClients fPath =
  runConduitRes $
    B.sourceFile fPath .| CT.decode CT.utf8 .| sinkParser parseClients

--- JSON

-- clientToJSON :: Client Integer -> Value
-- clientToJSON (GovOrg i n) =
--   object
--     [ "type" .= String "govorg",
--       "id" .= Number (fromInteger i),
--       "name" .= String (T.pack n)
--     ]
-- clientToJSON (Company i n p d) =
--   object
--     [ "type" .= String "company",
--       "id" .= Number (fromInteger i),
--       "name" .= String (T.pack n),
--       "person" .= personToJSON p,
--       "duty" .= String (T.pack d)
--     ]
-- clientToJSON (Individual i p) =
--   object
--     [ "type" .= String "individual",
--       "id" .= Number (fromInteger i),
--       "person" .= personToJSON p
--     ]

-- personToJSON :: Person -> Value
-- personToJSON (Person f l) =
--   object
--     [ "first" .= String (T.pack f),
--       "last" .= String (T.pack l)
--     ]

-- jsonToPerson :: Value -> Parser Person
-- jsonToPerson (Object o) = Person <$> o .: "first" <*> o .: "last"
-- jsonToPerson _ = Control.Applicative.empty

-- instance ToJSON Person where
--   toJSON = personToJSON

-- instance FromJSON Person where
--   parseJSON = jsonToPerson

-- jsonToClient :: FromJSON i => Value -> Parser (Client i)
-- jsonToClient (Object o) =
--   case M.lookup "type" o of
--     Just (String "govorg") -> GovOrg <$> o .: "id" <*> o .: "name"
--     Just (String "company") -> Company <$> o .: id <*> o .: "name" <*> o .: "person" <*> o .: "duty"
--     Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
--     _ -> Control.Applicative.empty
-- jsonToClient _ = Control.Applicative.empty

-- instance ToJSON (Client Integer) where
--   toJSON = clientToJSON

-- instance FromJSON i => FromJSON (Client i) where
--   parseJSON = jsonToClient