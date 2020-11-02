{-# LANGUAGE ViewPatterns #-}


module Main where

main :: IO ()
main = do
  putStrLn "hello world"


-- Exercise 2-1

emptyEmpty :: [[a]] -> Bool
emptyEmpty []      = True
emptyEmpty ([]: _) = True
emptyEmpty _       = False

oneElement :: [a] -> Bool
oneElement (x:_) = True
oneElement _     = False

concatTwo :: [[a]] -> [a]
concatTwo (x:y:xs) = x ++ y ++ concatTwo xs
concatTwo []       = []


----

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving Show

data Person
  = Person String String Gender
  deriving Show

data Gender = Male | Female | Unknown
  deriving Show

data TimeMachine
  = TimeMachine Manufacturer Int String Mode Float
  deriving (Show)

data Manufacturer = Manufacturer String String
  deriving (Show)

data Mode
  = Past
  | Future
  | Both
  deriving (Show)


--- Exercise 2-6

ack :: Integer -> Integer -> Integer
ack m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ack (m - 1) 1
  | otherwise = ack (m - 1) (ack m (n - 1))

myUnzip :: [(Int, Int)] -> ([Int], [Int])
myUnzip [] = ([], [])
myUnzip ((x1, y1): xs) = ([x1] ++ restX, [y1] ++ restY)
  where
    (restX, restY) = myUnzip xs



--- VIEW PAtterns

clientName :: Client -> String
clientName (GovOrg n)                            = n
clientName (Company n _ _ _)                     = n
clientName (Individual (Person fname lname _) _) = fname ++ " " ++ lname

specialClient :: Client -> Bool
specialClient (clientName -> "Ali Ahmed") = True
specialClient _                           = False


---- RECORDS


data ClientR
  = GovOrgR { clientRName :: String }
  | CompanyR { clientRName :: String, companyId :: Integer, person :: PersonR, duty :: String }
  | IndividualR { person :: PersonR }
  deriving (Show)

data PersonR = PersonR
  { firstName :: String, lastName :: String }
  deriving Show
