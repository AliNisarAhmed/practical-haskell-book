{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Error
import Control.Exception
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans
import Data.Binary (Binary)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import Data.Conduit.Network
import qualified Data.Conduit.Serialization.Binary as S
import Data.Csv (FromRecord, ToRecord)
import qualified Data.Csv as Csv
import qualified Data.Csv.Conduit as Csv
import Data.Monoid
import Data.String
import Data.Typeable
import GHC.Generics (Generic)
import Lib
import Network.Socket
import System.Environment
import System.IO
import System.IO.Error
import System.Random (Random (randomIO, randomRIO))

main :: IO ()
main = do
  putStrLn "Where do you want to travel?"
  place <- getLine
  let year = (length place) * 10
  putStrLn $ "You should travel to year " ++ show year

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show, Eq, Ord, Read, Generic)

data Client i
  = GovOrg {clientId :: i, clientName :: String}
  | Company {clientId :: i, clientName :: String, person :: Person, duty :: String}
  | Individual {clientId :: i, person :: Person}
  deriving (Show, Eq, Ord, Read)

createVIPList :: Show a => [Client a] -> IO [Client a]
createVIPList =
  foldM
    ( \lst c -> do
        putStrLn $ "\nShould " ++ show c ++ "be included as VIP? "
        answer <- getLine
        case answer of
          'Y' : _ -> return $ c : lst
          _ -> return lst
    )
    []

run = do
  hSetBuffering stdout LineBuffering
  actionName <- getLine
  case lookup actionName listOfActions of
    Just action -> action
    Nothing -> putStrLn "Unknown action"

listOfActions :: [(String, IO ())]
listOfActions =
  [ ( "greet",
      do
        putStrLn "Your name? "
        name <- getLine
        putStrLn $ "Hello " ++ name
    ),
    ( "sum",
      do
        putStrLn "First Number: "
        n1 <- fmap read getLine
        putStrLn "Second Number: "
        n2 <- fmap read getLine
        putStrLn (show n1 ++ "+" ++ show n2 ++ "=" ++ show (n1 + n2))
    )
  ]

run2 = do
  (initial :: Int) <- fmap read getLine
  jumps <-
    unfoldrM
      ( \_ -> do
          next <- randomRIO (0, 3000)
          if next == initial
            then return Nothing
            else return $ Just (next, next)
      )
      initial
  print $ take 10 jumps

winATrip :: (Int, Int) -> Int -> IO ()
winATrip (init, final) guessesAllowed = do
  putStrLn $ "Enter a number between " ++ displayRange ++ " for a chance to win a trip to space, you have " ++ show guessesAllowed ++ " tries to guess the number"
  correctAnswer <- randomRIO (init, final)
  _ <-
    unfoldrM
      ( \g -> do
          (guess :: Int) <- fmap read getLine
          if guess == correctAnswer
            then do
              putStrLn "Congrats you won"
              return Nothing
            else
              if g == guessesAllowed
                then do
                  putStrLn $ "You lost, the correct number was " ++ show correctAnswer
                  return Nothing
                else do
                  putStrLn "Wrong guess, try again"
                  return $ Just (guess, g + 1)
      )
      1
  return ()
  where
    displayRange = show init ++ " and " ++ show final

-- Does not work

-- winATrip2 :: (Int, Int) -> Int -> IO ()
-- winATrip2 (init, final) guessesAllowed = do
--   putStrLn $ "Enter a number between " ++ displayRange ++ " for a chance to win a trip to space, you have " ++ show guessesAllowed ++ " tries to guess the number"
--   correctAnswer <- randomRIO (init, final)
--   whileJust_
--     (return $ Just 1)
--     ( \g -> do
--         (guess :: Int) <- fmap read getLine
--         if guess == correctAnswer
--           then do
--             putStrLn "Congrats you won"
--             return Nothing
--           else
--             if g == guessesAllowed
--               then do
--                 putStrLn $ "You lost, the correct number was " ++ show correctAnswer
--                 return Nothing
--               else do
--                 putStrLn "Wrong guess, try again"
--                 return $ Just $ g + 1
--     )
--   where
--     displayRange = show init ++ " and " ++ show final

--- Reading and WRiting Files

run3 :: IO ()
run3 = do
  clients <- fmap lines $ readFile "clients.db"
  clientAndWinners <-
    mapM
      ( \c -> do
          (winner :: Bool) <- randomIO
          (year :: Int) <- randomRIO (0, 3000)
          return (c, winner, year)
      )
      clients
  writeFile "clientsWinners.db" $ concatMap show clientAndWinners

run4 :: IO ()
run4 = do
  (inFile : outFile : _) <- getArgs
  inHandle <- openFile inFile ReadMode
  outHandle <- openFile outFile WriteMode
  loop inHandle outHandle
  hClose inHandle
  hClose outHandle
  where
    loop inHandle outHandle = do
      isEof <- hIsEOF inHandle
      if not isEof
        then do
          client <- hGetLine inHandle
          (winner :: Bool) <- randomIO
          (year :: Int) <- randomRIO (0, 3000)
          hPutStrLn outHandle $ (show (client, winner, year)) ++ "\n"
          loop inHandle outHandle
        else return ()

run5 :: IO ()
run5 = do
  (inFile : outFile : _) <- getArgs
  withFile inFile ReadMode $ \inHandle ->
    withFile outFile WriteMode $ \outHandle ->
      loop inHandle outHandle
  where
    loop inHandle outHandle = do
      isEof <- hIsEOF inHandle
      if not isEof
        then do
          client <- hGetLine inHandle
          (winner :: Bool) <- randomIO
          (year :: Int) <- randomRIO (0, 3000)
          hPutStrLn outHandle $ (show (client, winner, year)) ++ "\n"
          loop inHandle outHandle
        else return ()

-- Exercise 9.2

data ClientKind
  = GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Eq, Ord, Show)

clientKind :: Client i -> ClientKind
clientKind GovOrg {} = GovOrgKind
clientKind Company {} = CompanyKind
clientKind Individual {} = IndividualKind

storeClientByKinds :: IO ()
storeClientByKinds = do
  (inFile : _) <- getArgs
  withFile inFile ReadMode $ \inHandle ->
    withFile "GovOrgs.db" WriteMode $ \gHandle ->
      withFile "Companies.db" WriteMode $ \cHandle ->
        withFile "Individuals.db" WriteMode $ \iHandle ->
          loop inHandle gHandle cHandle iHandle
  where
    loop inh gh ch ih = do
      isEof <- hIsEOF inh
      if isEof
        then return ()
        else do
          clientStr <- hGetLine inh
          let client = (read clientStr) :: Client Int
          let h = getHandle client
          hPutStrLn h clientStr
      where
        getHandle c =
          case clientKind c of
            GovOrgKind -> gh
            CompanyKind -> ch
            IndividualKind -> ih

------------------------------------------------

-- Error Handling

data CompanyNameError
  = GovOrgArgument
  | IndividualArgument

companyName :: Client i -> Either CompanyNameError String
companyName Company {clientName = n} = Right n
companyName GovOrg {} = Left GovOrgArgument
companyName Individual {} = Left IndividualArgument

companyName2 :: MonadError CompanyNameError m => Client i -> m String
companyName2 Company {clientName = n} = return n
companyName2 GovOrg {} = throwError GovOrgArgument
companyName2 Individual {} = throwError IndividualArgument

companyNameDef :: MonadError CompanyNameError m => Client i -> m String
companyNameDef c = companyName2 c `catchError` (\_ -> return "")

-- Exceptions - or Impure Code Errors

run6 :: IO ()
run6 =
  do
    clients <- fmap lines $ readFile "clients.db"
    clientsAndWinners <-
      mapM
        ( \c -> do
            (winner :: Bool) <- randomIO
            (year :: Int) <- randomRIO (0, 3000)
            return (c, winner, year)
        )
        clients
    writeFile "clientWinners.db" $ concatMap show clientsAndWinners
    `catch` ( \(e :: IOException) ->
                if isDoesNotExistError e
                  then putStr "File does not exist"
                  else putStrLn $ "Other error: " ++ show e
            )

run7 :: IO ()
run7 =
  do
    (n1 :: Int) <- fmap read getLine
    (n2 :: Int) <- fmap read getLine
    putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show (n1 `div` n2)
    `catch` (\(_ :: ErrorCall) -> putStrLn "Error reading number")
    `catch` ( \(e :: ArithException) -> case e of
                DivideByZero -> putStrLn "Division by zero"
                _ -> putStrLn $ "Other error: " ++ show e
            )

run8 :: IO ()
run8 =
  catchJust
    (\e -> if e == DivideByZero then Just e else Nothing)
    ( do
        (n1 :: Int) <- fmap read getLine
        (n2 :: Int) <- fmap read getLine
        putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show (n1 `div` n2)
        `catch` (\(_ :: ErrorCall) -> putStrLn "Error reading number")
    )
    (\_ -> putStrLn "Division by zero")

run9 :: IO ()
run9 =
  do
    throw $ NoMethodError "I dont know what to do"
    `catch` ( \(e :: SomeException) ->
                do
                  putStrLn "An exception was thrown: "
                  putStrLn $ show e
            )

-- making out own custom exceptions by making them an instance of Show, Typeable and Exception

data AuthenticationException
  = UnknownUserName String
  | PasswordMismatch String
  | NotEnoughRights String
  deriving (Show, Typeable)

instance Exception AuthenticationException

run10 :: IO ()
run10 =
  do
    throw $ UnknownUserName "Ali"
    `catch` ( \(e :: AuthenticationException) -> do
                putStrLn "Exception"
                print e
            )

------- CONDUIT Library Intro --------

sumOneToTen = runConduitPure $ L.sourceList [1 .. 10] .| L.fold (+) 0

sumSquareOdd = runConduitPure $ L.sourceList [1 .. 20] .| L.filter odd .| L.map (\x -> x * x) .| L.fold (+) 0

firstTen = runConduitPure $ L.unfold (\x -> Just (x, x + 1)) 1 .| L.isolate 10 .| L.consume

people :: Monad m => ConduitT (Client i) Person m ()
people = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      case c of
        Company {person = p} -> yield p
        Individual {person = p} -> yield p
        _ -> return ()
      people

listOfClients :: [Client Integer]
listOfClients = [GovOrg 1 "NASA", Individual 2 (Person "Samrah" "Akber"), Company 3 "Punchcard" (Person "Ali" "Ahmed") "Developer"]

onlyPeople =
  runConduitPure $
    L.sourceList listOfClients
      .| people
      .| L.consume

countGovOrgs :: MonadState Int m => ConduitT (Client i) Void m Int
countGovOrgs = do
  client <- await
  case client of
    Nothing -> do
      n <- lift $ get
      return n
    Just c -> do
      case c of
        GovOrg {} -> lift $ modify (+ 1)
        _ -> return ()
      countGovOrgs

run11 :: IO ()
run11 = print $ execState (runConduit (L.sourceList listOfClients .| countGovOrgs)) 0

winners :: ConduitT (Client i) (Client i, Bool, Int) IO ()
winners = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      (w :: Bool) <- lift $ randomIO
      (y :: Int) <- lift $ randomRIO (0, 3000)
      yield (c, w, y)
      winners

printWinners :: ConduitT (Client Integer, Bool, Int) Void IO ()
printWinners = do
  c <- await
  case c of
    Nothing -> return ()
    Just (client, b, n) -> do
      lift $ print client
      lift $ print b
      lift $ print n

run12 :: IO ()
run12 = runConduit (L.sourceList listOfClients .| winners .| printWinners)

---- Exercise 9-4

myMapC :: Monad m => (a -> b) -> ConduitT a b m ()
myMapC f = do
  input <- await
  case input of
    Nothing -> return ()
    Just x -> do
      yield $ f x
      myMapC f

add2 :: IO [Integer]
add2 = runConduit $ L.sourceList [1, 2, 3] .| myMapC (+ 2) .| L.consume

myFilterC :: Monad m => (a -> Bool) -> ConduitT a a m ()
myFilterC f = do
  input <- await
  case input of
    Nothing -> return ()
    Just x -> do
      if f x
        then do
          yield x
          myFilterC f
        else myFilterC f

oddRemoved :: IO [Integer]
oddRemoved = runConduit $ L.sourceList [1, 2, 3, 4, 5] .| myFilterC even .| L.consume

myUnfoldC :: Monad m => (b -> Maybe (a, b)) -> b -> ConduitT i a m ()
myUnfoldC f b1 = do
  case f b1 of
    Nothing -> return ()
    Just (a, b) -> do
      yield a
      myUnfoldC f b

firstTen2 :: [Integer]
firstTen2 = runConduitPure $ myUnfoldC (\x -> Just (x, x + 1)) 1 .| L.isolate 10 .| L.consume

myFoldC :: Monad m => (b -> a -> b) -> b -> ConduitT a o m b
myFoldC f b = do
  input <- await
  case input of
    Nothing -> return b
    Just a -> do
      myFoldC f (f b a)

sumSquareOdd2 :: Integer
sumSquareOdd2 = runConduitPure $ L.sourceList [1 .. 20] .| L.filter odd .| L.map (\x -> x * x) .| myFoldC (+) 0

---- Using conduit with Files

winnersFile :: (Monad m, MonadIO m) => ConduitT BS.ByteString BS.ByteString m ()
winnersFile = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      (w :: Bool) <- liftIO $ randomIO
      (y :: Int) <- liftIO $ randomRIO (0, 3000)
      yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y ++ "\n")
      winnersFile

run13 :: IO ()
run13 = runConduitRes $ B.sourceFile "clients.db" .| B.lines .| winnersFile .| B.sinkFile "clientWinnersConduit.db"

--- Basic Networking with Conduit

isWinner :: ConduitT BS.ByteString BS.ByteString IO ()
isWinner = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      lift $ BS.putStrLn c
      (w :: Bool) <- liftIO $ randomIO
      (y :: Int) <- liftIO $ randomRIO (0, 3000)
      yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y)
      isWinner

serverApp :: AppData -> IO ()
serverApp d = runConduit $ appSource d .| isWinner .| appSink d

run14 :: IO ()
run14 = withSocketsDo $ runTCPServer (serverSettings 8900 "*") serverApp

run15 :: IO ()
run15 = withSocketsDo $ do
  (name : _) <- getArgs
  runTCPClient (clientSettings 8900 "127.0.0.1") (clientApp name)

clientApp :: String -> AppData -> IO ()
clientApp name d = do
  runConduit $ (yield $ BS.pack name) .| appSink d
  runConduit $
    appSource d
      .| ( do
             Just w <- await
             lift $ BS.putStrLn w
         )

-- run the TCP client using -- withArgs ["Ali"] run15
-- and if the TCP server is running, it will receive the request

-------- Binary Serialization --------

instance Binary Person

run16 :: IO ()
run16 = runConduitRes $ L.sourceList clients .| S.conduitEncode .| B.sinkFile "people.db"
  where
    clients = [Person "Alejandro" "Serrano", Person "Ali" "Ahmed"]

run17 :: IO ()
run17 =
  runConduitRes $
    B.sourceFile "people.db"
      .| S.conduitDecode
      .| L.mapM_ (\(p :: Person) -> lift $ putStrLn $ show p)

--- Handling CSV Files

instance FromRecord Person

instance ToRecord Person

run18 :: IO ()
run18 =
  runConduitRes $
    B.sourceFile "people.db"
      .| Csv.fromCsvLiftError (userError . show) Csv.defaultDecodeOptions Csv.NoHeader
      .| L.mapM_ (\(p :: Person) -> lift $ putStrLn $ show p)