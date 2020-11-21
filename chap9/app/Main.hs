{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.Loops
import Data.String
import Lib
import System.Environment
import System.IO
import System.Random

main :: IO ()
main = do
  putStrLn "Where do you want to travel?"
  place <- getLine
  let year = (length place) * 10
  putStrLn $ "You should travel to year " ++ show year

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show, Eq, Ord, Read)

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
          hPutStrLn h $ show client
      where
        getHandle c =
          case clientKind c of
            GovOrgKind -> gh
            CompanyKind -> ch
            IndividualKind -> ih
