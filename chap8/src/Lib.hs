-- stack command for multicore execution

-- stack build && stack exec -- chap8-exe 25 +RTS -N -s

module Lib where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Par
import System.Random

someFunc :: IO ()
someFunc = do
  let (x, y) = findTwoFactors 100000000 200000000
  print x
  print y

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n =
  let oneFactor = findFactor n 2
   in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m
  | n == m = n
  | n `mod` m == 0 = m
  | otherwise = findFactor n (m + 1)

findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _ = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)

---

printTicket :: Int -> Int -> [(Int, String)] -> [(Int, String)] -> String
printTicket customerId productId clients products = runPar $ do
  clientV <- new
  productV <- new
  fork $ lookupPar clientV customerId clients
  fork $ lookupPar productV productId products
  envV <- new
  letterV <- new
  fork $ printEnvelope clientV envV
  fork $ printLetter clientV productV letterV
  envS <- get envV
  letterS <- get letterV
  return $ envS ++ "\n\n" ++ letterS

lookupPar :: (Eq a, NFData b) => IVar (Maybe b) -> a -> [(a, b)] -> Par ()
lookupPar i _ [] = put i Nothing
lookupPar i x ((k, v) : r)
  | x == k = put i $ Just v
  | otherwise = lookupPar i x r

printEnvelope :: IVar (Maybe String) -> IVar String -> Par ()
printEnvelope clientV envV = do
  clientName <- get clientV
  case clientName of
    Nothing -> put envV "Unknown"
    Just n -> put envV $ "To: " ++ n

printLetter ::
  IVar (Maybe String) ->
  IVar (Maybe String) ->
  IVar String ->
  Par ()
printLetter clientV productV letterV = do
  clientName <- get clientV
  productName <- get productV
  case (clientName, productName) of
    (Nothing, Nothing) -> put letterV "Unknown"
    (Just n, Nothing) -> put letterV $ n ++ " bought something"
    (Nothing, Just p) -> put letterV $ "Someone bought " ++ p
    (Just n, Just p) -> put letterV $ n ++ " bought " ++ p

----

run :: IO ()
run = do
  v <- newMVar 10000
  forkDelay 5 $ updateMoney v
  forkDelay 5 $ readMoney v
  _ <- getLine
  return ()

updateMoney :: MVar Integer -> IO ()
updateMoney v = do
  m <- takeMVar v
  putStrLn $ "updating value, which is " ++ show m
  putMVar v (m + 500)

readMoney :: MVar Integer -> IO ()
readMoney v = do
  m <- readMVar v
  putStrLn $ "The current value is " ++ show m

randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (3, 15)
  threadDelay (r * 1000000)

forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

---

run2 :: IO ()
run2 = do
  v <- newMVar 10000
  s <- newMVar [("a", 7)]
  forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
  forkDelay 5 $ printMoneyAndStock v s
  _ <- getLine
  return ()

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a, Integer)] -> IO ()
updateMoneyAndStock product price money stock = do
  s <- takeMVar stock
  let Just productNo = lookup product s
  if productNo > 0
    then do
      m <- takeMVar money
      let newS = map decrementQuantity s
      putMVar money (m + price) >> putMVar stock newS
    else putMVar stock s
  where
    decrementQuantity (k, v) =
      if k == product
        then (k, v - 1)
        else (k, v)

printMoneyAndStock :: Show a => MVar Integer -> MVar [(a, Integer)] -> IO ()
printMoneyAndStock money stock = do
  m <- readMVar money
  s <- readMVar stock
  putStrLn $ show m ++ "\n" ++ show s

--- using STM to resolve deadlocking and inconsistent worldview problems with Concurrency

updateMoneyAndStockStm :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStockStm product price money stock = do
  s <- readTVar stock
  let Just productNo = lookup product s
  if productNo > 0
    then do
      m <- readTVar money
      let newS = map decrementQuantity s
      writeTVar money (m + price) >> writeTVar stock newS
    else return ()
  where
    decrementQuantity (k, v) =
      if k == product
        then (k, v - 1)
        else (k, v)

run3 :: IO ()
run3 = do
  v <- newTVarIO 10000
  s <- newTVarIO [("a", 7)]
  forkDelay 5 $ atomically $ updateMoneyAndStockStm "a" 1000 v s
  _ <- getLine
  return ()