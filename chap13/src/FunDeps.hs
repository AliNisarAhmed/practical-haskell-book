{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module FunDeps where

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show)

newtype PersonReader a = PersonReader {runPersonReader :: Person -> a}
  deriving (Functor)

instance Applicative PersonReader where
  pure x = PersonReader $ const x
  PersonReader f <*> PersonReader x = PersonReader $ \env -> f env (x env)

instance Monad PersonReader where
  return = pure
  PersonReader x >>= f = PersonReader $ \env ->
    let a = x env
        PersonReader b = f a
     in b env

askp :: PersonReader Person
askp = PersonReader id

greetingp :: PersonReader String
greetingp = do
  person <- askp
  pure $
    concat
      [ "Greeting",
        show $ name person,
        ", you are ",
        show $ age person,
        " years old."
      ]

run1 :: IO ()
run1 = do
  let alice = Person "Alice" 30
  putStrLn $ runPersonReader greetingp alice

---- Generalizing the Reader

newtype Reader env a = Reader {runReader :: env -> a}
  deriving (Functor)

instance Applicative (Reader env) where
  pure x = Reader $ const x
  Reader f <*> Reader x = Reader $ \env -> f env $ x env

instance Monad (Reader env) where
  return = pure
  Reader x >>= f = Reader $ \env -> runReader (f (x env)) env

askr :: Reader env env
askr = Reader id

greetingr :: Reader Person String
greetingr = do
  person <- askr
  pure $
    concat
      [ "Greeting",
        show $ name person,
        ", you are ",
        show $ age person,
        " years old."
      ]

run2 :: IO ()
run2 = do
  let alice = Person "Alice" 30
  putStrLn $ runReader greetingr alice

--- Genralizing ask

-- class MonadReader m where
--   ask :: m env env

-- instance MonadReader PersonReader where
--   ask = PersonReader id

-- instance MonadReader Reader where
--   ask = Reader id

------- using multi param type classes

class Monad m => MonadReader env m | m -> env where
  ask :: m env

instance MonadReader Person PersonReader where
  ask = PersonReader id

instance MonadReader env (Reader env) where
  ask = Reader id

greeting :: PersonReader String
greeting = do
  person <- ask
  pure $ show person

run3 :: IO ()
run3 = do
  let alice = Person "Alice" 30
  putStrLn $ runReader greetingr alice

-- instance MonadReader String PersonReader where
--   ask = PersonReader $ \p -> name p

-- instance MonadReader Int PersonReader where
--   ask = PersonReader $ \p -> age p

-------- Using Type Families

class MonadReader2 m where
  type Env m
  askm :: m (Env m)

instance MonadReader2 PersonReader where
  type Env PersonReader = Person
  askm = PersonReader id

instance MonadReader2 (Reader env) where
  type Env (Reader env) = env
  askm = Reader id

------

greetinge :: (Monad m, MonadReader Person m) => m String
greetinge = do
  name <- askName
  age <- askAge
  pure $ name ++ " is " ++ show age ++ " years old "

askName :: MonadReader Person m => m String
askName = do
  p <- ask
  pure $ name p

askAge :: MonadReader Person m => m Int
askAge = age <$> ask