{-# LANGUAGE GADTs #-}

module Lib where

data Offer a
  = Present a
  | PercentDiscount Float
  | AbsoluteDiscount Float
  | Restrict [a] (Offer a) -- product Restriction
  | From Integer (Offer a) -- time restriction
  | Until Integer (Offer a)
  | Extend Integer (Offer a)
  | Both (Offer a) (Offer a)
  | BetterOf (Offer a) (Offer a) -- offer combinators
  | If (Expr2 a) (Offer a) (Offer a)
  deriving (Show)

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

data Expr2 a
  = AmountOf2 a
  | PriceOf2 a
  | TotalNumberOfProducts2
  | TotalPrice2
  | IVal2 Integer
  | FVal2 Float
  | (Expr2 a) :#+: (Expr2 a) -- adding a # before each operator to differentiate from newer definitions below
  | (Expr2 a) :#*: (Expr2 a)
  | (Expr2 a) :#<: (Expr2 a)
  | (Expr2 a) :#<=: (Expr2 a)
  | (Expr2 a) :#>: (Expr2 a)
  | (Expr2 a) :#>=: (Expr2 a)
  | (Expr2 a) :#&&: (Expr2 a)
  | (Expr2 a) :#||: (Expr2 a)
  | Not2 (Expr2 a)
  deriving (Show)

-- express the following offer
-- "For the next 30 days, you will get the best of these two details"
-- 1. Either getting a discount of $10 off your final bill AND getting a ballon as a present
-- OR
-- 2. if you buy more than $100 of products, a 5 percent discount

offer1 :: Offer String
offer1 =
  Until
    30
    ( BetterOf
        (Both (AbsoluteDiscount 10) (Present "Ballon"))
        (If (TotalPrice2 :#>: IVal2 100) (PercentDiscount 5) noOffer)
    )

---------------------------------------------------------------
-- Exercise 13-1 --

period :: Integer -> Integer -> Offer a -> Offer a
period f d o = From f (Until d o)

-- period2 :: Integer -> Integer -> Offer a -> Offer a
-- period2 f d o = Both (From f o) (Until d o)

allOf :: [Offer a] -> Offer a
allOf = foldr Both noOffer

-- express the offer
-- Offer: From the 3rd day and for 5 days hence, you will get a free balloon, a free chocolate muffin
-- and a 10% discount in the Time Machine Store.

offer2 :: Offer String
offer2 = period 3 8 (allOf [Present "Balloon", Present "Chocolate Muffin", PercentDiscount 10])

---------------------------------------------------------------

data Expr a r where
  AmountOf :: a -> Expr a Integer
  PriceOf :: a -> Expr a Float
  TotalNumberOfProducts :: Expr a Integer
  TotalPrice :: Expr a Float
  IVal :: Integer -> Expr a Integer
  FVal :: Float -> Expr a Float
  (:+:) :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:) :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:<=:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:>:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:>=:) :: (Num n, Ord n) => Expr a n -> Expr a n -> Expr a Bool
  (:&&:) :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:) :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not :: Expr a Bool -> Expr a Bool

---------------------- Exercise 13-2 ----------------------

data ExprR = EInt Integer | EFloat Float | EBool Bool

interpretExpr2 :: Eq a => Expr2 a -> [(a, Float)] -> ExprR
interpretExpr2 (AmountOf2 a) xs =
  EInt $ foldl (\acc (n, _) -> if n == a then 1 + acc else acc) 0 xs
interpretExpr2 (PriceOf2 a) xs =
  EFloat $ foldl (\acc (n, p) -> if n == a then p + acc else acc) 0 xs
interpretExpr2 TotalNumberOfProducts2 xs = EInt $ fromIntegral $ length xs
interpretExpr2 TotalPrice2 xs = EFloat $ foldl (\acc (_, p) -> acc + p) 0 xs
interpretExpr2 (e1 :#+: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EFloat f1, EFloat f2) -> EFloat $ f1 + f2
    (EInt n1, EInt n2) -> EInt $ n1 + n2
    _ -> error "Type Error"
interpretExpr2 (e1 :#*: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EFloat f1, EFloat f2) -> EFloat $ f1 * f2
    (EInt n1, EInt n2) -> EInt $ n1 * n2
    _ -> error "Type Error"
interpretExpr2 (e1 :#<: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EInt n1, EInt n2) -> EBool $ n1 < n2
    (EFloat f1, EFloat f2) -> EBool $ f1 < f2
interpretExpr2 (e1 :#<=: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EInt n1, EInt n2) -> EBool $ n1 <= n2
    (EFloat f1, EFloat f2) -> EBool $ f1 <= f2
    _ -> error "Type Error"
interpretExpr2 (e1 :#>: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EInt n1, EInt n2) -> EBool $ n1 > n2
    (EFloat f1, EFloat f2) -> EBool $ f1 > f2
interpretExpr2 (e1 :#>=: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EInt n1, EInt n2) -> EBool $ n1 >= n2
    (EFloat f1, EFloat f2) -> EBool $ f1 >= f2
interpretExpr2 (e1 :#&&: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EBool b1, EBool b2) -> EBool $ b1 && b2
    _ -> error "Type Error"
interpretExpr2 (e1 :#||: e2) list =
  case (interpretExpr2 e1 list, interpretExpr2 e2 list) of
    (EBool b1, EBool b2) -> EBool (b1 || b2)
    _ -> error "Type Error"
interpretExpr2 (Not2 e) list =
  case interpretExpr2 e list of
    EBool b -> EBool $ not b
    _ -> error "Type Error"

interpretExpr :: (Eq a, Ord r) => Expr a r -> [(a, Float)] -> r
interpretExpr (e1 :+: e2) list = interpretExpr e1 list + interpretExpr e2 list
interpretExpr (e1 :*: e2) list = interpretExpr e1 list * interpretExpr e2 list
interpretExpr (e1 :||: e2) list = interpretExpr e1 list || interpretExpr e2 list
interpretExpr (e1 :&&: e2) list = interpretExpr e1 list && interpretExpr e2 list
interpretExpr (e1 :<: e2) list = interpretExpr e1 list < interpretExpr e2 list
interpretExpr (e1 :<=: e2) list = interpretExpr e1 list <= interpretExpr e2 list
interpretExpr (e1 :>: e2) list = interpretExpr e1 list > interpretExpr e2 list
interpretExpr (e1 :>=: e2) list = interpretExpr e1 list >= interpretExpr e2 list
interpretExpr (AmountOf x) list = _