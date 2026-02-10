-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&))
import GHC.Base (TrName(TrNameD))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- False || True = True
-- True || False = True
-- False || False = False

--An alternative re-implementation
-- (||) :: Bool -> Bool -> Bool
-- False || False   = False
-- _ || _           = True

-- Another alternative re-implementation
(||) :: Bool -> Bool -> Bool
True || _     =  True
False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

fibonacci :: Int -> Int
-- fibonacci n
-- | n == 0 = 0
-- | n == 1 = 1
-- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--1
infix 3 &&

-- (&&):: Bool -> Bool -> Bool
-- True && True = True
-- False && True = False
-- True && False = False
-- False && False = False

-- (&&):: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False

(&&):: Bool -> Bool -> Bool
False && _ = False
_ && False = False
True && a = True

--2
exOr :: Bool -> Bool -> Bool
exOr True False = True
exOr False True = True
exOr _ _ = False

