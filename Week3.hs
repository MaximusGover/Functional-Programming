-- We don't import '||' from the prelude, so that we can
-- define our own version

import System.Win32 (xBUTTON1)
import Prelude hiding (gcd, (&&), (||))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
-- (||) :: Bool -> Bool -> Bool
-- False || False   = False
-- _ || _           = True

-- Another alternative re-implementation
-- (||) :: Bool -> Bool -> Bool
-- True || _     =  True
-- False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = -mult (-n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

fibonacci :: Int -> Int

-- fibonacci n

-- | n == 0 = 0
-- | n == 1 = 1
-- | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- A naive re-implementation of the Prelude operator &&
(&&) :: Bool -> Bool -> Bool
True && True = True
False && True = False
True && False = False
False && False = False

-- An alternative re-implementation
-- (&&) :: Bool -> Bool -> Bool
-- True && True   = True
-- _ && _           = False

-- Another alternative re-implementation
-- (&&) :: Bool -> Bool -> Bool
-- False && _     =  False
-- True && a    = a

exOr :: Bool -> Bool -> Bool
exOr False x = x
exOr True x = not x

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x y = x
ifThenElse False x y = y

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth n = 31

validDate :: Int -> Int -> Bool
validDate d m = m >= 1 && m <= 12 && daysInMonth m >= d && d >= 1

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = sumNumbers (n - 1) + n

-- sumNumbers :: Int -> Int
-- sumNumbers x
--   | x == 0 = 0
--   | otherwise = sumNumbers (x - 1) + x

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = sumSquares (n - 1) + n ^ 2

-- sumSquares :: Int -> Int
-- sumSquares n
--   | n == 0 = 0
--   | otherwise = sumSquares (n - 1) + n ^ 2

power :: Int -> Int -> Int
power v1 0 = 1
power v1 1 = v1
power v1 v2 = power v1 (v2 - 1) * v1

-- power :: Int -> Int -> Int
-- power v1 v2
--   | v2 == 0 = 1
--   | v2 == 1 = v1
--   | otherwise = power v1 (v2 - 1) * v1

sumFromTo :: Int -> Int -> Int
sumFromTo v1 v2 
    | v1 > v2 = 0
    | otherwise = sumFromTo (v1 + 1) v2 + v1

-- sumFromTo :: Int -> Int -> Int
-- sumFromTo v1 v2
--   | v1 > v2 = 0
--   | otherwise = sumFromTo (v1 + 1) v2 + v1

gcd :: Int -> Int -> Int
gcd v1 0 = v1
gcd v1 v2 = gcd v2 (v1 `mod` v2)

-- gcd :: Int -> Int -> Int
-- gcd v1 v2
--   | v2 == 0 = v1
--   | otherwise = gcd v2 (v1 `mod` v2)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

-- findRoot :: Int -> Int -> Int
-- findRoot n s | s * s <= n = s
-- findRoot n s = findRoot n (s - 1)

findRoot :: Int -> Int -> Int
findRoot n s 
  | s * s <= n = s
  | otherwise = findRoot n (s - 1)