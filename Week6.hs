{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (* 2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (> 0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs

alwaysEven :: (Int -> Int) -> [Int] -> Bool
-- alwaysEven f xs = length (filter even (map f xs)) == length xs
-- alwaysEven f xs = length (filter (\y -> mod y 2 == 0)) == length xs
-- alwaysEven f xs = andAll (map (even . f) xs)
alwaysEven f = andAll . map (even . f)

updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
-- updatePositivesOnly _ [] = []
-- updatePositivesOnly f (x : xs)
--   | x > 0 = f x : updatePositivesOnly f xs
--   | otherwise = x : updatePositivesOnly f xs
updatePositivesOnly f = map (\x -> if x > 0 then f x else x)

mult10 :: [Int] -> [Int]
mult10 = map (10 *)

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

orAll :: [Bool] -> Bool
orAll = foldr (||) False

sumSquares :: [Int] -> Int
-- sumSquares = foldr ((+) . (^ 2)) 0
sumSquares = foldr (+) 0 . map (^ 2)

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>= 0) . filter (<= 10)

squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>= 0)

countBetween :: Float -> Float -> [Float] -> Int
countBetween lb ub = length . filter (\x -> x >= lb && x <= ub)

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f xs = length (filter (> 0) (map f xs)) == length xs
-- alwaysPositive f xs = andAll (map (\x -> f x > 0) xs)
alwaysPositive f = andAll . map (\x -> f x > 0)

productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr ((*) . sqrt) 1 (filter (>= 0) xs)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst p (x : xs)
  | p x = xs
  | otherwise = x : removeFirst p xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast p = reverse . removeFirst p . reverse

zeroToTen1 :: [Int] -> [Int]
zeroToTen1 = filter (\x -> x >= 0 && x <= 10)

alwaysPositiveA :: (Float -> Float) -> [Float] -> Bool
alwaysPositiveA f = foldr ((&&) . (> 0) . f) True

productSquareRootsB :: [Float] -> Float
productSquareRootsB = foldr (\x result -> if x >= 0 then sqrt x * result else result) 1

reverseC :: [a] -> [a]
reverseC = foldr (\x acc -> acc ++ [x]) []