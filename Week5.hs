{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head (x : _) = x

tail (_ : xs) = xs

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
  | x == ' ' = 1 + countSpaces xs
  | otherwise = countSpaces xs

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x : xs) (y : ys)
  | x <= y = x : mergeLists xs (y : ys)
  | otherwise = y : mergeLists (x : xs) ys

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x : xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x : x : xs

rotate :: [a] -> [a]
rotate (x : y : xs) = y : x : xs
rotate xs = xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = listLength xs + 1

multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = multAll xs * x

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x : xs) = andAll xs && x

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x : xs) = orAll xs || x

countIntegers :: Int -> [Int] -> Int
countIntegers y [] = 0
countIntegers y (x : xs)
  | x == y = countIntegers y xs + 1
  | otherwise = countIntegers y xs

removeAll :: Int -> [Int] -> [Int]
removeAll y [] = []
removeAll y (x : xs)
  | x == y = removeAll y xs
  | otherwise = x : removeAll y xs

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst y (x : xs)
  | x == y = x : removeAll y xs
  | otherwise = x : removeAllButFirst y xs

listMarks :: String -> [(String, Int)] -> [Int]
listMarks _ [] = []
listMarks y ((n, m) : xs)
  | y == n = m : listMarks y xs
  | otherwise = listMarks y xs

sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x : y : xs)
  | x <= y = sorted (y : xs)
  | otherwise = False

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix (x : xs) (y : ys)
  | x == y = prefix xs ys
  | otherwise = False

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence xs (y : ys) = prefix xs (y : ys) || subSequence xs ys