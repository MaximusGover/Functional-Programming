import Data.Char (intToDigit)
import Text.Printf (printf)

-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
  deriving (Eq, Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
  | m1 >= m2 = s1
  | otherwise = s2

-- Shapes algebraic type
data Shape
  = Circle Float
  | Rectangle Float Float
  deriving (Show, Eq)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
  deriving (Show)

data Building
  = Name String
  | Number Int
  deriving (Show)

-- Binary tree algebraic type
data Tree
  = Null
  | Node Int Tree Tree
  deriving (Show, Eq)

-- Binary tree test data
testTree =
  Node
    20
    (Node 3 (Node 12 Null Null) (Node 7 Null Null))
    (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =
  Node
    5
    (Node 1 Null Null)
    (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

type Make = String

type Model = String

type HorsePower = Int

type Price = Float

data EngineType = Petrol | Diesel | Electric
  deriving (Show, Eq)

data Engine = Engine EngineType HorsePower
  deriving (Show)

data CarName = CarName Make Model
  deriving (Show)

data Car = Car CarName Engine Price
  deriving (Show)

testCars :: [Car]
testCars =
  [ Car (CarName "Ford" "Fiesta") (Engine Petrol 55) 10000.0,
    Car (CarName "Ford" "Focus") (Engine Diesel 85) 15000.0,
    Car (CarName "Vauxhall" "Corsa") (Engine Petrol 55) 8000.0,
    Car (CarName "Vauxhall" "Astra") (Engine Diesel 81) 12000.0,
    Car (CarName "Vauxhall" "Astra") (Engine Diesel 96) 14000.0,
    Car (CarName "VolksWagen" "Golf") (Engine Electric 81) 20000.0
  ]

getMake :: Car -> Make
getMake (Car (CarName make _) _ _) = make

getModel :: Car -> Model
getModel (Car (CarName _ model) _ _) = model

getPrice :: Car -> Price
getPrice (Car _ _ price) = price

totalPrice :: [Car] -> Float
totalPrice [] = 0
totalPrice (Car _ _ price : cs) = price + totalPrice cs

filterByMake :: String -> [Car] -> [Car]
filterByMake manufacturer = filter (\c -> getMake c == manufacturer)

updatePriceAt :: Int -> Float -> [Car] -> [Car]
updatePriceAt index price cars = take index cars ++ [newCar] ++ drop (index + 1) cars
  where
    newCar = updatePrice price (cars !! index)

updatePrice :: Float -> Car -> Car
updatePrice newPrice (Car name engine _) = Car name engine newPrice

formatCar :: [Car] -> Int -> String
formatCar [] _ = ""
formatCar cars i =
  printf
    "%d- %s %s costs %.2f pounds"
    (i + 1)
    (getMake c)
    (getModel c)
    (getPrice c)
  where
    c = cars !! i

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Eq)

data Season = Spring | Summer | Autumn | Winter
  deriving (Show, Eq)

season :: Month -> Season
season m
  | m `elem` [December, January, February] = Winter
  | m `elem` [March, April, May] = Spring
  | m `elem` [June, July, August] = Summer
  | m `elem` [September, October, November] = Autumn

type Year = Int

numberOfDays :: Month -> Year -> Int
numberOfDays m y
  | m == February && y `mod` 4 == 0 = 29
  | m == February = 28
  | m `elem` [January, March, May, July, September, October, December] = 31
  | otherwise = 30

data Point = Point {x :: Float, y :: Float}
  deriving (Show, Eq)

data PositionedShape = PositionedShape Shape Point
  deriving (Show, Eq)

move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) dx dy = PositionedShape shape (Point (x + dx) (y + dy))

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ left right) = 1 + numberOfNodes left + numberOfNodes right

isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember x (Node y left right)
  | x == y = True
  | x < y = isMember x left
  | otherwise = isMember x right

leaves :: Tree -> [Int]
leaves Null = []
leaves (Node value Null Null) = [value]
leaves (Node _ left right) = leaves left ++ leaves right

testBinaryTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node value left right) = inOrder left ++ [value] ++ inOrder right

insert :: Int -> Tree -> Tree
insert x Null = Node x Null Null
insert x (Node y left right)
  | x == y = Node y left right
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

listToSearchTree :: [Int] -> Tree
listToSearchTree = foldr insert Null . reverse

binaryTreeSort :: [Int] -> [Int]
binaryTreeSort = inOrder . listToSearchTree