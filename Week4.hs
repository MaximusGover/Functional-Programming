import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
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

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], even i]

-- sumEvenNumbersBetween x y
--   | x > y = 0
--   | mod x 2 == 0 = x + sumEvenNumbersBetween (x + 2) y
--   | otherwise = sumEvenNumbersBetween (x + 1) y

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
  where
    sumMarks = sum [mk | (_, mk) <- stmks]
    numberOfStudents = length stmks

sumDifference :: Int -> Int -> (Int, Int)
sumDifference v1 v2 = (v1 + v2, v1 - v2)

-- sumDifference v1 v2 = (fromIntegral sum, fromIntegral diff)
--   where
--     sum = v1 + v2
--     diff = v1 - v2

grade :: StudentMark -> Char
grade (_, m)
  | m > 100 || m < 0 = error "Not a valid mark"
  | m > 70 = 'A'
  | m > 60 = 'B'
  | m > 50 = 'C'
  | m > 40 = 'D'
  | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (s, m)
  | m > 100 || m < 0 = error "Not a valid mark"
  | m > 40 = (s, 40)
  | otherwise = (s, m)

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [n ^ 2 | n <- firstNumbers n]

capitalise :: String -> String
capitalise inp = [toUpper x | x <- inp]

onlyDigits :: String -> String
onlyDigits inp = [x | x <- inp, isDigit x]

capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [capMark x| x <- stmks]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(s, grade (s, m)) | (s, m) <- stmks]

-- duplicate :: String -> Int -> String
-- duplicate str times
--   | times == 1 = str
--   | otherwise = duplicate str (times - 1) ++ str

duplicate :: String -> Int -> String
duplicate str times = concat [str | _ <- [1 .. times]]

divisors :: Int -> [Int]
divisors num = [i | i <- [1 .. num], num `mod` i == 0]

isPrime :: Int -> Bool
isPrime num = length (divisors num) == 2

-- split :: [(a, b)] -> ([a], [b])
-- split inp = (map fst inp, map snd inp)

split :: [(a, b)] -> ([a], [b])
split inp = ([x | (x, _) <- inp], [y | (_, y) <- inp])