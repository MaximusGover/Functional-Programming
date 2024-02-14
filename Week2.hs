import Distribution.Simple.Setup (trueArg)

heartMonitor :: Int -> Int -> String
heartMonitor age bpm
  | age > 80 && bpm > 100 = "High heart rate for 81+!"
  | age > 60 && bpm > 130 = "High heart rate for 61-80!"
  | age > 40 && bpm > 140 = "High heart rate for 41-60!"
  | age > 20 && bpm > 155 = "High heart rate for 21-40!"
  | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
  | otherwise = "Normal heart rate"

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
  where
    area = pi * (fromIntegral diameter / 2) ^ 2
    toppingCalories
      | toppings == "pepperoni" = 6
      | toppings == "tuna" = 4
      | toppings == "veggie" = 2.5
      | otherwise = 0

absolute :: Int -> Int
absolute num
  | num >= 0 = num
  | otherwise = num * (-1)

sign :: Int -> Int
sign num
  | num > 0 = 1
  | num == 0 = 0
  | otherwise = -1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual v1 v2 v3
  | v1 == v2 && v1 == v3 = 3
  | v1 == v2 || v1 == v3 || v2 == v3 = 2
  | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths s1 s2 s3 = mult s1 + mult s2 + mult s3
  where
    mult num = sqrt (2 * num ^ 2)

taxiFare :: Int -> Float
taxiFare dist
  | dist > 10 = 7.2 + (fromIntegral dist - 10) * 0.3
  | otherwise = 2.2 + fromIntegral dist * 0.5

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage v1 v2 v3
  | v1 == v2 && v1 == v3 = 0
  | v1 > average && v2 > average || v1 > average && v3 > average || v2 > average && v3 > average = 2
  | otherwise = 1
  where
    average = (v1 + v2 + v3) `div` 3

validDate :: Int -> Int -> Bool
validDate d m
  | d <= 28 && m == 2 = True
  | d <= 31 && odd m = True
  | d <= 30 && even m = True
  | otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m == 2 && y `mod` 4 == 0 = 29
  | m == 2 = 28
  | odd m = 31
  | otherwise = 30

{-
3 + 5 + 7 = 15
8 + 4 + 2 = 14

(1 - 4 /= 0) (True) and (1 - 2 /= 0) (True) and (4 - 2 /= 0) (True) = True
(1 - 7 /= 0) (True) and (1 - 7 /= 0) (True) and (7 - 7 /= 0) (False) = False

If (3 = 5) and (3 = 2) then 3 (False)
Else If (3 = 5) or (3 = 2) or (5 = 2) then 2 (False)
Else 0 (True)

If (5 = 2) and (5 = 5) then 3 (False)
Else If (5 = 2) or (5 = 5) or (2 = 5) then 2 (True)
Else 0
-}
