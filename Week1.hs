circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen num = num * 10

sumThree :: Int -> Int -> Int -> Int
sumThree one two three = one + two + three

areaOfCircle :: Float -> Float
areaOfCircle radius = pi * (radius ^ 2)

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = areaOfCircle r * h

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2) ^ 2 + (x1 - x2) ^ 2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent v1 v2 v3 = (v1 - v2 /= 0) && (v1 - v3 /= 0) && (v2 - v3 /= 0)

divisibleBy :: Int -> Int -> Bool
divisibleBy v1 v2 = v1 `rem` v2 == 0

isEven :: Int -> Bool
isEven = even

averageThree :: Int -> Int -> Int -> Float
averageThree v1 v2 v3 = fromIntegral (v1 + v2 + v3) / 3

absolute :: Int -> Int
absolute num = if num >= 0 then num else -num