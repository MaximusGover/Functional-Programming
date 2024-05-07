type Make = String

type Model = String

type NumberPlate = String

data CarName = CName Make Model
  deriving (Show)

type Mileage = Int

type MotYear = Int

type Mots = [MotYear]

data Car = Car NumberPlate CarName Mileage Mots
  deriving (Show)

testCars :: [Car]
testCars =
  [
    Car "AB12 CDE" (CName "Ford" "Fiesta") 71650 [2020, 2021],
    Car "CD34 EFG" (CName "Ford" "Focus") 10354 [2017, 2018, 2019, 2020, 2021, 2022],
    Car "EF56 GHI" (CName "Ford" "Mondeo") 35465 [2019, 2020, 2021, 2022, 2023],
    Car "GH78 IJK" (CName "Vauxhall" "Corsa") 94759 [2020, 2021, 2022],
    Car "IJ90 KLM" (CName "Vauxhall" "Astra") 3964 [2021, 2022],
    Car "KL12 MNO" (CName "Vauxhall" "Vectra") 99801 [2020, 2021, 2022, 2023],
    Car "MN34 OPQ" (CName "Vauxhall" "Vectra") 5554 [2020, 2021, 2022, 2023],
    Car "OP56 QRST" (CName "Volkswagen" "Golf") 65168 [2021, 2022],
    Car "QR78 STU" (CName "Volkswagen" "Golf") 45630 [2020, 2021, 2022, 2023],
    Car "ST90 UVW" (CName "Volkswagen" "Passat") 36325 [2018, 2019]
  ]
