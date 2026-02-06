--

heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 && bpm > 130 = "High heart rate for 61-80!"
    | age > 40 && bpm > 140 = "High heart rate for 41-60!"
    | age > 20 && bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"

--
pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
    where
    area = pi * (fromIntegral diameter / 2) ^ 2
    toppingCalories
        | toppings == "pepperoni" = 6
        | toppings == "tuna" = 4
        | toppings == "veggie" = 2.5
        | otherwise = 0

--1
absolute :: Int -> Int
absolute x
    | x < 0 = -x
    | otherwise = x

--2
sign :: Int -> Int
sign x
    | x < 0 = -1
    | x > 0 = 1
    | otherwise = 0

--3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z 
    | x == y && x == z = 2
    | x == y || y == z || x == z = 1
    | otherwise = 0

--4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths l1 l2 l3 = sumDiagonal l1 + sumDiagonal l2 + sumDiagonal l3
    where
    sumDiagonal l = sqrt (2 * l ^ 2)

--5
taxiFare :: Int -> Float
taxiFare km = 2.20 + distPrice (fromIntegral km)
    where
    distPrice km 
        | km > 10 = 5 + (km - 10) * 0.30
        | otherwise = 0.50 * km

--6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = aboveAverage (fromIntegral x) avg + aboveAverage (fromIntegral y) avg + aboveAverage (fromIntegral z) avg
    where
        avg = averageThree x y z
        averageThree x y z = fromIntegral(x + y + z) / 3    
        aboveAverage x avg = if x > avg then 1 else 0

--7
validDate :: Int -> Int -> Bool
validDate day month
    | month == 2 = day <= 28
    | month <= 7 = comp day month 30 31
    | otherwise = comp day month 31 30
    where
        comp day month v1 v2      
            | even month = day <= v1
            | otherwise = day <= v2

--8
daysInMonth :: Int -> Int -> Int
daysInMonth month year = validDate month (if mod year 4 == 0 then 1 else 0)
    where
        validDate month leap
            | month == 2 = 28 + leap
            | month <= 7 = comp month 30 31
            | otherwise = comp month 31 30
            where
                comp month v1 v2      
                    | even month = v1
                    | otherwise = v2

