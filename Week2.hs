import Control.Monad (when)
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

sign :: Int -> Int
sign x
    | x < 0 = -1
    | x > 0 = 1
    | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z 
    | x == y && x == z = 2
    | x == y || y == z || x == z = 1
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths l1 l2 l3 = sumDiagonal l1 + sumDiagonal l2 + sumDiagonal l3
    where
    sumDiagonal l = sqrt (2 * l ^ 2)