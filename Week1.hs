--

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

--

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

--1

timesTen :: Int -> Int
timesTen x = x * 10

--2

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

--3
areaOfCircle :: Float -> Float
areaOfCircle r = r * r * pi

--4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = h * areaOfCircle r

--5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2) * (y1 - y2) + (x1 - x2) * (x1 - x2))

--6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

--7
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

--8
isEven :: Int -> Bool
isEven x = divisibleBy x 2

--9
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

--10
absolute :: Int -> Int
absolute x = if x < 0 then (-x) else x

--  unary - on x
-- negate 

--cmd /k "cd C:\Users\up2267744\OneDrive - University of Portsmouth\Desktop\Haskell & C:\ghcup\bin\ghci Week1.hs"