--1

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h

--2

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c
