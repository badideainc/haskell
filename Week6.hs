{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char
import Data.List (intersect)

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

alwaysEven :: (Int -> Int) -> [Int] -> Bool
alwaysEven _ [] = True
alwaysEven f (x:xs) = even (f x) && alwaysEven f xs

--1

mult10 :: [Int] -> [Int]
mult10 = map (*10)

--2

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

--3

orAll :: [Bool] -> Bool
orAll = foldr (||) False

--4

sumSquares :: [Int] -> Int
sumSquares x = sum ( map (^2) x)

--5

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>0) . filter (<10)

--6

squareRoots :: [Float] -> [Float]
squareRoots = filter (>=0) . map sqrt

--7

countBetween :: Float -> Float -> [Float] -> Int
countBetween a b = length . filter (>=a) . filter(<=b) 

--8

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f x = length ( filter (> 0) ( map f x)) == length x