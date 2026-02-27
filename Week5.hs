{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

-- countSpaces :: String -> Int
-- countSpaces [] = 0
-- countSpaces (x: xs) = if head xs == ' ' then 1 else 0 + countSpaces xs

--1
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

--2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x: xs) = x: x: xs

--3
rotate :: [a] -> [a]
rotate [] = []
rotate (x1: x2: xs) = x2: x1: xs

--4
listLength :: [a] -> Int
listLength [] = 0
listLength (x: xs) = 1 + listLength xs

--5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x: xs) = x * multAll xs

--6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x: xs) = x && andAll xs

--7
orAll :: [Bool] -> Bool
orAll [] = False
orAll (x: xs) = x || orAll xs

--8
countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers n (x: xs) = (if n == x then 1 else 0) + countIntegers n xs

--9
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x: xs) = if n == x then removeAll n xs else x: removeAll n xs

--10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst n (x: xs) = x: removeAll n xs

--11
type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

listMarks:: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks stu (x: xs) = if stu == fst x then snd x: listMarks stu xs else listMarks stu xs

--12
sorted :: [Int] -> Bool
sorted [] = True
sorted (x1: x2: xs) = x1 < x2 && sorted xs

--13
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x: xs) (y:ys) = x == y && prefix xs ys

--14
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence xs ys = prefix xs ys || subSequence xs (tail ys)