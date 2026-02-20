import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

--1

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

--2

grade :: StudentMark -> Char
grade (_, mark)
    | mark > 100 || mark < 0 = error "Not within the mark range!"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

--3

capMark :: StudentMark -> StudentMark
capMark (stu, mark)
    | mark > 100 || mark < 0 = error "Not within the mark range!"
    | mark > 40 = (stu, 40)
    | otherwise = (stu, mark)

--4

firstNumbers :: Int -> [Int]
firstNumbers x = [1 .. x]

--5

firstSquares :: Int -> [Int]
firstSquares x = [i ^ 2 | i <- [1.. x]]

--6

capitalise :: String -> String
capitalise word = [toUpper i | i <- word]