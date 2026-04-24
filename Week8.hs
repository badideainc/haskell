helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines

--1

greeting :: IO ()
greeting = do 
    putStr "What is your name? "
    name <- getLine
    putStrLn ("Hello, " ++ name)

--2

addTwoNumbers :: IO ()
addTwoNumbers = do
    putStr "Enter a number: "
    num1 <- getInt
    putStr "Enter a second number: "
    num2 <- getInt
    print (num1 + num2)

--3

copyFile :: IO ()
copyFile = do
    putStr "Enter source filename: "
    file <- getLine
    putStr "Enter destination filename: "
    destFile <- getLine
    contents <- readFile file
    writeFile destFile contents

--4

buildString :: [String] -> IO ()
buildString xs = do
    print ("Line is now " ++ show  (reverse xs))
    putStr "Enter a line: "
    str <- getLine
    if str == "" then
        return ()
    else
        buildString (str : xs)

listBuilder :: IO ()
listBuilder = do
    putStr "Enter a line: "
    str <- getLine
    buildString [str]

--5
sumInts :: Int -> Int -> IO ()
sumInts total n = do
    if n == 0 then
        print total
    else do
        putStr "Enter a number: "
        num <- getInt
        if num == 0 then
            return ()
        else
            sumInts (total + num) (n - 1)

sumNInts :: IO ()
sumNInts = do
    putStr "How many numbers do you want to sum? "
    n <- getInt
    sumInts 0 n

--6
--a

addWord :: String -> [String] -> [String]
addWord str xs = xs ++ [str]

--b

wordsToString :: [String] -> String
wordsToString = foldr (\w xs -> w ++ "\n" ++ xs) ""

--c

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n = foldr (\w acc -> if length w == n then w : acc else acc) []

--d
main :: IO ()
main = do
    contents <- readFile "words.txt"
    let xs = (read contents :: [String])
    let newXs = addWord "Lemon" xs
    putStrLn (wordsToString newXs)
    putStr "Enter a length: "
    lengthWord <- getInt
    putStrLn (wordsToString (wordsOfLength lengthWord newXs))
    writeFile "words.txt" (show newXs)