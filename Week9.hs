import Data.Time (Day)
import Data.List (find)

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

type Date = Day

type Title = String
type Description = String

type Reviewed = Bool

data DiaryEntry = DiaryEntry Date Title Description Reviewed
  deriving (Show, Read)

diary :: [DiaryEntry]
diary = []

getDate :: IO Date
getDate = do
  input <- getLine
  return (read input :: Date)

addEntry :: IO ()
addEntry = do
    putStrLn "Enter the date:"
    date <- getDate
    putStrLn "Enter the title:"
    title <- getLine
    putStrLn "Enter the description:"
    description <- getLine
    let entry = DiaryEntry date title description False
    writeEntryToFile entry
    putStrLn "Entry added to diary."
    wait <- getLine
    displayMenu

writeEntryToFile :: DiaryEntry -> IO ()
writeEntryToFile entry = do
    let filename = "diary.txt"
    appendFile filename (show entry ++ "\n")

deleteEntry :: IO ()
deleteEntry = do
    putStrLn "Enter the date of the entry to delete:"
    date <- getDate
    contents <- readFile "diary.txt"
    let entries = map read (lines contents) :: [DiaryEntry]
    let updatedEntries = filter (\(DiaryEntry d _ _ _) -> d /= date) entries
    writeFile "diary.txt" (unlines (map show updatedEntries))
    putStrLn "Entry deleted from diary."
    _wait <- getLine
    displayMenu

displayDiary :: IO ()
displayDiary = do
    putStrLn "Diary Entries:"
    contents <- readFile "diary.txt"
    let entries = map read (lines contents) :: [DiaryEntry]
    mapM_ displayEntry entries
    wait <- getLine
    displayMenu

displayEntry :: DiaryEntry -> IO ()
displayEntry (DiaryEntry date title description reviewed) = do
    putStrLn $ "Title: " ++ title
    putStrLn description
    putStrLn $ "Reviewed: " ++ show reviewed ++ " added " ++ show date

findEntry :: Date -> [DiaryEntry] -> Maybe DiaryEntry
findEntry d = find (\(DiaryEntry date _ _ _) -> date == d)

displayMenu :: IO ()
displayMenu = do
  putStrLn "Welcome to the Diary App!"
  putStrLn "1. Add Entry"
  putStrLn "2. Delete Entry"
  putStrLn "3. Display Diary"
  putStrLn "4. Exit"
  choice <- getLine
  case choice of
    "1" -> addEntry
    "2" -> deleteEntry
    "3" -> displayDiary
    "4" -> putStrLn "Exiting..."
    _   -> putStrLn "Invalid choice, please try again."

main :: IO ()
main = do
    displayMenu