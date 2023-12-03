import Data.List.Split (splitOn)
import Data.Char (isDigit)

main = do
    contents <- readFile "day3_data"
    let parsedContents = splitOn ("\n") (contents)
        symbolMatrix = map (map isSymbol) (parsedContents)
        n = length (head parsedContents)
        digitInfo = map (locationConverter n) (digitFinder (concat parsedContents) ([]) (0))
        checkedDigits = zip (map (\xs -> digitChecker xs symbolMatrix) (digitInfo)) (digitInfo)
    print $ sumIfTrue checkedDigits
    --print $ map (surroundingPoints n) digitInfo
    --print $ starChecker n x y parsedContents
    print $ div (sum $ [starChecker n x y parsedContents | x <- digitInfo, y <- digitInfo, x /= y]) (2)

isDot :: Char -> Bool
isDot '.' = True
isDot _ = False

isSymbol :: Char -> Bool
isSymbol a = 
    if (isDigit a) || (isDot a)
        then False
        else True

digitProcessor :: String -> (Int, Int)
digitProcessor string = (read $ takeWhile (isDigit) (string), length $ takeWhile (isDigit) (string))

locationConverter :: Int -> (Int, Int, Int) -> (Int, Int, (Int, Int)) 
locationConverter n (a, b, loc) = 
    let rowIndex = loc `div` n
        colIndex = loc `mod` n
    in (a, b, (rowIndex, colIndex))

-- digitList is (digit, length of digit, location)
digitFinder :: String -> [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
digitFinder [] digitList _ = digitList 
digitFinder str@(x:xs) digitList location = 
    if isDigit x
        then let (digit, digitLength) = digitProcessor str
        in digitFinder (drop (digitLength) str) ((digit, digitLength, location) : digitList) (location + digitLength)
        else digitFinder (xs) (digitList) (location+1)

extractRange :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
extractRange matrix (startRow, startCol) (endRow, endCol) =
  [ [matrix !! i !! j | j <- [startCol..endCol]] | i <- [startRow..endRow]]

digitChecker :: (Int, Int, (Int, Int)) -> [[Bool]] -> Bool
digitChecker (digit, digitLength, (rowIndex, colIndex)) symbolMatrix = 
    let n = (length symbolMatrix)-1
        topLeft = (max 0 (rowIndex-1), max 0 (colIndex-1))
        bottomRight = (min n (rowIndex+1), min n (colIndex+digitLength))
        surroundingSymbols = extractRange symbolMatrix topLeft bottomRight
    in any id (concat surroundingSymbols)

sumIfTrue :: [(Bool, (Int, Int, (Int, Int)))] -> Int
sumIfTrue xs = foldl (\acc (condition, (digit, _, (_,_))) -> if condition then acc + digit else acc ) 0 xs

isStar :: [String] -> (Int, Int) -> Bool
isStar matrix (x,y) =
    if matrix !! x !! y == '*'
        then True
        else False

surroundingPoints :: Int -> (Int, Int, (Int, Int)) -> [(Int, Int)]
surroundingPoints n (_, digitLength, (rowIndex, colIndex)) =
    let startRow = max 0 (rowIndex-1)
        endRow = min (n-1) (rowIndex+1)
        startCol = max 0 (colIndex-1)
        endCol = min (n-1) (colIndex + digitLength)
    in [(x,y) | x <- [startRow .. endRow ], y <- [startCol .. endCol] ]

starChecker :: Int -> (Int, Int, (Int, Int)) -> (Int, Int, (Int, Int)) -> [String] -> Int 
starChecker n (digit1, digitLength1, (rowIndex1, colIndex1)) (digit2, digitLength2, (rowIndex2, colIndex2)) matrix = 
    let surroundingPoints1 = surroundingPoints n (digit1, digitLength1, (rowIndex1, colIndex1))
        surroundingPoints2 = surroundingPoints n (digit2, digitLength2, (rowIndex2, colIndex2))
        commonPoints = intersection surroundingPoints1 surroundingPoints2
        starPoints = [ x | x <- commonPoints, isStar matrix x]
    in if starPoints == []
        then 0
        else digit1*digit2

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]