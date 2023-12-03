import Data.List
import Data.Maybe (mapMaybe, fromJust)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, isDigit, isSpace)

main = do
    contents <- readFile "day2_data"
    let parsedContents = map (removeSpaces) (splitOn "\n" contents)
    let parsedContents2 = map (fromJust . dropUntil ":") (parsedContents)
    let baglist = map (bagConstructor) (map (splitOn ";") parsedContents2)
    let possible =  map (all bagChecker) (baglist)
    print $ sumIfSecondTrue (zip ([1..]) (possible))
    let minbags = map (minBagFinder) (baglist)
    print $ sum $ map (powerBag) (minbags)

-- Red Blue Green
data Bag = Bag Int Int Int deriving (Show)

removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

extractColor :: String -> String
extractColor string = dropWhile (isDigit) (string)

extractNumber :: String -> Int
extractNumber string = read $ takeWhile (isDigit) (string)

bagAdder :: Bag -> Bag -> Bag
bagAdder (Bag red1 blue1 green1) (Bag red2 blue2 green2) = Bag (red1+red2) (blue1+blue2) (green1+green2)

bagMin :: Bag -> Bag -> Bag
bagMin (Bag red1 blue1 green1) (Bag red2 blue2 green2) = Bag (max red1 red2) (max blue1 blue2) (max green1 green2)

bagConverter :: String -> Bag
bagConverter string = 
    let digit = extractNumber string
        color = extractColor string
    in case color of
        "red" -> Bag (digit) (0) (0)
        "blue" -> Bag (0) (digit) (0)
        "green" -> Bag (0) (0) (digit)

bagConstructor :: [String] -> [Bag]
bagConstructor xs = map (revealAnalyzer) (xs)

revealAnalyzer :: String -> Bag
revealAnalyzer a = 
    let list = splitOn "," a
        bags = map (bagConverter) (list)
    in foldl (\acc x -> bagAdder acc x) (Bag 0 0 0) bags

dropUntil :: String -> String -> Maybe String
dropUntil find string = case findString find string of 
    Nothing -> Nothing
    (Just x) -> Just (drop (x+1) (string))

bagChecker :: Bag -> Bool
bagChecker (Bag a b c) = (a<=12)&&(b<=14)&&(c<=13)

sumIfSecondTrue :: [(Int, Bool)] -> Int
sumIfSecondTrue xs = foldl (\acc (num, condition) -> if condition then acc + num else acc) 0 xs

minBagFinder :: [Bag] -> Bag
minBagFinder xs = foldl (bagMin) (Bag 0 0 0) xs

powerBag :: Bag -> Int
powerBag (Bag a b c) = a*b*c