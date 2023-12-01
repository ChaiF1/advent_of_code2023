import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Data.Char (digitToInt, isLetter)

main = do
    contents <- readFile "day1_data"
    print $ sum $ map (result) (parser contents)
    print $ sum $ map (result . replaceSubstrings (["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])) (parser contents)

parser :: String -> [String]
parser = splitOn "\n"

firstDigit :: String -> Int
firstDigit = digitToInt . head . dropWhile (isLetter)

result :: String -> Int
result a =
    let tens = firstDigit a
        ones = firstDigit (reverse a)
    in 10*tens+ones

textToInt :: String -> String
textToInt "one" = "1" ++ [last "one"]
textToInt "two" = "2" ++ [last "two"]
textToInt "three" = "3" ++ [last "three"]
textToInt "four" = "4" ++ [last "four"]
textToInt "five" = "5" ++ [last "five"]
textToInt "six" = "6" ++ [last "six"]
textToInt "seven" = "7" ++ [last "seven"]
textToInt "eight" = "8" ++ [last "eight"]
textToInt "nine" = "9" ++ [last "nine"]

applyAll :: [a -> b] -> a -> [b]
applyAll [] _ = []
applyAll (f:fs) x = (f x) : (applyAll fs x)

replaceSubstrings :: [String] -> String -> String
replaceSubstrings _ [] = []
replaceSubstrings find str@(x:xs)
    | any (\sub -> sub `isPrefixOf` str) (find) = replacePrefix find str
    | otherwise = x : replaceSubstrings find xs

replacePrefix :: [String] -> String -> String
replacePrefix [] str = str
replacePrefix (x:xs) str
    | x `isPrefixOf` str = textToInt (x) ++ replaceSubstrings ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ([last (x)] ++ drop (length x) str)
    | otherwise = replacePrefix xs str