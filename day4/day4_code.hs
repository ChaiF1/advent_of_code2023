import Data.List.Split (splitOn)
import Data.Char (isDigit)

main = do
    contents <- readFile "day4_data"
    let parsedContents = map (dropWhile (not . isDigit) . drop 8) (splitOn "\n" contents)
    print $ sum $ map (problemOne) (parsedContents)
    let zippedContents = zip ([1..]) (parsedContents)
        n = length zippedContents
    print $ sum $ foldl (problemTwo) (replicate n 1) (zippedContents)

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

dropEmpties :: [String] -> [String]
dropEmpties a = filter (\x -> x /= "") (a)

problemOne ::  String -> Int
problemOne a =
    let [list1, list2] = map (dropEmpties . splitOn " ") (splitOn " | " a )
        winners = intersection list1 list2
    in div (2^(length winners)) (2)

elementWiseAddition :: [Int] -> [Int] -> [Int]
elementWiseAddition a b = zipWith (+) (a) (b)

problemTwo :: [Int] -> (Int, String) -> [Int]
problemTwo acc (cardNumber, card) = 
    let [list1, list2] = map (dropEmpties . splitOn " ") (splitOn " | " card)
        wins = length $ intersection (list1) (list2)
        n = length acc
        copyCards = (replicate (cardNumber) (0)) ++ (replicate (wins) (acc !! (cardNumber-1) )) ++ (replicate (n-cardNumber-wins) (0))
    in elementWiseAddition (copyCards) (acc)