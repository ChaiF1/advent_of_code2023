import Data.List.Split (splitOn)
import Data.List (sort, sortBy, nub, partition)

main = do
    contents <- readFile "day7_data"
    let parsedContents = map (\(hand,bid) -> (handValue hand, bid)) (parser contents)
        sorter = \(hand1, _) (hand2, _) -> listSorter hand1 hand2
        zippedContents = zip ([1..]) (sortBy (sorter) (parsedContents))
    print $ sum $ map (winningsCalc) (zippedContents)
--    print $ filter (\(x:xs) -> 1 `elem` xs) (map (\(rank, (hand, bid)) -> hand) zippedContents)


--FOR QUESTION 1 CHANGE ValueConverterTwo to ValueConverter!
parser :: String -> [([Int], Int)]
parser a = 
     let input = map (splitOn " ") (lines a)
         handConverter = \[a,b] -> (map (valueConverterTwo) a, read b)
     in map handConverter input

valueConverter :: Char -> Int
valueConverter 'A' = 14
valueConverter 'K' = 13
valueConverter 'Q' = 12
valueConverter 'J' = 11
valueConverter 'T' = 10
valueConverter '9' = 9
valueConverter '8' = 8
valueConverter '7' = 7
valueConverter '6' = 6
valueConverter '5' = 5 
valueConverter '4' = 4
valueConverter '3' = 3
valueConverter '2' = 2

valueConverterTwo :: Char -> Int
valueConverterTwo 'A' = 14
valueConverterTwo 'K' = 13
valueConverterTwo 'Q' = 12
valueConverterTwo 'T' = 10
valueConverterTwo '9' = 9
valueConverterTwo '8' = 8
valueConverterTwo '7' = 7
valueConverterTwo '6' = 6
valueConverterTwo '5' = 5 
valueConverterTwo '4' = 4
valueConverterTwo '3' = 3
valueConverterTwo '2' = 2
valueConverterTwo 'J' = 1


handValue :: [Int] -> [Int]
handValue a
    | a == [1,1,1,1,1] = [7, 1, 1, 1, 1, 1]
    | countUniqueValues ([]) (jokerReplacer a) == [5] = 7 : a
    | countUniqueValues ([]) (jokerReplacer a) == [1,4] = 6 : a
    | countUniqueValues ([]) (jokerReplacer a) == [2,3] = 5 : a
    | countUniqueValues ([]) (jokerReplacer a) == [1,1,3] = 4 : a
    | countUniqueValues ([]) (jokerReplacer a) == [1,2,2] = 3 : a
    | countUniqueValues ([]) (jokerReplacer a) == [1,1,1,2] = 2 : a
    | otherwise = 1 : a


countUniqueValues :: [Int] -> [Int] -> [Int]
countUniqueValues acc [] = sort acc
countUniqueValues acc list@(x:xs) = 
    let (isHead, isNotHead) = partition (\a -> a == x) (list)
    in countUniqueValues (length (isHead) : acc) (isNotHead)

listSorter :: [Int] -> [Int] -> Ordering
listSorter [] [] = EQ
listSorter (x:xs) (y:ys)
    | x > y = GT
    | x < y = LT
    | x == y = listSorter (xs) (ys) 

winningsCalc :: (Int, ([Int], Int)) -> Int
winningsCalc (rank, (_, bid)) = rank*bid

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs)
  | x == old   = new : replace old new xs
  | otherwise  = x : replace old new xs

jokerReplacer :: [Int] -> [Int]
jokerReplacer hand 
  | mostFrequentValue hand == 1 = replace (1) (mostFrequentValue (filter (\a -> a /= 1) (hand) )) (hand)
  | otherwise = replace (1) (mostFrequentValue hand) (hand)

countFrequency :: (Eq a) => [a] -> a -> Int
countFrequency xs x = length (filter (\num -> num == x) xs) 

enumerator :: (Int, Int) -> Int
enumerator (a,b) = 100*a+b

mostFrequentValue :: [Int] -> Int
mostFrequentValue xs = snd $ last $ sortBy (\a b -> compare (enumerator a) (enumerator b)) (map (\x -> (countFrequency xs x, x)) (xs))
