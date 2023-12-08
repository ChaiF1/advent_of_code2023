import Data.List (isInfixOf, sortBy)
import Data.List.Split (splitOn, splitWhen)

main = do
    content <- readFile "day5_data"
    let parsedContents = filter (\x -> x /= "") (lines content)
        initialSeeds = stringToIntList $ drop 7 (head parsedContents)
        stringMaps = tail $ splitWhen (\a -> "map" `isInfixOf` a) (tail parsedContents)
        maps = map (sortBySecondElement . map stringToIntList) (stringMaps)
    print $ minimum $ map (seedIterator maps) initialSeeds
    print $ seedSetUp initialSeeds
    print $ maps

stringToIntList :: String -> [Int]
stringToIntList a = map (read) (splitOn " " a)

seedIterator :: [[[Int]]] -> Int -> Int
seedIterator [] seed = seed
seedIterator (x:xs) seed = seedIterator (xs) (chapterIterator x seed)

--Destination, source, range
chapterIterator :: [[Int]] -> Int -> Int
chapterIterator [] seed = seed
chapterIterator ([startDestination, startSource, range]:xs) seed =
    if (startSource <= seed) && (seed <= startSource+range-1)
        then (startDestination + seed - startSource)
        else chapterIterator xs seed

sortByFirstElement :: (Ord a) => [[a]] -> [[a]]
sortByFirstElement = sortBy (\x y -> compare (head x) (head y))

sortBySecondElement :: (Ord a) => [[a]] -> [[a]]
sortBySecondElement = sortBy (\x y -> compare (x !! 1) (y !! 1))

seedSetUp :: [Int] -> [[Int]]
seedSetUp [] = []
seedSetUp (x:y:xs) = sortByFirstElement ([x,y] : seedSetUp (xs))

chapterIterator :: [[Int]] -> [[Int]] -> [[Int]]
chapterIterator [] seed = seed
chapterIterator
chapterIterator maps@([startDestination, startSource, mapRange]:xs) seeds@([startSeed, seedRange]:ys)
    | startSource + mapRange - 1 < startSeed = chapterIterator (xs) (seeds)
    | (startSource < startSeed) && (startSource+mapRange-1  <= startSeed+seedRange-1) =
    | (startSeed <= startSource) && (startSource+mapRange-1  <= startSeed+seedRange-1) =
    | (startSource <= startSeed) && (startSeed+seedRange-1 <= startSource+mapRange-1) =
    | (startSeed <= startSource) && (startSeed+seedRange-1 < startSource+mapRange-1) =
    | (startSeed+seedRange-1 < startSource) =