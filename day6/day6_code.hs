import Data.List.Split (splitOn)

main = do
    let (x1,x2) = abc (-1) (60947882) (-475213810151650)
    print $ (floor x2 - ceiling x1)+1

abc :: Double -> Double -> Double -> (Double, Double)
abc a b c = ( (-b + sqrt (b^2-4*a*c))/(2*a), (-b - sqrt (b^2-4*a*c))/(2*a))