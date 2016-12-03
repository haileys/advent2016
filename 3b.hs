import Data.List
import Data.List.Split

data Triangle = Triangle Int Int Int

isTriangle :: Triangle -> Bool
isTriangle (Triangle a b c) = a + b > c && a + c > b && b + c > a

main = do
  input <- getContents
  print $ length
        $ filter isTriangle
        $ map (\[a,b,c] -> Triangle a b c)
        $ concat
        $ map transpose
        $ chunksOf 3
        $ map (map read . words)
        $ lines input
