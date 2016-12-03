import Data.List

data Triangle = Triangle Int Int Int

readTriangle :: String -> Triangle
readTriangle s =
  let [a, b, c] = map read $ words s
  in Triangle a b c

isTriangle :: Triangle -> Bool
isTriangle (Triangle a b c) = a + b > c && a + c > b && b + c > a

main = do
  input <- getContents
  print $ length
        $ filter isTriangle
        $ map readTriangle
        $ lines input
