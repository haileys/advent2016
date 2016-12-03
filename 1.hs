import Data.Complex
import Data.List.Split

data Move = LeftTurn Double | RightTurn Double

data State = State { direction :: Complex Double, position :: Complex Double }

readMove :: String -> Move
readMove ('L':n) = LeftTurn  (read n :: Double)
readMove ('R':n) = RightTurn (read n :: Double)

moveDirection :: Move -> Complex Double
moveDirection (LeftTurn _) = 0 :+ 1
moveDirection (RightTurn _) = 0 :+ (-1)

moveMagnitude :: Move -> Complex Double
moveMagnitude (LeftTurn n) = n :+ 0
moveMagnitude (RightTurn n) = n :+ 0

makeMove :: State -> Move -> State
makeMove s m =
  let newDirection = direction s * moveDirection m in
  State {
    direction = newDirection,
    position = position s + newDirection * moveMagnitude m
  }

initialState :: State
initialState = State { direction = 0 :+ 1, position = 0 :+ 0 }

taxicabDistance :: Num a => Complex a -> a
taxicabDistance c = abs (realPart c) + abs (imagPart c)

main = do
  input <- getContents
  print $ taxicabDistance
        $ position
        $ foldl makeMove initialState
        $ map readMove
        $ splitOn ", " input
