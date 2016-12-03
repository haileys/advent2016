data Move = MoveLeft | MoveUp | MoveRight | MoveDown | Select

readMove :: Char -> Move
readMove 'L'  = MoveLeft
readMove 'U'  = MoveUp
readMove 'R'  = MoveRight
readMove 'D'  = MoveDown
readMove '\n' = Select

data Position = Position Int Int

makeMove :: Position -> Move -> Position
makeMove (Position 0 y) MoveLeft  = Position 0 y
makeMove (Position x 0) MoveUp    = Position x 0
makeMove (Position 2 y) MoveRight = Position 2 y
makeMove (Position x 2) MoveDown  = Position x 2
makeMove (Position x y) m =
  case m of
    MoveLeft  -> Position (x - 1) y
    MoveUp    -> Position x       (y - 1)
    MoveRight -> Position (x + 1) y
    MoveDown  -> Position x       (y + 1)

initialPosition :: Position
initialPosition = Position 1 1

applyMoves :: Position -> [Move] -> [Position]
applyMoves _ []          = []
applyMoves p (Select:ms) = p : applyMoves p ms
applyMoves p (m:ms)      = applyMoves (makeMove p m) ms

positionToKey :: Position -> Char
positionToKey (Position x y) = case (x, y) of
  (0, 0) -> '1'
  (1, 0) -> '2'
  (2, 0) -> '3'
  (0, 1) -> '4'
  (1, 1) -> '5'
  (2, 1) -> '6'
  (0, 2) -> '7'
  (1, 2) -> '8'
  (2, 2) -> '9'

main = do
  input <- getContents
  putStrLn $ map positionToKey
           $ applyMoves initialPosition
           $ map readMove input
