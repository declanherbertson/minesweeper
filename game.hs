module Game where

data Cell = Empty | Clicked Int | Bomb
data Board = Array (Int, Int) Cell
data GameStatus = Start | Continue | Gameover
type NumberOfBombs = Int
type BombsLeft = Int
data GameState = GameState Board NumberOfBombs BombsLeft GameStatus


initialGameState = 42

-- this is the function that is 'minesweeper', it takes an action and a state and returns the updated state
transformGame _ game = game