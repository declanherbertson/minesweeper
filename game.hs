module Game where

import Data.Array
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set

import System.Random

data Cell = Empty | Clicked Int | Bomb
data Board = Array (Int, Int) Cell
data GameStatus = Start | Continue | Gameover
type NumberOfBombs = Int
type NumberTilesOpened = Int
data GameState = GameState Board NumberOfBombs NumberTilesOpened GameStatus


isBombHelper :: Int-> Int -> Board -> Int -> Int -> Int
isBombHelper x y board w h 
	| (x >= 0) && (y >= 0) && (x < w) && (y < h) && board ! (x,y) == Bomb = 1
	| otherwise = 0

countBombsAround :: Int -> Int -> Board -> Int -> Int -> Int
countBombsAround x y board w h = 
	(isBombHelper (x+1) (y) board w h) + 
	(isBombHelper (x+1) (y+1) board w h)+
	(isBombHelper (x+1) (y -1) board w h)+
	(isBombHelper (x) (y-1) board w h)+
	(isBombHelper (x) (y+1) board w h)+
	(isBombHelper (x-1) (y) board w h)+
	(isBombHelper (x-1) (y-1) board w h)+
	(isBombHelper (x-1) (y+1) board w h)

initialGameState width height BombCount 
	= GameState (array indexRange $ zip(range indexRange) (cycle [Empty])) BombCount (width * height - BombCount) Start
	where indexRange = ((0,0),(width - 1, height -1))


generateRandomPositions :: Set (Int,Int) -> Int -> Int -> Int -> Set (Int,Int)
generateRandomPositions setSoFar 0 width height = setSoFar
generateRandomPositions setSoFar NumLeft width height = do 
	x1 <- randomIO :: IO Int 
	y1 <- randomIO :: IO Int 
	x <- x1 `rem` width
	y <- y1 `rem` height
	if Set.member (x,y) setSoFar
		then GenerateRandomPositions setSoFar NumLeft width height
	else GenerateRandomPositions (Set.insert (x,y) setSoFar) (NumLeft-1) width height
	

-- this is the function that is 'minesweeper', it takes an action and a state and returns the updated state
transformGame _ game = game