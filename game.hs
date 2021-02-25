module Game where

import Data.Array
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set

import System.Random

data Cell = Empty | Clicked Int | Bomb
	deriving (Eq, Show)
type Board = (Array(Int, Int) Cell)
data GameStatus = Start | Continue | Gameover deriving (Show, Eq)
type NumberOfBombs = Int
type NumberTilesOpened = Int
type Width = Int
type Height = Int
type InitSeed = Int
data GameState = GameState Board NumberOfBombs NumberTilesOpened GameStatus Width Height InitSeed deriving Show


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

initialGameState width height bombCount initSeed
	= GameState ((array indexRange $ zip(range indexRange) (cycle [Empty]))) bombCount (width * height - bombCount) 
	Start width height initSeed
	where indexRange = ((0,0),(width - 1, height -1))


generateRandomPositions :: Set (Int,Int) -> Int -> Int -> Int -> [Int] -> [Int] -> Set (Int,Int)
generateRandomPositions setSoFar 0 width height _ _ = setSoFar
generateRandomPositions setSoFar numLeft width height (x:t) (y:r)= do 
	--x <- randomRIO (0,width)
	--y <- randomRIO (0,height)
	--x <- x1 
	--y <- y1 
	if Set.member (x,y) setSoFar
		then generateRandomPositions setSoFar numLeft width height t r
	else generateRandomPositions (Set.insert (x,y) setSoFar) (numLeft-1) width height t r

setArrayValsToBombs :: [(Int,Int)] -> Array(Int,Int) Cell -> Array (Int,Int) Cell
--setArrayValsToBombs [] soFar = soFar
--setArrayValsToBombs (h:t) soFar = setArrayValsToBombs t (soFar // [(h, Bomb)])
setArrayValsToBombs a b = b // zip a (cycle [Bomb])


onPress :: Int -> Int -> GameState -> GameState
onPress x y (GameState b n t Start w h i) = 
	onPress x y (GameState (setArrayValsToBombs 
		(filter (/=(x,y)) (Set.toList
			(generateRandomPositions  
				(Set.singleton (x,y)) n w h 
				(randomRs (0,w) (mkStdGen i))
				(drop n (randomRs (0,h) (mkStdGen i)))
			)
		)
	) b)
	n t Continue w h i
	)
	
onPress x y (GameState board bombCount tilesOpened Continue width height i) = do
	let cella = board ! (x,y)
	let bombsAround = countBombsAround x y board width height		
	if x < 0 || y < 0 || x>= width || y >= height 
	then GameState board bombCount tilesOpened Continue width height i
	else 		
		if cella == Bomb
		then GameState board bombCount tilesOpened Gameover width height i
		else			
			if bombsAround == 0
			then (onPress (x+1) y 
				(onPress (x+1) (y-1)
				(onPress (x) (y+1)
				(onPress x (y-1) 
				(onPress (x-1) (y-1) 
				(onPress (x-1) y 
				(onPress (x-1) (y+1)
				(onPress (x+1) (y+1)
				(GameState (board // [((x,y), Clicked bombsAround)]) bombCount tilesOpened Continue width height i)))))))))
			else (GameState (board // [((x,y), Clicked bombsAround)]) bombCount tilesOpened Continue width height i)
				

-- this is the function that is 'minesweeper', it takes an action and a state and returns the updated state
transformGame _ game = game



















