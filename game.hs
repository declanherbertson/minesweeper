module Game where
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Constants

import Data.Array
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set

import System.Random

data Cell = Empty | Clicked Int | Bomb
	deriving (Eq, Show)
type Board = (Array(Int, Int) Cell)
data GameStatus = Start | Continue | Gameover | Won
	deriving(Show, Eq)
type NumberOfBombs = Int
type NumberTilesOpened = Int
type Width = Int
type Height = Int
type InitSeed = Int
type CheatsOn = Bool
type FlaggedSpots = Set (Int,Int)
data GameState = GameState Board NumberOfBombs NumberTilesOpened GameStatus Width Height InitSeed CheatsOn FlaggedSpots deriving Show


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

initialGameState width height bombCount initSeed cheatsOn
	= GameState ((array indexRange $ zip(range indexRange) (cycle [Empty]))) bombCount 0
	Start width height initSeed cheatsOn Set.empty
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
setArrayValsToBombs a b = b // (zip a (cycle [Bomb]))


isClicked :: Cell -> Bool
isClicked (Clicked _) = True
isClicked _ = False

onPress :: Int -> Int -> GameState -> GameState
onPress x y (GameState b n t Start w h i c f) = 
	onPress x y (GameState (setArrayValsToBombs 
		(filter (/=(x,y)) (Set.toList
			(generateRandomPositions  
				(Set.singleton (x,y)) n w h 
				(randomRs (0,w-1) (mkStdGen i))
				(drop n (randomRs (0,h-1) (mkStdGen i)))
			)
		)
	) b)
	n t Continue w h i c f
	)
	

	
onPress x y (GameState board bombCount tilesOpened Continue width height i c f) = do
	let cella = board ! (x,y)
	let bombsAround = countBombsAround x y board width height		
	if x < 0 || y < 0 || x>= width || y >= height 
	then GameState board bombCount tilesOpened Continue width height i c f
	else 		
		if cella == Bomb
		then GameState board bombCount tilesOpened Gameover width height i c f
		else
		if isClicked cella then
		GameState board bombCount tilesOpened Continue width height i c f
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
				(GameState (board // [((x,y), Clicked bombsAround)]) bombCount (tilesOpened + 1) Continue width height i c (Set.delete (x,y) f) )))))))))
			else (GameState (board // [((x,y), Clicked bombsAround)]) bombCount (tilesOpened+1) Continue width height i c (Set.delete (x,y) f))

onPress x y (GameState board bombCount tilesOpened Gameover width height i c f) = (initialGameState width height bombCount (i+1) c)
onPress x y (GameState board bombCount tilesOpened Won width height i c f) = (initialGameState width height bombCount (i+1) c)

winningCheck (GameState board bombCount tilesOpened Continue width height i c f) = do
	if bombCount + tilesOpened == width * height
	then (GameState board bombCount tilesOpened Won width height i c f)
	else trace (show (bombCount + tilesOpened)) (GameState board bombCount tilesOpened Continue width height i c f)

winningCheck game = game

--our origin is bottom left corner: board[0][0] = bottom left corner
-- gloss origin is the center of the screen
getCellFromCoords (GameState _ _ _ _ w h _ _ _) position = getCellFromPosition w h position

positionIsFlagged ::(Int,Int) -> GameState -> Bool
positionIsFlagged pos (GameState board bombCount tilesOpened status width height i c f) = Set.member pos f

addOrRemove :: Set(Int,Int) -> (Int,Int) -> Set(Int,Int)
addOrRemove s val = 
	if Set.member val s
	then Set.delete val s
	else Set.insert val s

-- this is the function that is 'minesweeper', it takes an action and a state and returns the updated state
transformGame (EventKey (MouseButton LeftButton) Up _ coords) game = do
	let (r,c) = getCellFromCoords game coords
	if (positionIsFlagged (r,c) game)
	then trace (show (r,c)) game
	else trace (show (r,c)) (winningCheck(onPress r c game)) -- this is where you handle a click event for box at row r, col c

transformGame (EventKey (MouseButton RightButton) Up _ coords) (GameState board bombCount tilesOpened state width height i cc f) =	
	do
	let (r,c) = getCellFromCoords (GameState board bombCount tilesOpened state width height i cc f) coords
	if  (r >= 0) && (c >= 0) && (r < width) && (c < height) && (board ! (r,c) == Bomb || board ! (r,c) == Empty)
	then trace (show (r,c) ++ show (Set.toList (addOrRemove f (r,c)))) (GameState board bombCount tilesOpened state width height i cc (addOrRemove f (r,c))) 
	else (GameState board bombCount tilesOpened state width height i cc f) 


transformGame _ game = game






