module Render where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game
import Data.Array
import Constants

backgroundColor = makeColor 0 0 0 255
foregroundColor = makeColor 255 255 255 255

window = InWindow "Minesweeper" (screenWidth, screenHeight) (100, 100)

gameoverScreen = translate textStartX 0 (scale textScale textScale (Color white (text "Boom.... :( Click to play again")))

winScreen = translate textStartX 0 (scale textScale textScale (Color white (text "You win! :) Click to play again")))

mySquare w h = rectangleWire (offsetX w) (offsetY h)

boardValue :: Cell -> Bool -> Picture
-- uncomment to see bombs
boardValue Bomb c 
  | c = circle 10
  | otherwise = blank

boardValue (Clicked n) _ = translate centerTextX centerTextY (scale smallTextScale smallTextScale (text (show n)))
boardValue _ _ = blank

position :: Picture -> Int -> Int -> Int -> Int -> Picture
position pic x y w h = let offX = offsetX w
                           offY = offsetY h 
                        in Translate ((fromIntegral x)*offX - (startX w)) ((fromIntegral y)*offY - (startY h)) pic

v1 ((_,_), v) = v 
x1 ((x,_), _) = x
y1 ((_,y), _) = y
boardValues board w h c = Pictures [ position (Color white (boardValue (v1 a) c)) (x1 a) (y1 a) w h | a <- assocs(board)]
boardGrid w h = Pictures [ position(Color white (mySquare w h)) x y w h | x<-[0..fromIntegral(w-1)], y<-[0..fromIntegral(h-1)] ]
boardScreen board w h c = Pictures [ (boardGrid w h), (boardValues board w h c) ]

gameAsPicture :: GameState -> Picture
gameAsPicture (GameState board bombs opened status width height _ cheats flaggedSpots)
  | status == Start || status == Continue = boardScreen board width height cheats
  | status == Gameover = gameoverScreen
  | otherwise = winScreen