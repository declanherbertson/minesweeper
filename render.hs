module Render where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game
import Constants

backgroundColor = makeColor 0 0 0 255
foregroundColor = makeColor 255 255 255 255

window = InWindow "Minesweeper" (screenWidth, screenHeight) (100, 100)

gameoverScreen = blank 

winScreen = blank

square x y w h = rectangleWire (offsetX w) (offsetY h)

position pic x y w h = let offX = offsetX w
                           offY = offsetY h 
                        in Translate (x*offX - (startX w)) (y*offY - (startY h)) pic

boardGrid board w h = Pictures [ position (Color white (square x y w h)) x y w h | x<-[1..fromIntegral(w)], y<-[1..fromIntegral(h)] ]
boardScreen board w h = Pictures [ (boardGrid board w h) ]

gameAsPicture :: GameState -> Picture
gameAsPicture (GameState board bombs opened status width height _)
  | status == Start || status == Continue = boardScreen board width height
  | status == Gameover = gameoverScreen
  | otherwise = winScreen