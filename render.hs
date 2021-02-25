module Render where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game

backgroundColor = makeColor 0 0 0 255
foregroundColor = makeColor 255 255 255 255

window = InWindow "Minesweeper" (680, 400) (100, 100)

gameoverScreen = blank 
winScreen = blank
boardScreen board = Color white (thickCircle 88 10)

gameAsPicture :: GameState -> Picture
gameAsPicture (GameState board bombs opened status width height seed)
  | status == Start || status == Continue = boardScreen board
  | status == Gameover = gameoverScreen
  | otherwise = winScreen