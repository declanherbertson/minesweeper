module Main where

import Graphics.Gloss
import Render
import Game


main :: IO ()
main = play window backgroundColor 30 initialGameState gameAsPicture transformGame (\_ -> id)
