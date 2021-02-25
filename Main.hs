module Main where

import Graphics.Gloss
import Render
import Game
import UserInput

main :: IO ()
main = do
  w <- getParamInt "Please enter the width of the game (as an integer greater than 1):" (Just 2) Nothing
  h <- getParamInt "Please enter the height of the game (as an integer greater than 1):" (Just 2) Nothing
  b <- getParamInt ("Please enter the number of bombs in game (as an integer" ++ " less than " ++ (show (w*h)) ++ "):") (Just 0) (Just (w*h-1))
  s <- getParamInt "Please enter the random seed of the game (as an integer of at least 0):" (Just 0) Nothing
  c <- getParamYN "Would you like to enable cheats? (y/n)"
  play window backgroundColor 30 (initialGameState w h b s c) gameAsPicture transformGame (\_ -> id)
