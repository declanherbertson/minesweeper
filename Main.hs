module Main where

import Graphics.Gloss
import Render
import Game
import Text.Read

checkValidInt :: String -> Bool
checkValidInt strInt
 | (readMaybe strInt :: Maybe Int) == Nothing = False
 | otherwise = True

getParamInt :: String -> IO Int
getParamInt str = do 
  putStrLn(str)
  v <- getLine
  if checkValidInt v
    then return (read v :: Int)
    else getParamInt str

main :: IO ()
main = do
  w <- getParamInt "Please enter the width of the game (as an integer):"
  h <- getParamInt "Please enter the height of the game (as an integer):"
  b <- getParamInt "Please enter the number of bombs in game (as an integer):"
  s <- getParamInt "Please enter the random seed of the game (as an integer):"
  play window backgroundColor 30 (initialGameState w h b s) gameAsPicture transformGame (\_ -> id)
