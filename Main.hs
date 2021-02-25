module Main where

import Graphics.Gloss
import Render
import Game
import Text.Read
import Data.Char

lowerString str = [ toLower loweredString | loweredString <- str]

checkValidInt :: String -> Bool
checkValidInt strInt
 | (readMaybe strInt :: Maybe Int) == Nothing = False
 | otherwise = True

checkValidYN :: String -> Bool
checkValidYN str = let lstr = lowerString str in (lstr == "y" || lstr == "n" || lstr == "yes" || lstr == "no")

getParamInt :: String -> IO Int
getParamInt str = do 
  putStrLn(str)
  v <- getLine
  if checkValidInt v
    then return (read v :: Int)
    else getParamInt str

getParamYN :: String -> IO Bool
getParamYN str = do 
  putStrLn(str)
  v <- getLine
  if checkValidYN v
  then let lstr = lowerString v in return ((v == "y" || v == "yes") == True)
  else getParamYN str

main :: IO ()
main = do
  w <- getParamInt "Please enter the width of the game (as an integer):"
  h <- getParamInt "Please enter the height of the game (as an integer):"
  b <- getParamInt "Please enter the number of bombs in game (as an integer):"
  s <- getParamInt "Please enter the random seed of the game (as an integer):"
  c <- getParamYN "Would you like to enable cheats? (y/n)"
  play window backgroundColor 30 (initialGameState w h b s c) gameAsPicture transformGame (\_ -> id)
