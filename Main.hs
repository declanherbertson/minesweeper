module Main where

import Graphics.Gloss
import Render
import Game
import Text.Read
import Data.Char

lowerString str = [ toLower loweredString | loweredString <- str]

checkValidInt :: String -> Maybe Int -> Maybe Int -> Bool

checkValidInt strInt Nothing Nothing
 | (readMaybe strInt :: Maybe Int) == Nothing = False
 | otherwise = True

checkValidInt strInt (Just min) Nothing
 | (readMaybe strInt :: Maybe Int) == Nothing = False
 | otherwise = (read strInt :: Int) >= min

checkValidInt strInt Nothing (Just max)
 | (readMaybe strInt :: Maybe Int) == Nothing = False
 | otherwise = (read strInt :: Int) <= max

checkValidInt strInt (Just min) (Just max)
 | (readMaybe strInt :: Maybe Int) == Nothing = False
 | otherwise = (read strInt :: Int) >= min && (read strInt :: Int) <= max

checkValidYN :: String -> Bool
checkValidYN str = let lstr = lowerString str in (lstr == "y" || lstr == "n" || lstr == "yes" || lstr == "no")

getParamInt :: String -> Maybe Int -> Maybe Int -> IO Int
getParamInt str mmin mmax = do 
  putStrLn(str)
  v <- getLine
  if checkValidInt v mmin mmax
    then return (read v :: Int)
    else getParamInt str mmin mmax

getParamYN :: String -> IO Bool
getParamYN str = do 
  putStrLn(str)
  v <- getLine
  if checkValidYN v
  then let lstr = lowerString v in return ((v == "y" || v == "yes") == True)
  else getParamYN str

main :: IO ()
main = do
  w <- getParamInt "Please enter the width of the game (as an integer greater than 1):" (Just 2) Nothing
  h <- getParamInt "Please enter the height of the game (as an integer greater than 1):" (Just 2) Nothing
  b <- getParamInt ("Please enter the number of bombs in game (as an integer" ++ " less than " ++ (show (w*h)) ++ "):") (Just 0) (Just (w*h-1))
  s <- getParamInt "Please enter the random seed of the game (as an integer of at least 0):" (Just 0) Nothing
  c <- getParamYN "Would you like to enable cheats? (y/n)"
  play window backgroundColor 30 (initialGameState w h b s c) gameAsPicture transformGame (\_ -> id)
