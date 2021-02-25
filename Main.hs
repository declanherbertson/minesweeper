module Main where

import Graphics.Gloss
import Render
import Game


main :: IO ()
main = do
  -- TODO add validation
  putStrLn("Please enter the width of the game:")
  width <- getLine
  let w = (read width :: Int)
  putStrLn("Please enter the height of the game:")
  height <- getLine
  let h = (read height :: Int)
  putStrLn("Please enter the number of bombs in the game:")
  numBombs <- getLine
  let b = (read numBombs :: Int)
  putStrLn("Please enter random seed:")
  randomSeed <- getLine
  let s = (read randomSeed :: Int)

  -- putStrLn $ show $ initialGameState w h b s

  play window backgroundColor 30 (initialGameState w h b s) gameAsPicture transformGame (\_ -> id)
