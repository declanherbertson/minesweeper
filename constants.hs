module Constants where

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

startX :: Int -> Float
startX w = (fromIntegral screenWidth / 2) + (offsetX w) / 2 - 1

startY :: Int -> Float
startY h = (fromIntegral screenHeight / 2)  + (offsetY h) / 2 - 1

offsetX :: Int -> Float
offsetX w = fromIntegral screenWidth / fromIntegral(w) - 1

offsetY :: Int -> Float
offsetY h = fromIntegral screenHeight / fromIntegral(h) - 1