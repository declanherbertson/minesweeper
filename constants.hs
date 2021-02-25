module Constants where

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

textStartX :: Float
textStartX = 5 - (fromIntegral screenWidth / 2)

textScale :: Float
textScale = 0.3

smallTextScale :: Float
smallTextScale = 0.15

centerTextX :: Float
centerTextX = 0 - 5

centerTextY :: Float
centerTextY = 0 - 5

startX :: Int -> Float
startX w = (fromIntegral screenWidth / 2) - (offsetX w) /2 - 1

startY :: Int -> Float
startY h = (fromIntegral screenHeight / 2)  - (offsetY h) / 2 - 1

offsetX :: Int -> Float
offsetX w = fromIntegral screenWidth / fromIntegral(w) - 1

offsetY :: Int -> Float
offsetY h = fromIntegral screenHeight / fromIntegral(h) - 1

-- todo may need to tweek boundaries since rect is positioned in middle
getCellFromPosition :: Int -> Int -> (Float, Float) -> (Int, Int)
getCellFromPosition w h (x, y)
  | x < (0 - startX w) - (offsetX w) / 2 = (-1, -1)
  | y < (0 - startY h) - (offsetY h) / 2 = (-1, -1)
  | x > (fromIntegral(w)*(offsetX w) - (startX w) - (offsetX w) / 2) = (-1, -1)
  | y > (fromIntegral(h)*(offsetY h) - (startY h) - (offsetY h) / 2) = (-1, -1)
  | otherwise = (r, c)
  where r = round ((x + (startX w))/(offsetX w))
        c = round ((y + (startY h))/(offsetY h))