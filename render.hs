module Render where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

backgroundColor = makeColor 0 0 0 255

window = InWindow "Functional" (680, 400) (100, 100)

gameAsPicture _ = Blank