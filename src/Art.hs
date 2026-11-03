-- | スケッチはこれ 1 つを import すれば書ける。
module Art
  ( module Art.Sketch, module Art.Draw, module Art.Sound, module Art.Device
  , module Graphics.Gloss, module Graphics.Gloss.Interface.IO.Game
  , randomRIO
  ) where

import Art.Device
import Art.Draw
import Art.Sketch
import Art.Sound
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
