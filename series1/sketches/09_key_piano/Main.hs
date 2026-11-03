-- ============================================
-- 09 キーボードピアノ
-- A S D F G H J K キーでドレミファソラシド
-- 音に合わせて光の波紋が広がる
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower, toUpper)
import Data.List (elemIndex)

-- 波紋 1 つ
data Ripple = Ripple
  { rX, rY :: !Float
  , rSize  :: !Float      -- 直径
  , rHue   :: !Float
  , rLife  :: !Float      -- 寿命（100 → 0）
  }

data World = World
  { wRipples :: ![Ripple]
  , wGlow    :: ![Float]  -- 鍵盤の光り具合（8 個）
  }

keyList :: String
keyList = "asdfghjk"

notes, names :: [String]
notes = ["C4", "D4", "E4", "F4", "G4", "A4", "B4", "C5"]  -- ★ C3 などに下げるとどうなる？
names = ["Do", "Re", "Mi", "Fa", "So", "La", "Ti", "Do"]

main :: IO ()
main = runSketch (sketch "01-09 Key Piano" (World [] (replicate 8 0)))
  { skBack  = hsb 250 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> World
step w = w
  { wRipples = [ r { rSize = rSize r + 6, rLife = rLife r - 1.5 }   -- ★ 広がる速さと寿命
               | r <- wRipples w, rLife r - 1.5 > 0 ]
  , wGlow = map (\g -> max 0 (g - 4)) (wGlow w)
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map drawRipple (wRipples w) ++ keys)
  where
    keyW = envW env / 8
    keys = concat (zipWith3 (drawKey env keyW) [0 ..] (wGlow w) (zip names keyList))

-- ---- 波紋（残像ぶんも計算して描く）----
drawRipple :: Ripple -> Picture
drawRipple r = Pictures (concatMap echo [4, 3, 2, 1, 0])
  where
    echo k =
      let f = fromIntegral (k :: Int)
          size = rSize r - 6 * f
          alpha = (rLife r + 1.5 * f) / 100 * 0.7 ** f     -- 古いほど薄い
          col = hsba (rHue r) 70 100 alpha
      in if size <= 0 then [] else
         [ Color col (at (rX r, rY r) (ring (size / 2) 6))
         , Color col (at (rX r, rY r) (ring (size * 0.6 / 2) 2)) ]

-- ---- 鍵盤 ----
drawKey :: Env -> Float -> Int -> Float -> (String, Char) -> [Picture]
drawKey env keyW i glow (name, k) =
  [ Color edge (roundRect x top keyW 120 8)
  , Color (hsb (fromIntegral i * 45) glow 100) (roundRect (x + 2) (top - 2) (keyW - 4) 116 6)   -- 押すと色がつく
  , labelC cx (-h / 2 + 75) 20 edge name
  , labelC cx (-h / 2 + 40) 14 edge [toUpper k]
  ]
  where
    h = envH env
    edge = hsb 0 0 20
    x = -envW env / 2 + fromIntegral i * keyW
    top = -h / 2 + 120
    cx = x + keyW / 2

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | Just i <- elemIndex (toLower c) keyList -> pressKey env i w
  _ -> pure w

pressKey :: Env -> Int -> World -> IO World
pressKey env i w = do
  playNote (concat (take 1 (drop i notes))) 0.4 0.6 Triangle   -- ★ Square にすると音色が変わる
  -- 画面の上から 15%〜60% のどこかに波紋を出す
  y <- randomRIO (envH env / 2 - envH env * 0.6, envH env / 2 - envH env * 0.15)
  let new = Ripple
        { rX = (fromIntegral i + 0.5) * envW env / 8 - envW env / 2
        , rY = y
        , rSize = 0
        , rHue = fromIntegral i * 45
        , rLife = 100
        }
  pure w
    { wRipples = take 150 (new : wRipples w)
    , wGlow = [ if j == i then 80 else g | (j, g) <- zip [0 ..] (wGlow w) ]
    }

-- ---------- やってみよう ----------
-- 1. 好きな曲を弾いてみよう（かえるのうた：ドレミファミレド）
-- 2. notes を 1 オクターブ下げる（C4 → C3 など）とどうなる？
-- 3. playNote の最後の Triangle を Square にすると音色が変わる

-- ---------- 道具 ----------

-- 角の丸い四角。(x, top) は左上の角、w は幅、h は高さ（下へのびる）、r は角の丸み
roundRect :: Float -> Float -> Float -> Float -> Float -> Picture
roundRect x top w h r = Polygon (concatMap corner
  [ (x + w - r, top - h + r, 270), (x + w - r, top - r, 0), (x + r, top - r, 90), (x + r, top - h + r, 180) ])
  where
    corner (cx, cy, a0) =
      [ (cx + r * cos a, cy + r * sin a)
      | i <- [0 .. 4 :: Int], let a = (a0 + fromIntegral i * 90 / 4) * pi / 180 ]
