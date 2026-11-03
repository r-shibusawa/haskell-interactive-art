-- ============================================
-- 10 カオス・メロディ
-- ロジスティック写像の x を「音の高さ」にする
-- r が小さいと同じ音のくり返し、r を上げると 2音・4音のフレーズ、そしてカオスの即興演奏
-- クリックで再生 / マウス左右 = r / 上下 = テンポ
-- ============================================
module Main (main) where

import Art
import qualified Data.Map.Strict as Map
import Numeric (showFFloat)

data World = World
  { wX        :: !Double     -- 長く動かしても 0 に落ちないよう Double で計算する
  , wPlaying  :: !Bool
  , wLastBeat :: !Float      -- 最後に鳴らした時刻（秒）
  , wPlayed   :: ![Int]      -- 鳴らした音の番号（新しいものが先頭）
  }

notes :: [String]
notes = ["C3", "D3", "E3", "G3", "A3", "C4", "D4", "E4", "G4", "A4", "C5", "D5", "E5", "G5"]   -- ★ 音階

noteCount :: Int
noteCount = length notes

noteTable :: Map.Map Int String
noteTable = Map.fromList (zip [0 ..] notes)

playedMax :: Int
playedMax = 64

main :: IO ()
main = runSketch (sketch "04-10 Chaos Melody" (World 0.3 False 0 []))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> step env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

rOf, intervalOf :: Env -> Float
rOf env = remapC (fst (mouseP5 env)) 0 (envW env) 2.8 4.0
intervalOf env = remapC (snd (mouseP5 env)) 0 (envH env) 0.1 0.5

step :: Env -> World -> IO World
step env w
  | wPlaying w && envTime env - wLastBeat w > interval = do
      let r = rOf env
          x = realToFrac r * wX w * (1 - wX w)                 -- ★ 次の値
          i = max 0 (min (noteCount - 1) (floor (x * fromIntegral noteCount)))   -- 0〜1 → 音の番号
          wave = if r > 3.57 then Sawtooth else Triangle
      playNote (Map.findWithDefault "C4" i noteTable) 0.3 (interval * 1.5) wave
      pure w { wX = x, wLastBeat = envTime env, wPlayed = take playedMax (i : wPlayed w) }
  | otherwise = pure w
  where interval = intervalOf env

stateText :: Float -> String
stateText r
  | r < 3.0   = "One note repeating"
  | r < 3.449 = "2-note phrase"
  | r < 3.544 = "4-note phrase"
  | r < 3.57  = "8 notes..."
  | otherwise = "Chaos (improvising)"

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( -- 鳴らした音を「楽譜」風に描く
    [ Pictures
        [ Color (hsb 0 0 25) (thickLine 1 (sc env left y) (sc env right y))
        , txt env 16 (y + 5) 14 (hsb 0 0 55) name ]
    | (k, name) <- zip [0 :: Int ..] notes
    , let y = noteY k ] ++
    zipWith drawNote [newest, newest - 1 ..] (wPlayed w) ++
  [ txt env 20 40 18 white
      (if wPlaying w
         then "r = " ++ fmt 3 r ++ " (mouse X)   " ++ stateText r ++ "   Tempo " ++ fmt 2 (intervalOf env) ++ " s (mouse Y)"
         else "Click to play")
  , txt env 20 70 14 (hsb 0 0 60) "x <- r x (1 - x)     The value of x (0-1) picks one of 14 notes"
  ])
  where
    r = rOf env
    left = 60
    right = envW env - 60
    top = 120
    bot = envH env - 80
    noteY :: Int -> Float
    noteY k = remap (fromIntegral k) 0 (fromIntegral (noteCount - 1)) bot top
    newest = length (wPlayed w) - 1
    drawNote :: Int -> Int -> Picture
    drawNote k i =
      let isLast = k == newest
          alpha = if isLast then 1 else 0.6
          hue = remap (fromIntegral i) 0 (fromIntegral noteCount) 180 360
          px = remap (fromIntegral k) 0 63 left right
      in Color (hsba hue 70 100 alpha) (at (sc env px (noteY i)) (disc (if isLast then 13 else 8)))

onEvent :: Env -> Event -> World -> IO World
onEvent env (EventKey (MouseButton LeftButton) Down _ _) w
  | not (wPlaying w) = pure w { wPlaying = True, wLastBeat = envTime env }
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. r を 2.8 から 4.0 までゆっくり動かして、音楽が変わっていくのを聞こう
-- 2. r = 3.83 付近は「3音のフレーズ」。見つけられる？
-- 3. notes を自分の好きな音階に。ペンタトニックだとカオスでも心地よい

-- ---------- 道具 ----------

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
