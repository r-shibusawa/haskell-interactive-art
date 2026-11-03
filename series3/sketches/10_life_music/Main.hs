-- ============================================
-- 10 ライフゲーム・ミュージック
-- 02 のライフゲームを「楽譜」にする。縦が音の高さ、横が時間。
-- 再生バーが通った列の、生きているセルの音が鳴る。世代が進むと曲も変わる
-- クリックで音 ON・セルを描く / スペース = 世代を進める ON/OFF / R = ランダム / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U

data World = World
  { wGrid     :: !(U.Vector Word8)   -- x + y * cols 番目のセル（0 か 1）
  , wFlash    :: !(U.Vector Float)   -- 鳴ったセルが光る強さ
  , wPlayhead :: !Int                -- 再生バーの位置（列）
  , wLastBeat :: !Float
  , wEvolving :: !Bool
  , wAudioOn  :: !Bool               -- クリックするまで音は鳴らさない
  }

cols, rows :: Int
cols = 32                  -- ★ 列の数（横＝時間）
rows = 12

beat :: Float
beat = 0.25                -- ★ 8分音符の長さ（BPM 120）

-- ★ 縦の位置 → 音（ペンタトニック・スケール。何が重なってもきれい）
notes :: [String]
notes = ["C5", "A4", "G4", "E4", "D4", "C4", "A3", "G3", "E3", "D3", "C3", "A2"]

emptyGrid :: U.Vector Word8
emptyGrid = U.replicate (cols * rows) 0

main :: IO ()
main = runSketch (sketch "03-10 Life Music" (World emptyGrid (U.replicate (cols * rows) 0) 0 0 True False))
  { skBack  = hsb 260 40 10
  , skStart = \_ -> do
      g <- randomGrid
      pure (World g (U.replicate (cols * rows) 0) 0 0 True False)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

randomGrid :: IO (U.Vector Word8)
randomGrid = U.replicateM (cols * rows) ((\r -> if r < (0.2 :: Float) then 1 else 0) <$> randomRIO (0, 1))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let w = w0 { wFlash = U.map (* 0.9) (wFlash w0) }
  if wAudioOn w && envTime env - wLastBeat w > beat
    then do
      -- 8分音符ごとに再生バーを進めて、この列の生きているセルを鳴らす
      let ph = (wPlayhead w + 1) `mod` cols
          playing = [(y, n) | (y, n) <- zip [0 ..] notes, wGrid w U.! (ph + y * cols) == 1]
      forM_ playing $ \(_, n) -> playNote n 0.15 0.4 Triangle
      let flash = wFlash w U.// [(ph + y * cols, 1) | (y, _) <- playing]
          -- 1周ごとにライフゲームを1世代進める
          grid = if ph == 0 && wEvolving w then nextGeneration (wGrid w) else wGrid w
      pure w { wPlayhead = ph, wLastBeat = envTime env, wFlash = flash, wGrid = grid }
    else pure w

-- ライフゲームのルール（02 と同じ）
nextGeneration :: U.Vector Word8 -> U.Vector Word8
nextGeneration grid = U.generate (cols * rows) $ \i ->
  let x = i `mod` cols
      y = i `div` cols
      c = sum [ grid U.! (((x + dx + cols) `mod` cols) + ((y + dy + rows) `mod` rows) * cols)
              | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0 ]
  in if grid U.! i == 1
       then (if c == 2 || c == 3 then 1 else 0)
       else (if c == 3 then 1 else 0)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures
      [ Pictures [cellPic x y | y <- [0 .. rows - 1], x <- [0 .. cols - 1]]
      -- 再生バー
      , Color (hsba 0 0 100 0.25) (box (fromIntegral (wPlayhead w) * cw) 60 cw (envH env - 60))
      ]
  , hud env 30 16 white $
      if wAudioOn w
        then "Click: draw cells   Space: evolve " ++ (if wEvolving w then "ON" else "off") ++ "   R: random   C: clear"
        else "Click to turn the sound ON"
  ]
  where
    cw = envW env / fromIntegral cols
    ch = (envH env - 60) / fromIntegral rows
    cellPic x y =
      let i = x + y * cols
          fy = fromIntegral y
          col | wGrid w U.! i == 1 = hsb (remap fy 0 (fromIntegral rows) 300 180) 60 (60 + wFlash w U.! i * 40)
              | otherwise = hsb 260 30 15
      in Color col (roundBox (fromIntegral x * cw + 1) (60 + fy * ch + 1) (cw - 2) (ch - 2) 4)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        x = floor (mx / (envW env / fromIntegral cols)) :: Int
        y = floor ((my - 60) / ((envH env - 60) / fromIntegral rows)) :: Int
        i = x + y * cols
        w1 = if wAudioOn w then w else w { wAudioOn = True, wLastBeat = envTime env }
    in pure $ if x >= 0 && x < cols && y >= 0 && y < rows
         then w1 { wGrid = wGrid w1 U.// [(i, 1 - wGrid w1 U.! i)] }
         else w1
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wEvolving = not (wEvolving w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> (\g -> w { wGrid = g }) <$> randomGrid
    | c `elem` "cC" -> pure w { wGrid = emptyGrid }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. スペースで世代を止めて、自分で描いたパターンを「作曲」しよう
-- 2. notes を変える（例：長調のドレミファソラシ）
-- 3. beat の 0.25（8分音符）を 0.125 にすると倍速になる

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の rect と同じ、左上の角 (x, y) と幅・高さで決める長方形
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 角を r だけ落とした長方形（p5.js の rect の角丸の代わり）
roundBox :: Float -> Float -> Float -> Float -> Float -> Picture
roundBox x y w h r = Polygon
  [ (x + r, y), (x + w - r, y), (x + w, y + r), (x + w, y + h - r)
  , (x + w - r, y + h), (x + r, y + h), (x, y + h - r), (x, y + r) ]

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
