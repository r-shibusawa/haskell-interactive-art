-- ============================================
-- 11 カントール・リズム（音のフラクタル）
-- 線を 3 つに分けて真ん中を消す「カントール集合」を、
-- 時間に当てはめるとリズムになる。段ごとにちがう音が鳴る
-- クリックで再生 / 1〜5 キーで段の ON/OFF / マウス左右 = テンポ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.Set as S
import Numeric (showFFloat)

levels :: Int
levels = 5   -- 段の数

notes :: [String]
notes = ["C2", "G2", "C3", "E3", "G3"]   -- ★ 段ごとの音

data World = World
  { wOn        :: ![Bool]    -- 段ごとの ON/OFF
  , wFlash     :: ![Float]   -- 段が光る
  , wPlaying   :: !Bool
  , wLoopStart :: !Float
  , wFired     :: !(S.Set (Int, Int, Int))  -- この周で鳴らしたか（段, 周, 区間）
  }

main :: IO ()
main = runSketch (sketch "02-11 Cantor Rhythm" (World (replicate levels True) (replicate levels 0) False 0 S.empty))
  { skBack  = hsb 250 30 8
  , skStep  = \env _ w -> scheduler env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- カントール集合：level 段目の「残っている区間」を (始まり, 終わり)（0〜1）で返す
cantor :: Int -> [(Float, Float)]
cantor 0 = [(0, 1)]
cantor level = concatMap cut (cantor (level - 1))
  where
    cut (a, b) =
      let t = (b - a) / 3
      in [(a, a + t), (b - t, b)]   -- 左の 1/3 と 右の 1/3（真ん中は消す）

-- 段ごとの区間（同じ計算をくり返さないよう、先に作っておく）
table :: [(Int, String, [(Float, Float)])]
table = [(l, n, cantor l) | (l, n) <- zip [0 .. levels - 1] notes]

-- 1 周の長さ（秒）
loopLenOf :: Env -> Float
loopLenOf env = clampF 2 8 (remap (fst (envMouse env) + envW env / 2) 0 (envW env) 8 2)

-- もうすぐ鳴る音を調べて鳴らす
scheduler :: Env -> World -> IO World
scheduler env w0 = do
  let w = w0 { wFlash = map (* 0.85) (wFlash w0) }
  if not (wPlaying w) then pure w else do
    w' <- foldM fire w events
    pure (if S.size (wFired w') > 2000 then w' { wFired = S.empty } else w')  -- 古い記録を捨てる
  where
    now = envTime env
    loopLen = loopLenOf env
    elapsed = now - wLoopStart w0
    cycle0 = floor (elapsed / loopLen) :: Int
    events =
      [ (l, c, i, n, t)
      | (l, n, segs) <- table
      , on <- take 1 (drop l (wOn w0)), on
      , (i, (a, _)) <- zip [0 ..] segs
      -- 区間の始まりの時刻（今の周と次の周）
      , c <- [cycle0, cycle0 + 1]
      , let t = wLoopStart w0 + fromIntegral c * loopLen + a * loopLen
      , t >= now - 0.05 && t < now + 0.02
      ]
    fire w (l, c, i, n, _)
      | S.member (l, c, i) (wFired w) = pure w
      | otherwise = do
          playNote n 0.25 (0.3 + fromIntegral (levels - l) * 0.15) (if l < 2 then Sawtooth else Triangle)
          pure w { wFired = S.insert (l, c, i) (wFired w)
                 , wFlash = [if k == l then 1 else f | (k, f) <- zip [0 ..] (wFlash w)] }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concatMap row table ++ [playhead, info])
  where
    ww = envW env
    wh = envH env
    loopLen = loopLenOf env
    x0 = -ww / 2 + ww * 0.08
    bw = ww * 0.84
    rowH = (wh - 140) / fromIntegral levels
    yOf l = wh / 2 - (100 + fromIntegral l * rowH)   -- 段の上の端
    row (l, n, segs) =
      let isOn = and (take 1 (drop l (wOn w)))
          fl = sum (take 1 (drop l (wFlash w)))
          bright = if isOn then 55 + fl * 45 else 15
          y = yOf l
          h = rowH * 0.6
          c = hsb (fromIntegral l * 50) 70 bright
      in Color c (Pictures
           [ rect (x0 + a * bw + ((b - a) * bw - 2) / 2) (y - h / 2) ((b - a) * bw - 2) h | (a, b) <- segs ])
         : [ label x0 (y + 6) 14 (gray (if isOn then 1 else 0.4))
               (show (l + 1) ++ "  " ++ n ++ "  x" ++ show (length segs)) ]
    -- 再生ヘッド
    phase = wrapF 1 ((envTime env - wLoopStart w) / loopLen)   -- いま 1 周の何 % か
    playhead
      | wPlaying w =
          let x = x0 + phase * bw
          in Color white (thickLine 2 (x, wh / 2 - 90) (x, -wh / 2 + 40))
      | otherwise = Blank
    info = label (-ww / 2 + 20) (wh / 2 - 30) 16 white $
      if wPlaying w
        then "Keys 1-5: rows on/off   Mouse left/right: tempo (loop " ++ showFFloat (Just 1) loopLen "" ++ " s)"
        else "Click to play"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ | not (wPlaying w) ->
    pure w { wPlaying = True, wLoopStart = envTime env + 0.1, wFired = S.empty }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '5' ->
    let k = fromEnum c - fromEnum '1'
    in pure w { wOn = [if i == k then not b else b | (i, b) <- zip [0 ..] (wOn w)] }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 段を 1 つだけ ON にして、リズムを聞き分けよう。段が深いほど細かくなる
-- 2. notes を変えて、好きな和音にしよう
-- 3. 「真ん中を消す」を「左を消す」に変えると、どんなリズムになる？
