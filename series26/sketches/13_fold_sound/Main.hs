-- ============================================
-- 13 折りの音
-- 紙折り数列（12 のドラゴン曲線の折り目）を音にする。右に曲がる（1）→ 音が 1 つ上へ、左（0）→ 1 つ下へ
--   ドラゴン曲線をなぞる点と音がいっしょに進む。規則的なのにくり返さない、不思議なメロディ
-- クリックで音をスタート / 1 = メロディ / 2 = リズム / 3 = 両方 / ↑↓ = テンポ / ←→ = 折る回数 n / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U

noteScale :: [Int]
noteScale = [0, 2, 4, 7, 9]   -- ★ 音階（ペンタトニック）。半音の数

baseFreq :: Float
baseFreq = 220                -- ★ いちばん下の音（Hz）

data World = World
  { wN       :: !Int          -- ★ 折る回数（2ⁿ − 1 個の折り目で 1 周）
  , wBpm     :: !Int          -- ★ 1 分あたりの音の数
  , wMode    :: !Int
  , wIdx     :: !Int
  , wLevel   :: !Int
  , wHeading :: !Int
  , wPaused  :: !Bool
  , wStarted :: !Bool         -- 音をスタートしたか
  , wLast    :: !Float        -- 最後に音を鳴らした時刻
  , wFlash   :: !Float
  , wPts     :: !(U.Vector (Float, Float))  -- ドラゴン曲線の点（p5 の座標）
  , wBase    :: Picture       -- 曲線の全体（薄く描いた絵。変わるまで使い回す）
  }

main :: IO ()
main = runSketch (sketch "26-13 Sound of Folds" (build 1280 800 World
    { wN = 8, wBpm = 300, wMode = 3, wIdx = 0, wLevel = 7, wHeading = 0, wPaused = False
    , wStarted = False, wLast = 0, wFlash = 0, wPts = U.empty, wBase = Blank }))
  { skBack  = rgb255 24 22 30
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

turnAt :: Int -> Int
turnAt k
  | even k = turnAt (k `div` 2)
  | k `mod` 4 == 1 = 1
  | otherwise = 0

-- ドラゴン曲線の点（画面に収まるように）
build :: Float -> Float -> World -> World
build wd ht w =
  let nn = 2 ^ wN w :: Int
      go (!x, !y, !hd, acc) i =
        let (dx, dy) = case hd `mod` 4 of
              0 -> (1, 0)
              1 -> (0, 1)
              2 -> (-1, 0)
              _ -> (0, -1)
            x' = x + dx
            y' = y + dy
            hd' | i < nn = if turnAt i == 1 then hd + 1 else hd - 1
                | otherwise = hd
        in (x', y', hd', (x', y') : acc)
      (_, _, _, rev) = foldl' go (0 :: Float, 0 :: Float, 0 :: Int, [(0, 0)]) [1 .. nn]
      raw = reverse rev
      ext (!a, !b, !c, !d) (x, y) = (min a x, max b x, min c y, max d y)
      (x0, x1, y0, y1) = foldl' ext (0, 0, 0, 0) raw
      s = min ((wd * 0.6) / max 1 (x1 - x0)) ((ht - 260) / max 1 (y1 - y0))
      ox = wd * 0.35 - (x0 + x1) / 2 * s
      oy = 110 + (ht - 260) / 2 - (y0 + y1) / 2 * s
      pts = [(ox + x * s, oy + y * s) | (x, y) <- raw]
      toGloss (x, y) = (x - wd / 2, ht / 2 - y)
  in w { wPts = U.fromList pts
       , wBase = Color (rgba255 255 255 255 30) (thickPath 2 (map toGloss pts))
       , wIdx = 0, wLevel = 7, wHeading = 0 }

-- ★ 1 歩進んで、その折り目の音を鳴らす
stepOnce :: World -> IO World
stepOnce w0 = do
  let w1 = if wIdx w0 + 1 >= 2 ^ wN w0
             then w0 { wIdx = 1, wLevel = 7, wHeading = 0 }
             else w0 { wIdx = wIdx w0 + 1 }
      b = turnAt (wIdx w1)
      nL = length noteScale
      level = max 0 (min (nL * 3 - 1) (wLevel w1 + (if b == 1 then 1 else -1)))
      heading = (wHeading w1 + (if b == 1 then 1 else 3)) `mod` 4
      (oct, r) = level `divMod` nL
      semi = sum (take 1 (drop r noteScale)) + 12 * oct
      mode = wMode w1
  when (mode == 1 || mode == 3) $
    playTone (clampF 40 4000 (baseFreq * 2 ** (fromIntegral semi / 12))) 0.18 (60 / fromIntegral (wBpm w1) * 1.6) Triangle
  when (mode == 2 || mode == 3) $
    if b == 1 then playKick else playHat
  pure w1 { wLevel = level, wHeading = heading, wFlash = 1 }

-- 低い太鼓と、高いシャカ
playKick, playHat :: IO ()
playKick = playTone 70 0.5 0.2 Sine
playHat = playTone 7000 0.12 0.05 Square

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  w1 <- if wStarted w0 && not (wPaused w0) && envTime env - wLast w0 > 60 / fromIntegral (wBpm w0)
          then stepOnce w0 { wLast = envTime env }
          else pure w0
  pure w1 { wFlash = wFlash w1 * 0.85 }

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let pts = wPts w
      idx = wIdx w
      nP = U.length pts
      from = max 0 (idx - 200)
      -- 曲線：なぞった所は明るく
      trail =
        [ Color (hsba (wrapF 360 (fromIntegral i * 0.7)) 70 100 ((30 + 70 * fromIntegral (i - from) / 200) / 100))
            (thickLine 2 (sc env (pts U.! i)) (sc env (pts U.! (i + 1))))
        | i <- [from .. idx - 1], i + 1 < nP ]
      headPic = case pts U.!? min idx (nP - 1) of
        Just p -> Color (rgb255 255 220 90) (at (sc env p) (disc (7 + wFlash w * 8)))
        Nothing -> Blank
      -- 音の高さの帯（右）
      bx = envW env * 0.75
      by = 120
      bh = envH env - 300
      nLevels = length noteScale * 3
      bandH = bh / fromIntegral nLevels
      band =
        [ let top = by + bh - fromIntegral (i + 1) * bandH
              (cx, cy) = sc env (bx + 80, top + (bandH - 2) / 2)
          in Color (if i == wLevel w then rgb255 255 190 40 else rgba255 255 255 255 25) (rect cx cy 160 (bandH - 2))
        | i <- [0 .. nLevels - 1] ]
        ++ [ txt env bx (by - 10) 14 (gray 0.78) "Pitch (right = up, left = down)" ]
      -- 数列のテープ（下）
      tx = 20
      ty = envH env - 110
      cw = 18
      shown = floor ((envW env - 40) / cw) :: Int
      maxK = 2 ^ wN w :: Int
      tape =
        [ let (cx, cy) = sc env (tx + fromIntegral j * cw + (cw - 3) / 2, ty + (if b == 1 then 0 else 20) + 9)
              b = turnAt k
              col | k == idx = rgb255 255 190 40
                  | b == 1 = rgb255 90 150 255
                  | otherwise = rgb255 230 90 70
          in Color col (rect cx cy (cw - 3) 18)
        | j <- [0 .. shown - 1]
        , let k = idx - shown `div` 2 + j
        , k >= 1 && k < maxK ]
        ++ [ txt env tx (ty - 10) 14 (gray 0.63) "Paper-folding tape (upper row = 1 = right, lower row = 0 = left)" ]
      status | not (wStarted w) = "Click to start sound"
             | wPaused w = "Paused"
             | otherwise = "Playing"
      modeName = case wMode w of { 1 -> "melody"; 2 -> "rhythm"; _ -> "both" }
      headingName = case wHeading w of { 0 -> "right"; 1 -> "down"; 2 -> "left"; _ -> "up" }
      hud =
        [ txt env 20 40 18 white (status ++ "   1: melody  2: rhythm  3: both (" ++ modeName ++ ")   Up/Down: tempo " ++ show (wBpm w) ++ "   Left/Right: n = " ++ show (wN w) ++ "   Space: pause")
        , txt env 20 68 15 (gray 0.67) ("Crease " ++ show idx ++ " / " ++ show (maxK - 1) ++ "   heading " ++ headingName ++ "    Rhythm: 1 = low drum, 0 = high shaker")
        ]
  in Pictures ([wBase w] ++ trail ++ [headPic] ++ band ++ tape ++ hud)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> start
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> start { wMode = fromEnum c - fromEnum '0' }
    | otherwise -> start
  EventKey (SpecialKey KeyUp) Down _ _ -> start { wBpm = min 900 (wBpm w + 30) }
  EventKey (SpecialKey KeyDown) Down _ _ -> start { wBpm = max 60 (wBpm w - 30) }
  EventKey (SpecialKey KeyRight) Down _ _ -> build (envW env) (envH env) start { wN = min 12 (wN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> build (envW env) (envH env) start { wN = max 2 (wN w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> start { wPaused = not (wPaused w) }
  EventResize _ -> build (envW env) (envH env) w
  _ -> w
  where
    -- 最初の操作で音をスタートする
    start | wStarted w = w
          | otherwise = w { wStarted = True, wLast = envTime env }

-- ---------- やってみよう ----------
-- 1. noteScale を [0, 3, 5, 7, 10]（ブルース風）や [0, 2, 4, 5, 7, 9, 11]（ドレミ）に変えてみよう
-- 2. 音の高さを「上がる・下がる」ではなく、曲線の向き wHeading（4 通り）で決めると？
-- 3. テンポを 900 まで上げると、数列のくり返し構造（自己相似）がリズムとして聞こえてくる
-- 4. 1 のときだけ和音（playChord）にして、ドラゴンの角ごとにコードが変わる曲をつくろう

-- ==================================================
-- ここから下は「画面の道具」
-- ==================================================

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s
