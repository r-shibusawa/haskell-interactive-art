-- ============================================
-- 13 不動点を聞く 🔊
-- ロジスティック写像 x ← r·x(1 − x) をくり返し、x を音の高さにする
--   不動点に吸い込まれる → 同じ音が鳴り続ける / 周期 2 → トリル / 周期 4 → 4 音のフレーズ / カオス → 終わらないメロディ
-- クリックで音をスタート / ←→ = r（Shift で細かく）/ ↑↓ = テンポ / 1〜5 = r の見本 / Q = 音階にそろえる / スペース = 止める / R = x をリセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

data World = World
  { wR      :: !Double   -- ★ ロジスティック写像のパラメータ
  , wX      :: !Double
  , wBps    :: !Int      -- ★ 1 秒に何音
  , wQuant  :: !Bool     -- 五音音階にそろえる
  , wPaused :: !Bool
  , wNotes  :: ![Double] -- 鳴らした x の記録（新しい順、200 個まで）
  , wNext   :: !Float    -- 次の音の時刻（秒）
  , wAudio  :: !Bool     -- 音をスタートしたか
  , wFine   :: !Bool     -- Shift を押しているか
  }

rlo, rhi :: Double
rlo = 2.4                -- ★ 分岐図の r の範囲
rhi = 4.0

scaleNotes :: [Double]
scaleNotes = [0, 2, 4, 7, 9]   -- 五音音階（ド レ ミ ソ ラ）

bifW, bifH :: Int
bifW = 352               -- 分岐図の解像度（2 倍に拡大して表示する）
bifH = 168

main :: IO ()
main = runSketch (sketch "31-13 Hearing Fixed Points" (World 2.8 0.2 6 True False [] 0 False False))
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 分岐図：r ごとに 300 回くり返して、最後の 150 回の x を点で打つ
bifImage :: U.Vector (Word8, Word8, Word8)
bifImage = U.create $ do
  v <- MU.replicate (bifW * bifH) (16, 18, 30)
  forM_ [0 .. bifW - 1] $ \i -> do
    let rr = rlo + (rhi - rlo) * fromIntegral i / fromIntegral (bifW - 1)
        go k xx
          | k >= 300 = pure ()
          | otherwise = do
              let x' = rr * xx * (1 - xx)
              when (k > 150) $ do
                let j = floor ((1 - x') * fromIntegral (bifH - 1)) :: Int
                    p = j * bifW + i
                when (p >= 0 && p < bifW * bifH) $ do
                  (a, b, c) <- MU.read v p
                  MU.write v p (add8 a 60, add8 b 70, add8 c 40)
              go (k + 1) x'
    go (0 :: Int) (0.5 :: Double)
  pure v
  where
    add8 :: Word8 -> Int -> Word8
    add8 a d = fromIntegral (min 255 (fromIntegral a + d))

bifPicture :: Picture
bifPicture = pixelImage bifW bifH 2 (\x y -> bifImage U.! (y * bifW + x))

-- ★ x（0〜1）→ 音の高さ（Hz）。2 オクターブ
pitch :: Bool -> Double -> Float
pitch quant v =
  let semis = v * 24
      semis'
        | quant =
            let oct = fromIntegral (floor (semis / 12) :: Int)
                s = semis - oct * 12
                best = foldl' (\a b -> if abs (b - s) < abs (a - s) then b else a) 0 scaleNotes
            in oct * 12 + best
        | otherwise = semis
  in realToFrac (220 * 2 ** (semis' / 12) :: Double)

-- 周期を調べる：最後の値が p 個前と同じなら周期 p
detectPeriod :: [Double] -> Int
detectPeriod notes
  | U.length v < 40 = 0
  | otherwise = case [p | p <- [1 .. 16], all (\i -> abs (v U.! i - v U.! (i + p)) <= 1e-4) [0 .. 15]] of
      (p : _) -> p
      [] -> -1
  where v = U.fromList (take 40 notes)

-- リアプノフ指数：負なら近くの点が近づく（周期）、正なら離れる（カオス）
lyapunov :: Double -> Double
lyapunov r = go (0 :: Int) 0.3 0 / 500
  where
    go !k !xx !s
      | k >= 600 = s
      | otherwise =
          let x' = r * xx * (1 - xx)
              s' = if k >= 100 then s + log (max 1e-12 (abs (r * (1 - 2 * x')))) else s
          in go (k + 1) x' s'

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let d = if wFine w0 then 0.0005 else 0.004
      r1 | isDown env (SpecialKey KeyLeft) = max rlo (wR w0 - d)
         | otherwise = wR w0
      r2 | isDown env (SpecialKey KeyRight) = min rhi (r1 + d)
         | otherwise = r1
      w = w0 { wR = r2 }
  -- 拍が来たら 1 回くり返して鳴らす
  if not (wPaused w) && envTime env >= wNext w
    then do
      let x0 = wR w * wX w * (1 - wX w)
          x1 = if isNaN x0 || isInfinite x0 || x0 <= 0 || x0 >= 1 then 0.2 else x0
          len = 1 / fromIntegral (wBps w) :: Float
      when (wAudio w) (playNote' (pitch (wQuant w) x1) len)
      pure w { wX = x1, wNotes = take 200 (x1 : wNotes w), wNext = envTime env + len }
    else pure w

-- 短い音を 1 つ（f Hz、len 秒）：三角波とオクターブ上のサイン波
playNote' :: Float -> Float -> IO ()
playNote' f len = do
  playTone f 0.25 (max 0.08 (len * 0.95)) Triangle
  playTone (f * 2) 0.12 (max 0.08 (len * 0.95)) Sine

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ Translate (bx + sw / 2) (by + sh / 2) (Scale 1 (-1) bifPicture)
  , ln 2 (hsb 320 70 100) (rxp, by) (rxp, by + sh)
  , spot (hsb 320 70 100) (rxp, by + (1 - fl (wX w)) * sh) 10
  , txt bx (by + sh + 18) 14 (hsb 0 0 70) ("r = " ++ nf 1 rlo)
  , txt (bx + sw - 60) (by + sh + 18) 14 (hsb 0 0 70) ("r = " ++ nf 1 rhi)
  , txt bx (by - 8) 14 (hsb 0 0 70) "Bifurcation diagram: horizontal = r, vertical = where x settles"
  -- 下：鳴らした音の列（楽譜のように）
  , box (hsb 0 0 30) bx sy0 sw sh2
  , txt bx (sy0 - 8) 14 (hsb 0 0 70) "Notes played (newest on the right)"
  ]
  ++ [ fillBox (hsba (wrapF 360 (fl v * 300 + 180)) 70 100 ((40 + 60 * fromIntegral i / fromIntegral cnt) / 100))
         (bx + fromIntegral i * sw / 64 + 1) (sy0 + sh2 - fl v * sh2 - 4) (sw / 64 - 2) 8
     | (i, v) <- zip [0 :: Int ..] shown ]
  ++ cobweb
  ++ [ txt 20 34 15 white ((if wAudio w then "Sound on" else "Click to start sound") ++ "   r = " ++ nf 4 (wR w) ++ " (Left/Right, Shift: fine)   tempo " ++ show (wBps w) ++ "/s (Up/Down)   1-5: presets   Q: scale " ++ (if wQuant w then "ON" else "off") ++ "   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   R: reset")
     , txt 20 62 14 (hsb 0 0 60) "x = 0..1 becomes 2 octaves of pitch. For converging r, there is a short melody at first, then a single note: the sound of a fixed point."
     ]
  where
    fl :: Double -> Float
    fl = realToFrac
    bx = 40
    by = 100
    sw = fromIntegral bifW * 2 :: Float
    sh = fromIntegral bifH * 2 :: Float
    rxp = bx + fl ((wR w - rlo) / (rhi - rlo)) * sw
    sy0 = by + sh + 50
    sh2 = envH env - sy0 - 40
    newest = wNotes w
    shown = reverse (take 64 newest)
    cnt = length shown
    -- 右：クモの巣図（01）
    cx0 = bx + sw + 50
    cs = min (envW env - cx0 - 30) 300
    cy0 = by
    r = wR w
    cobweb
      | cs > 120 =
          let toP u v = (cx0 + fl u * cs, cy0 + cs - fl v * cs)
              t = reverse (take 30 newest)
              xs = 1 - 1 / r
              d = abs (2 - r)
              per = detectPeriod newest
              ly = lyapunov r
              ty = cy0 + cs + 34
          in [ box (hsb 0 0 35) cx0 cy0 cs cs
             , Color (hsb 0 0 60) (Line [(cx0, cy0 + cs), (cx0 + cs, cy0)])
             , Color (hsb 190 70 100) (thickPath 2 [ toP u (r * u * (1 - u)) | s <- [0 .. 100 :: Int], let u = fromIntegral s / 100 ])
             , Color (hsba 320 70 100 0.7) (Line (concat (zipWith (\a b -> [toP a a, toP a b]) t (drop 1 t)) ++ map (\b -> toP b b) (take 1 (reverse t))))
             , spot (hsb (if d < 1 then 140 else 0) 80 100) (toP xs xs) 10
             , txt cx0 ty 15 white ("Fixed point x* = 1 - 1/r = " ++ nf 4 xs)
             , txt cx0 (ty + 26) 15 white ("Slope |f'(x*)| = |2-r| = " ++ nf 3 d ++ (if d < 1 then " (attracts)" else " (repels)"))
             , txt cx0 (ty + 56) 15 (hsb 45 90 100) (periodText per)
             , txt cx0 (ty + 82) 15 (hsb 0 0 80) ("Lyapunov exponent " ++ nf 3 ly ++ (if ly > 0 then " (+: chaos)" else " (-: settles)"))
             , txt cx0 (ty + 108) 14 (hsb 0 0 60) "At r = 3 the fixed point starts to repel and"
             , txt cx0 (ty + 128) 14 (hsb 0 0 60) "period 2 is born. 3.449...: period 4 ..."
             , txt cx0 (ty + 148) 14 (hsb 0 0 60) "3.5699...: chaos. Near 3.83: a period-3 window."
             ]
      | otherwise = []
    periodText per
      | per == 1 = "-> reached a fixed point: same note"
      | per > 1 = "-> period " ++ show per ++ ": " ++ show per ++ " notes repeat"
      | per == 0 = "..."
      | otherwise = "-> no period (chaos?)"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w0 { wAudio = True }
  EventKey k Down m _ -> keyDown k w0 { wAudio = True, wFine = shift m == Down }
  EventKey _ Up m _ -> pure w0 { wFine = shift m == Down }
  _ -> pure w0

keyDown :: Key -> World -> IO World
keyDown k w = case k of
  SpecialKey KeyUp -> pure w { wBps = min 16 (wBps w + 1) }
  SpecialKey KeyDown -> pure w { wBps = max 1 (wBps w - 1) }
  SpecialKey KeySpace -> pure w { wPaused = not (wPaused w) }
  Char c
    | c == '1' -> preset 2.8
    | c == '2' -> preset 3.2
    | c == '3' -> preset 3.5
    | c == '4' -> preset 3.835
    | c == '5' -> preset 3.95
    | toLower c == 'q' -> pure w { wQuant = not (wQuant w) }
    | toLower c == 'r' -> pure w { wX = 0.2, wNotes = [] }
  _ -> pure w
  where preset r = pure w { wR = r, wX = 0.2 }

-- ---------- やってみよう ----------
-- 1. r = 2.8 → 3.2 → 3.5 と上げて、1 音 → 2 音 → 4 音に分かれる瞬間を耳で聞こう（周期倍分岐）
-- 2. r = 3.83 付近で、カオスの中から急に 3 音のくり返しが現れる（周期 3 の窓）。Shift + ←→ で探そう
-- 3. pitch の 24（2 オクターブ）を 36 にしたり、scaleNotes を [0, 3, 5, 7, 10] に変えて雰囲気を変えよう
-- 4. 写像を x ← r·sin(πx)/4 などに変えても、同じ順番で分岐が起きる（ファイゲンバウムの普遍性）

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""
