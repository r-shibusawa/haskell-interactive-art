-- ============================================
-- 13 カタストロフィーの音
-- カスプの系 V(x) = x⁴ + a·x² + b·x の状態 x を「音の高さ」にする
--   コントロール (a, b) がゆっくり動く間は音もなめらかにすべる（短い音の連なり）。折り目をこえると音がとぶ + 「カン」と鳴る
--   カスプの外を通る道はなめらか、中を通る道はとぶ。耳でカタストロフィーを聞き分けよう
-- クリックで音をスタート / ドラッグ（左の平面）= (a, b) / A = 自動の道 / 1〜3 = 道の形 / ↑↓ = 速さ / M = 音を止める
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

baseFreq, octaves :: Float
baseFreq = 220             -- ★ x = 0 のときの音の高さ（Hz）
octaves = 1.2              -- ★ x が 1 変わると何オクターブ変わるか

toneEvery :: Int
toneEvery = 10             -- ★ 連続音の代わりに、何フレームごとに短い音を鳴らすか

data World = World
  { wA     :: !Float
  , wB     :: !Float
  , wX     :: !Float
  , wAuto  :: !Bool
  , wPath  :: !Int
  , wSpeed :: !Float       -- ★ 自動の道の速さ
  , wT     :: !Float
  , wMuted :: !Bool
  , wAudio :: !Bool        -- クリックで true
  , wLog   :: !(Seq Float)
  , wJumps :: ![(Int, Float, Float)]    -- 新しい順、30 個まで
  , wRings :: ![(Float, Float, Float)]
  }

world0 :: World
world0 = World (-1.5) 0 (-0.8) True 1 0.006 0 False False Seq.empty [] []

main :: IO ()
main = runSketch (sketch "35-13 Catastrophe Sound" world0)
  { skBack  = hsb 260 35 10
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

freqOf :: Float -> Float
freqOf x = baseFreq * 2 ** (x * octaves)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let t = if wAuto w0 then wT w0 + wSpeed w0 else wT w0
      (a, b) | not (wAuto w0) = (wA w0, wB w0)
             | wPath w0 == 1 = (-1.6, 2.4 * sin t)                         -- カスプを横切る（とぶ）
             | wPath w0 == 2 = (0.8, 2.4 * sin t)                          -- カスプの外（なめらか）
             | otherwise = (-0.8 + 1.4 * sin (t * 0.37), 2.2 * sin t)      -- 両方まざる
      descend :: Int -> Float -> Float
      descend 0 v = v
      descend k v = descend (k - 1) (v - 0.004 * (4 * v * v * v + 2 * a * v + b))
      x = clampF (-1.8) 1.8 (descend 20 (wX w0))
      frame = envFrame env
      farEnough = case wJumps w0 of
        [] -> True
        ((f, _, _) : _) -> frame - f > 20
      jump = abs (x - wX w0) > 0.06 && farEnough
      sounding = wAudio w0 && not (wMuted w0)
  when (jump && sounding) (playTone (clampF 60 4000 (freqOf x * 2)) 0.35 0.6 Sine)    -- 「カン」
  -- ★ 状態 x → 音の高さ（連続音の代わりに短い音を続けて鳴らす）
  when (sounding && frame `mod` toneEvery == 0) (playTone (clampF 40 3000 (freqOf x)) 0.18 0.2 Triangle)
  let rings0 = [ (ra, rb, l - 1) | (ra, rb, l) <- wRings w0, l > 1 ]
  pure w0 { wT = t, wA = a, wB = b, wX = x
          , wJumps = if jump then take 30 ((frame, a, b) : wJumps w0) else wJumps w0
          , wRings = if jump then (a, b, 39) : rings0 else rings0
          , wLog = let s = wLog w0 |> x in if Seq.length s > 600 then Seq.drop 1 s else s }

planeSize :: Env -> Float
planeSize env = min (envH env - 150) (envW env * 0.45)

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      py = 110
      sz = planeSize env
      px = 30
      x = wX w
      warm = hsb 25 90 100
      cool = hsb 195 80 100
      panel = hsb 260 25 17
      toB v = px + (v + 3) / 6 * sz
      toA v = py + (1.5 - v) / 4.5 * sz
      -- カスプの内側：へこんだ形なので、横長の台形に分けて塗る
      half = [ (-6 * s * s, 8 * s * s * s) | i <- [0 .. 35 :: Int], let s = 0.02 * fromIntegral i ]
      inner = zipWith (\(a0, b0) (a1, b1) -> Polygon [(toB (negate b0), toA a0), (toB b0, toA a0), (toB b1, toA a1), (toB (negate b1), toA a1)]) half (drop 1 half)
      outline = [ (toB (8 * s * s * s), toA (-6 * s * s)) | i <- [-35 .. 35 :: Int], let s = 0.02 * fromIntegral i ]
      rx = px + sz + 30
      rw = wd - rx - 30
      yOf v = py + sz / 2 - v / 1.8 * (sz / 2 - 20)
      logs = toList (wLog w)
      segs = zipWith3 (\i v0 v1 ->
                let jump = abs (v1 - v0) > 0.03
                    c | jump = hsb 0 80 100
                      | v1 > 0 = warm
                      | otherwise = cool
                in Color c (thickLine 3 (rx + fromIntegral (i - 1) / 600 * rw, yOf v0) (rx + fromIntegral i / 600 * rw, yOf v1)))
              [1 :: Int ..] logs (drop 1 logs)
      pathName = case wPath w of { 1 -> "across cusp"; 2 -> "outside cusp"; _ -> "mixed" }
      status | not (wAudio w) = "Click to start sound"
             | wMuted w = "Muted"
             | otherwise = "Playing"
  in Pictures
    [ onP5 env (Pictures
        [ Color panel (box px py sz sz)
        , Color (hsbA 50 80 100 25) (Pictures inner)
        , Color (hsb 50 80 100) (thickPath 2 (outline ++ take 1 outline))
        , Pictures [ Color (hsbA 0 0 100 (l * 2)) (at (toB rb, toA ra) (ring (max 0.5 ((80 - l * 1.8) / 2)) 2)) | (ra, rb, l) <- wRings w ]
        , Pictures [ Color (hsbA 0 80 100 70) (at (toB jb, toA ja) (disc 3.5)) | (_, ja, jb) <- wJumps w ]
        , Color (if x > 0 then warm else cool) (at (toB (wB w), toA (wA w)) (disc 9))
          -- 右：音の高さ（x）の楽譜ふうグラフ
        , Color panel (box rx py rw sz)
        , Color (hsbA 0 0 100 10) (Pictures [ Line [(rx, yy), (rx + rw, yy)] | k <- [-2 .. 2], let yy = py + sz / 2 - k * (sz / 2 - 20) / 2 ])
        , Pictures segs
        ])
    , txt env (px + 12) (py + 22) 14 (gray 0.85) "Control (a, b). Yellow: cusp (2 valleys). Red: jumps"
    , txt env (rx + 12) (py + 22) 14 (gray 0.85) ("Pitch = " ++ nf 0 (freqOf x) ++ " Hz (up = x, time ->). Red = jump")
    , txt env 20 36 14 white (status ++ "   A: auto " ++ onOff (wAuto w) ++ "   1-3: path (" ++ pathName ++ ")   Up/Dn: speed   M: mute   Drag: (a, b)")
    , txt env 20 62 14 (gray 0.6) "Path 1: the pitch glides, then CLANG! it jumps, at different places going and coming (hysteresis)."
    , txt env 20 84 14 (gray 0.6) "Path 2: smooth all the way."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton _) Down _ _ -> w { wAudio = True }
  EventMotion _
    | envDown env && mx > 30 && mx < 30 + sz && my > py && my < py + sz ->
        w { wAuto = False, wB = (mx - 30) / sz * 6 - 3, wA = 1.5 - (my - py) / sz * 4.5 }
  EventKey (SpecialKey KeyUp) Down _ _   -> on w { wSpeed = min 0.05 (wSpeed w * 1.4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> on w { wSpeed = max 0.001 (wSpeed w / 1.4) }
  EventKey (SpecialKey _) Down _ _ -> on w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> on w { wAuto = not (wAuto w) }
    | c `elem` "123" -> on w { wPath = fromEnum c - 48, wAuto = True }
    | c `elem` "mM" -> on w { wMuted = not (wMuted w) }
    | otherwise -> on w
  _ -> w
  where
    on v = v { wAudio = True }
    (mx, my) = mouseP5 env
    py = 110
    sz = planeSize env

-- ---------- やってみよう ----------
-- 1. 道 1 と道 2 を聞き比べよう。目を閉じても、どちらの道か当てられる？
-- 2. octaves を 3 にすると、とぶ音の差が大きくなる。baseFreq を 110 にすると？
-- 3. x を「音の高さ」でなく「音の大きさ」や「音色（波形）」にしてみよう
-- 4. 2 つのカスプ系を別々の道で動かして、2 声の曲にしてみよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
