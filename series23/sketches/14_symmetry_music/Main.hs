-- ============================================
-- 14 対称性の音楽（自由制作のベース）
-- メロディに群の元を作用させる：移調 Tk（時計を k 回す = Z12）、反行 I（時計を裏返す）、逆行 R（時間を逆に）
--   {原形 P, 逆行 R, 反行 I, 反行の逆行 RI} はクラインの四元群。12 音技法（シェーンベルク）やバッハのカノンで使われる
-- 下のマス目をクリック = メロディを書く / 1 = P、2 = R、3 = I、4 = RI / ←→ = 移調 k / スペース = 再生・停止 / X = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (xor)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

data World = World
  { wMelody  :: ![Int]    -- ★ 最初のメロディ（0 = C、1 = C#、… 11 = B）
  , wForm    :: !Int      -- 0 = P, 1 = R, 2 = I, 3 = RI
  , wT       :: !Int      -- 移調（半音）
  , wPlaying :: !Bool
  , wStep    :: !Int      -- 再生している音の番号（なければ -1）
  , wNext    :: !Float    -- 次の音を鳴らす時刻
  }

bpm :: Float
bpm = 140                -- ★ 速さ

base :: Int
base = 60                -- ★ C の MIDI 番号（60 = 真ん中のド）

noteNames, forms :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
forms = ["P", "R", "I", "RI"]

noteName :: Int -> String
noteName p = noteNames !! (p `mod` 12)

main :: IO ()
main = runSketch (sketch "23-14 Symmetry Music" (World [0, 4, 7, 11, 9, 5, 2, 7] 0 0 False (-1) 0))
  { skBack  = hsb 250 20 12
  , skStep  = \env _ w -> step (envTime env) w
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

-- 群の作用：反行は最初の音を軸に裏返す、逆行は順番を逆に、最後に移調
transformed :: World -> [Int]
transformed w =
  let m = wMelody w
      first = case m of
        (a : _) -> a
        [] -> 0
      inv = wForm w == 2 || wForm w == 3
      rev = wForm w == 1 || wForm w == 3
      m1 = if inv then map (\p -> (2 * first - p + 120) `mod` 12) m else m
      m2 = if rev then reverse m1 else m1
  in map (\p -> (p + wT w) `mod` 12) m2

-- MIDI 番号 midi の音を dur 秒鳴らす（三角波 + 少しの倍音）
playMidi :: Int -> Float -> IO ()
playMidi midi dur = do
  let f = midiFreq midi
  playTone f 0.25 dur Triangle
  playTone (f * 2) 0.06 dur Sine

-- 再生
step :: Float -> World -> IO World
step now w
  | wPlaying w && now >= wNext w - 0.05 = do
      let tm = transformed w
          s = (wStep w + 1) `mod` max 1 (length tm)
      playMidi (base + tm !! s) (60 / bpm * 0.9)
      pure w { wStep = s, wNext = max (wNext w) now + 60 / bpm }
  | otherwise = pure w

-- ピアノロールの位置
gx, gy, gw, gh, cw, ch :: Float
gx = 30
gy = height * 0.74
gw = width * 0.5
gh = height * 0.22
cw = gw / 8
ch = gh / 12

drawWorld :: World -> Picture
drawWorld w = Pictures ([clockRing] ++ axis ++ [polyLine (wMelody w) (hsba 0 0 100 0.25), polyLine tm (hsba 190 70 100 0.9)]
                        ++ clockDots ++ rollIn ++ rollOut ++ klein ++ hud)
  where
    tm = transformed w
    form = wForm w
    -- 時計（12 音）：もとのメロディ（うすい）と変換後を多角形で
    cx = width * 0.27
    cy = height * 0.43
    r = min (width * 0.18) (height * 0.24)
    angle p = -pi / 2 + fromIntegral p * tau / 12 :: Float
    clockRing = Color (hsba 0 0 100 0.2) (at (pt (cx, cy)) (ring r 2))
    first = case wMelody w of
      (a : _) -> a
      [] -> 0
    axis
      | form >= 2 =     -- 反行の鏡の軸
          let a = angle (first + wT w)
          in [ dashed 1.5 (hsba 50 80 100 0.5)
                 (cx - cos a * r * 1.2, cy - sin a * r * 1.2) (cx + cos a * r * 1.2, cy + sin a * r * 1.2) ]
      | otherwise = []
    polyLine m c = Color c (thickPath 3
      [ pt (cx + cos (angle p) * r * 0.92, cy + sin (angle p) * r * 0.92) | p <- m ])
    current | wPlaying w && wStep w >= 0 = drop (wStep w) tm
            | otherwise = []
    clockDots = concat
      [ [ Color (hsb (fromIntegral ((p * 30) `mod` 360)) 60 (if on then 100 else 70)) (at (pt (x, y)) (disc (if on then 22 else 15)))
        , txtC (x + cos a * 34) (y + sin a * 34) 14 white (noteName p) ]
      | p <- [0 .. 11 :: Int]
      , let a = angle p
            x = cx + cos a * r
            y = cy + sin a * r
            on = take 1 current == [p] ]
    -- ピアノロール（下）：8 ステップ × 12 音。もとのメロディを書く
    rollIn =
      [ box (gx + fromIntegral s * cw) (gy + fromIntegral (11 - p) * ch) (cw - 1) (ch - 1)
          (hsba 0 0 100 (if p == v then 0.7 else 0.06))
      | (s, v) <- zip [0 :: Int ..] (wMelody w), p <- [0 .. 11 :: Int] ]
      ++ [ txt gx (gy - 8) 14 (gray 0.8) "Write the melody (click)" ]
    -- 変換後のロール（右）
    rx = width * 0.58
    rollOut =
      [ box (rx + fromIntegral s * cw * 0.8) (gy + fromIntegral (11 - p) * ch) (cw * 0.8 - 2) ch
          (hsb (fromIntegral ((p * 30) `mod` 360)) 60 (if s == wStep w && wPlaying w then 100 else 75))
      | (s, p) <- zip [0 :: Int ..] tm ]
      ++ [ txt rx (gy - 8) 14 (gray 0.8) ("Melody you hear: " ++ formName ++ (if wT w /= 0 then " -> T" ++ show (wT w) else "")) ]
    formName = forms !! (form `mod` 4)
    -- クラインの四元群の表
    tx = width * 0.6
    ty = 130 :: Float
    c = 56 :: Float
    klein =
      txt tx (ty - 20) 14 white "Klein four-group {P, R, I, RI} (row, then column)"
      : concat
        [ let x = tx + fromIntegral (j + 1) * c
              y = ty + fromIntegral (i + 1) * c * 0.7
              lab | i < 0 = forms !! j
                  | j < 0 = forms !! i
                  | otherwise = forms !! (i `xor` j)
              idx = fromMaybe 0 (elemIndex lab forms)
              col | i < 0 || j < 0 = hsb 0 0 30
                  | otherwise = hsb (fromIntegral ((idx * 90 + 20) `mod` 360)) 50 (if lab == formName then 95 else 55)
          in [ box x y (c - 4) (c * 0.7 - 4) col
             , txtC (x + c / 2 - 2) (y + c * 0.35 - 2) 16 white lab ]
        | i <- [-1 .. 3 :: Int], j <- [-1 .. 3 :: Int], i >= 0 || j >= 0 ]
      ++ [ txt tx (ty + c * 3.8 + 20 * fromIntegral i) 14 (gray 0.75) s
         | (i, s) <- zip [0 :: Int ..]
             [ "Any one applied twice gives P (a*a = P)."
             , "R*I = I*R = RI (commutative)."
             , "T + I = clock rotation + mirror: D12 (24 elements)"
             , "With R too: a group of 48 elements." ] ]
      ++ [ txt tx (ty + c * 3.8 + 104) 18 white ("Notes: " ++ unwords (map noteName tm)) ]
    hud =
      [ txt 20 36 18 white ("Click: write melody   1: P  2: R  3: I  4: RI   Left/Right: transpose T = " ++ show (wT w)
          ++ "   Space: " ++ (if wPlaying w then "stop" else "play") ++ "   X: random")
      , txt 20 62 15 (gray 0.65) "Transpose = rotate the clock polygon, inversion = mirror it, retrograde = walk it backward. The shape stays the same"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    let (mx, my) = toP5 (envMouse env)
        s = floor ((mx - gx) / cw) :: Int
        p = 11 - floor ((my - gy) / ch) :: Int
    if s >= 0 && s < length (wMelody w) && p >= 0 && p < 12
      then do
        playMidi (base + p) 0.25
        pure w { wMelody = [if i == s then p else v | (i, v) <- zip [0 ..] (wMelody w)] }
      else pure w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure w { wForm = fromEnum c - fromEnum '1' }
    | c == 'x' || c == 'X' -> do
        m <- replicateM (length (wMelody w)) (randomRIO (0, 11))
        pure w { wMelody = m }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wT = (wT w + 1) `mod` 12 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wT = (wT w + 11) `mod` 12 }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure w { wPlaying = not (wPlaying w), wStep = -1, wNext = envTime env }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1〜4 を切りかえて、時計の多角形がどう動くか見よう。I は鏡映、T は回転
-- 2. R を 2 回、I を 2 回 → 原形 P。R → I と I → R が同じ RI になるのを表で確かめよう
-- 3. C・E・G（0, 4, 7）を I にすると長調が短調の形に。耳で聴き比べよう
-- 4. 12 音を 1 回ずつ使う「音列」を melody にすると 12 音技法。P・R・I・RI × 移調 12 = 48 通り
-- 5. 2 つのメロディ（P と T7 など）を同時に鳴らしてカノンを作ろう。作品のベースに

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

dashed :: Float -> Color -> Point -> Point -> Picture
dashed w c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      n = floor (l / 12) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Color c (Pictures
       [ thickLine w (pt (p (12 * fromIntegral i / l))) (pt (p (min 1 ((12 * fromIntegral i + 6) / l))))
       | i <- [0 .. n] ])
