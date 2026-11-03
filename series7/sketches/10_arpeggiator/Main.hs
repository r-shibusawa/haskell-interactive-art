-- ============================================
-- 10 コード進行とアルペジオ（音楽が「進む」しくみ）
-- 4 つの和音をくり返す「コード進行」。それぞれの和音の音を順番に鳴らす（アルペジオ）
-- 1〜4 で有名なコード進行 / マウス左右 = テンポ / 上下 = 音の並べ方 / クリックで再生・停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (intercalate)

-- 度数で表したコード進行（ローマ数字）。I = ドミソ、IV = ファラド、V = ソシレ、vi = ラドミ
data Prog = Prog { pName :: String, pRoman :: [String], pChords :: [[Int]] }

data Particle = Particle
  { ptX :: !Float, ptY :: !Float, ptSize :: !Float, ptHue :: !Float, ptLife :: !Float }

data World = World
  { wCurrent :: !Int          -- コード進行の番号（0〜3）
  , wPlaying :: !Bool
  , wNext    :: !Float        -- 次の音を鳴らす時刻（秒）
  , wStep    :: !Int
  , wChord   :: !Int          -- いま光っている和音
  , wParts   :: ![Particle]   -- 粒（鳴った音）
  }

chI, chIi, chIv, chV, chVi :: [Int]
chI = [0, 4, 7]
chIi = [2, 5, 9]
chIv = [5, 9, 12]
chV = [7, 11, 14]
chVi = [9, 12, 16]

-- ★ 自分のコード進行を足してみよう（度数：I=0, ii=2, iii=4, IV=5, V=7, vi=9）
progressions :: [Prog]
progressions =
  [ Prog "I - V - vi - IV (pop)" ["I", "V", "vi", "IV"] [chI, chV, chVi, chIv]
  , Prog "vi - IV - I - V (wistful)" ["vi", "IV", "I", "V"] [chVi, chIv, chI, chV]
  , Prog "I - vi - ii - V (jazz)" ["I", "vi", "ii", "V"] [chI, chVi, chIi, chV]
  , Prog "I - IV - V - I (basic)" ["I", "IV", "V", "I"] [chI, chIv, chV, chI]
  ]

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

main :: IO ()
main = runSketch (sketch "07-10 Arpeggiator" (World 0 False 0 0 0 []))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> scheduler env (moveParticles w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- テンポ：マウスの左右で 70〜160
bpmOf :: Env -> Float
bpmOf env = clampF 70 160 (remap (fst (fromG env (envMouse env))) 0 (envW env) 70 160)

-- 音の並べ方：マウスが上半分なら上昇、下半分なら上昇→下降
isUp :: Env -> Bool
isUp env = snd (fromG env (envMouse env)) < envH env / 2

moveParticles :: World -> World
moveParticles w = w { wParts = [p { ptY = ptY p - 2, ptLife = ptLife p - 2 } | p <- wParts w, ptLife p > 2] }

-- 時刻が来たら次の音を鳴らす
scheduler :: Env -> World -> IO World
scheduler env w
  | wPlaying w && wNext w <= envTime env = do
      let dur = 60 / bpmOf env / 2                  -- 8 分音符
          idx = wStep w
          chordIdx = (idx `div` 8) `mod` 4          -- 1 和音 = 8 ステップ
          chord = pChords (progressions !! wCurrent w) !! chordIdx
          sq = if isUp env then [0, 1, 2, 3] else [0, 1, 2, 3, 2, 1] :: [Int]  -- ★ 並べ方
          k = sq !! (idx `mod` length sq)
          midi = 60 + chord !! (k `mod` 3) + (if k >= 3 then 12 else 0)   -- 4 番目は 1 オクターブ上の根音
          cw = (envW env - 160) / 4
      playTone (hzOf midi) 0.25 (dur * 1.8) Triangle
      -- 和音の低音
      when (idx `mod` 8 == 0) (forM_ chord (\n -> playTone (hzOf (48 + n)) 0.12 (dur * 8) Sine))
      off <- randomRIO (40, max 41 (cw - 40))
      let part = Particle (80 + fromIntegral chordIdx * cw + off) (envH env * 0.7)
                          (14 + fromIntegral k * 4) (fromIntegral chordIdx * 60 + 200) 100
      scheduler env w { wChord = chordIdx, wParts = part : wParts w, wStep = idx + 1, wNext = wNext w + dur }
  | otherwise = pure w

hzOf :: Int -> Float
hzOf m = 440 * 2 ** (fromIntegral (m - 69) / 12)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (cards ++ concatMap particle (wParts w) ++ hud)
  where
    prog = progressions !! wCurrent w
    cw = (envW env - 160) / 4
    y = envH env * 0.25
    -- ---- 4 つの和音 ----
    cards = concat
      [ [ Color (hsb (fromIntegral c * 60 + 200) 60 (if active then 60 else 25)) (box env (x + 8) y (cw - 16) 120)
        , txtM env (x + cw / 2) (y + 40) 22 white roman
        , txtM env (x + cw / 2) (y + 80) 16 (hsb 0 0 80) (intercalate " " (map (\n -> noteNames !! (n `mod` 12)) chord))
        ]
      | (c, roman, chord) <- zip3 [0 :: Int ..] (pRoman prog) (pChords prog)
      , let x = 80 + fromIntegral c * cw
            active = wPlaying w && c == wChord w ]
    -- ---- 粒（鳴った音）。うすい残像を 3 つ重ねる ----
    particle p =
      [ Color (hsba (ptHue p) 70 100 (clampF 0 1 ((ptLife p + 2 * k) / 100) * 0.4 ** k))
              (at (toG env (ptX p, ptY p + 2 * k)) (disc (ptSize p / 2)))
      | k <- [3, 2, 1, 0] ]
    hud =
      [ txt env 20 40 18 white
          (pName prog ++ " (1-4)   Tempo " ++ show (round (bpmOf env) :: Int) ++ " (mouse X)   Order: "
           ++ (if isUp env then "up" else "up-down") ++ " (mouse Y)   Click: " ++ (if wPlaying w then "stop" else "play"))
      , txt env 20 68 15 (hsb 0 0 60) "One note at a time = arpeggio. Repeating 4 chords already sounds like a song. Many hits use No. 1."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let on = not (wPlaying w)
    in pure (if on then w { wPlaying = True, wNext = envTime env + 0.05, wStep = 0 } else w { wPlaying = False })
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' ->
    pure w { wCurrent = fromEnum c - fromEnum '1', wStep = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 と 2 は同じ 4 つの和音なのに、始める場所がちがうだけで明るさが変わる
-- 2. 自分のコード進行を progressions に追加（度数：I=0, ii=2, iii=4, IV=5, V=7, vi=9）
-- 3. アルペジオの並べ方を自分で決める（sq を [0, 2, 1, 3] など）

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（上下左右とも中央揃え）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx gy s c str
