-- ============================================
-- 01 音階と和音（五度圏）
-- 12 の音を「5 度ずつ」並べると円になる（五度圏）。隣どうしは仲がいい音
-- 音階 = 12 音から 7 つ選ぶパターン。和音 = 音階から 1 つおきに 3 つ取る
-- 円の音をクリックで調（キー）を変える / 1〜4 で音階の種類 / スペースで音階を演奏 / C で和音
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.List (intercalate, partition)

-- 予約した音：鳴らす時刻、MIDI 番号、光らせる音（0〜11）
data Booked = Booked !Float !Float !Int

data World = World
  { wRoot      :: !Int        -- 調の主音（0 = C）
  , wScale     :: !Int        -- 音階の種類（0〜3）
  , wHigh      :: !Int        -- 光っている音（-1 = なし）
  , wHighTime  :: !Float
  , wBooked    :: ![Booked]   -- これから鳴らす音
  }

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- ★ 音階：名前と、主音からの半音の数
scales :: [(String, [Int])]
scales =
  [ ("Major scale", [0, 2, 4, 5, 7, 9, 11])
  , ("Minor scale", [0, 2, 3, 5, 7, 8, 10])
  , ("Pentatonic", [0, 2, 4, 7, 9])
  , ("Blues", [0, 3, 5, 6, 7, 10])
  ]

main :: IO ()
main = runSketch (sketch "07-01 Scales and Chords" (World 0 0 (-1) 0 []))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> fireBooked (envTime env) w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

scaleSteps :: World -> [Int]
scaleSteps w = snd (scales !! wScale w)

scaleNotes :: World -> [Int]
scaleNotes w = map (\s -> (wRoot w + s) `mod` 12) (scaleSteps w)

-- 円の中心と半径（p5.js の座標）
circleOf :: Env -> (Float, Float, Float)
circleOf env = (envW env * 0.32, envH env * 0.55, min (envW env) (envH env) * 0.3)

-- 円の i 番目の位置
pos12 :: Env -> Int -> Point
pos12 env i =
  let (cx, cy, r) = circleOf env
      a = -pi / 2 + tau * fromIntegral i / 12
  in (cx + cos a * r, cy + sin a * r)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([scaleLine] ++ map node [0 .. 11] ++ rightSide ++ header)
  where
    inScale = scaleNotes w
    root = wRoot w
    -- ---- 五度圏：i 番目の位置には (i × 7) % 12 の音 ----
    node i =
      let n = (i * 7) `mod` 12         -- ★ 5 度（7 半音）ずつ
          p@(x, y) = pos12 env i
          on = n `elem` inScale
          hot = wHigh w == n && envTime env - wHighTime w < 0.4
          h' | n == root = 45
             | on = 170
             | otherwise = 230
          s' | n == root = 90
             | on = 70
             | otherwise = 30
          b' | hot = 100
             | n == root = 100
             | on = 90
             | otherwise = 30
          (gx, gy) = toG env p
      in Pictures
           [ Color (hsb h' s' b') (at (gx, gy) (disc (if hot then 35 else 28)))
           , txtM env x y 20 (hsb 0 0 (if on || n == root then 10 else 70)) (noteNames !! n)
           ]
    -- 音階の音を線でつなぐ
    scaleLine =
      let ps = [toG env (pos12 env i) | i <- [0 .. 11], ((i * 7) `mod` 12) `elem` inScale]
      in Color (hsba 170 60 100 0.4) (case ps of
           []      -> Blank
           (p : _) -> thickPath 2 (ps ++ [p]))
    -- ---- 右：音階と和音 ----
    rx = envW env * 0.62
    ry = envH env * 0.3
    steps = scaleSteps w
    gaps = zipWith (-) (drop 1 steps ++ [12]) steps
    nameOf s = noteNames !! ((root + s) `mod` 12)
    rightSide =
      [ txt env rx ry 24 white (noteNames !! root ++ " " ++ fst (scales !! wScale w))
      , txt env rx (ry + 44) 18 white ("Scale: " ++ intercalate "  " (map (noteNames !!) inScale))
      , txt env rx (ry + 72) 15 (hsb 0 0 60) ("Semitone gaps: " ++ intercalate " - " (map show gaps))
      ]
      ++ [ txt env rx (ry + 120) 18 white ("Chord (notes 1, 3, 5): " ++ intercalate " + " (map (nameOf . (steps !!)) [0, 2, 4]))
         | length steps >= 5 ]
      ++ zipWith (\k s -> txt env rx (ry + 170 + k * 22) 15 (hsb 0 0 60) s) [0 ..]
           [ "Click a note on the circle: change key"
           , "1-4: scale type"
           , "Space: play the scale"
           , "C: play the chord"
           ]
    header =
      [ txt env 20 40 18 white "Circle of fifths: neighbours have a frequency ratio close to 3:2 (they sound good)."
      , txt env 20 66 18 white "The 7 notes of a major scale form one cluster on the circle."
      ]

-- ★ 音を順に予約する（interval 秒ごと）
playSeq :: Env -> [Int] -> Float -> World -> World
playSeq env notes interval w = w { wBooked = wBooked w ++ zipWith book [0 ..] notes }
  where
    first = case notes of { (n : _) -> n; [] -> 0 }
    book :: Int -> Int -> Booked
    book i n =
      let m = 60 + n + (if i > 0 && n < first then 12 else 0)
      in Booked (envTime env + fromIntegral i * interval) (fromIntegral m) n

-- 時刻が来た音を鳴らす
fireBooked :: Float -> World -> IO World
fireBooked now w =
  let (due, later) = partition (\(Booked t _ _) -> t <= now) (wBooked w)
  in foldM fire w { wBooked = later } due
  where
    fire wa (Booked _ m n) = do
      playTone (440 * 2 ** ((m - 69) / 12)) 0.3 0.5 Triangle
      pure wa { wHigh = n, wHighTime = now }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    pure (foldl (clickNode env (fromG env pos)) w [0 .. 11])
  EventKey (Char c) Down _ _
    | isDigit c, c >= '1', c <= '4' -> pure w { wScale = fromEnum c - fromEnum '1' }
    | c == 'c' || c == 'C' ->
        let steps = scaleSteps w
        in pure (if length steps >= 5
                   then playSeq env (map (\k -> (wRoot w + steps !! k) `mod` 12) [0, 2, 4]) 0 w
                   else w)
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure (playSeq env (scaleNotes w ++ [wRoot w]) 0.25 w)
  _ -> pure w

clickNode :: Env -> Point -> World -> Int -> World
clickNode env m w i
  | dist2 m (pos12 env i) < 30 =
      let root = (i * 7) `mod` 12
      in playSeq env [root] 0 w { wRoot = root }
  | otherwise = w

-- ---------- やってみよう ----------
-- 1. 長音階と短音階を同じ調で聞き比べる。3 番目の音が半音ちがうだけで「明るい／暗い」
-- 2. ペンタトニックは隣り合う 5 つ。だから何を弾いてもぶつからない（黒鍵だけで弾ける F# ペンタトニック）
-- 3. 自分の音階を scales に追加しよう（例：日本の音階「都節」 [0, 1, 5, 7, 8]）

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（上下左右とも中央揃え）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx gy s c str
