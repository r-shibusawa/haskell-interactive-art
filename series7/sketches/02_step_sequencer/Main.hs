-- ============================================
-- 02 ステップシーケンサー（リズムを打ち込む）
-- 16 マス × 4 段のグリッド。マスを ON にすると、再生バーが通ったときに鳴る
-- 1 マス = 16 分音符。4 マスで 1 拍、16 マスで 1 小節（4/4 拍子）
-- マスをクリックで ON/OFF / 上の帯をドラッグ = テンポ / R でランダム / C で消す / スペースで再生・停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, forM_, when)
import qualified Data.Vector.Unboxed as U

data World = World
  { wGrid    :: !(U.Vector Bool)  -- tracks × steps のマス
  , wBpm     :: !Float
  , wHead    :: !Int              -- 再生バーの位置（-1 = 止まっている）
  , wNext    :: !Float            -- 次のマスを鳴らす時刻（秒）
  , wPlaying :: !Bool
  }

steps, tracks :: Int
steps = 16                 -- ★ 12 にすると 3 拍子（ワルツ）になる
tracks = 4

names :: [String]
names = ["Kick", "Snare", "Hi-hat", "Clap"]

-- お手本：基本の 8 ビート
initialGrid :: U.Vector Bool
initialGrid = U.fromList (map (== (1 :: Int)) (concat
  [ [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]
  , [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0]
  , [0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]))

main :: IO ()
main = runSketch (sketch "07-02 Step Sequencer" (World initialGrid 120 (-1) 0 False))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> scheduler (envTime env) w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

cellAt :: World -> Int -> Int -> Bool
cellAt w t s = wGrid w U.! (t * steps + s)

x0, y0 :: Float
x0 = 130
y0 = 120

-- マスの幅と高さ
cellSize :: Env -> (Float, Float)
cellSize env = ((envW env - 160) / fromIntegral steps, min 90 ((envH env - 260) / fromIntegral tracks))

-- ★ 時刻が来たら次のマスへ進んで鳴らす
scheduler :: Float -> World -> IO World
scheduler now w
  | wPlaying w && wNext w <= now = do
      let s = (wHead w + 1) `mod` steps
      forM_ [0 .. tracks - 1] (\t -> when (cellAt w t s) (drum t))
      scheduler now w { wHead = s, wNext = wNext w + 60 / wBpm w / 4 }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (tempo ++ cells ++ bar ++ beats ++ hud)
  where
    (cellW, cellH) = cellSize env
    rowY t = y0 + fromIntegral t * (cellH + 6)
    tempo =
      [ Color (hsb 0 0 20) (box env x0 80 (envW env - 160) 14)
      , Color (hsb 45 90 100) (box env x0 80 (remap (wBpm w) 60 200 0 (envW env - 160)) 14)
      ]
    cells = concat
      [ txt env 20 (rowY t + cellH / 2 + 6) 16 white name
        : [ Color (cellColor t s) (box env (x0 + fromIntegral s * cellW + 2) (rowY t) (cellW - 4) cellH)
          | s <- [0 .. steps - 1] ]
      | (t, name) <- zip [0 ..] names ]
    cellColor :: Int -> Int -> Color
    cellColor t s
      | cellAt w t s = hsb (fromIntegral t * 60 + 20) 80 (if s == wHead w then 100 else 75)
      | s == wHead w = hsb 260 30 30
      | s `mod` 4 == 0 = hsb 260 30 20
      | otherwise = hsb 260 30 15
    -- 再生バー
    bar =
      [ Color (hsba 0 0 100 0.2)
          (box env (x0 + fromIntegral (wHead w) * cellW) (y0 - 6) cellW (fromIntegral tracks * (cellH + 6)))
      | wHead w >= 0 ]
    -- 拍の番号
    beats =
      [ txt env (x0 + fromIntegral s * cellW + 4) (rowY tracks + 20) 14 (hsb 0 0 60)
          ("Beat " ++ show (s `div` 4 + 1))
      | s <- [0, 4 .. steps - 1] ]
    hud =
      [ txt env 20 40 18 white
          ("Click cells: on/off   Top bar: tempo " ++ show (round (wBpm w) :: Int) ++ " BPM   Space: "
           ++ (if wPlaying w then "stop" else "play") ++ "   R: random   C: clear")
      , txt env 20 64 15 (hsb 0 0 60) "1 cell = 16th note (1/4 beat). BPM = beats per minute. At 120 BPM, 1 beat = 0.5 s"
      ]

-- 上の帯の上ならテンポを変える
tempoAt :: Env -> Point -> Maybe Float
tempoAt env (mx, my)
  | my > 74 && my < 100 && mx > x0 = Just (clampF 60 200 (remap mx x0 (envW env - 30) 60 200))
  | otherwise = Nothing

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(mx, my) = fromG env pos
        (cellW, cellH) = cellSize env
        s = floor ((mx - x0) / cellW) :: Int
        t = floor ((my - y0) / (cellH + 6)) :: Int
    in case tempoAt env m of
         Just b -> pure w { wBpm = b }
         Nothing
           | s >= 0 && s < steps && t >= 0 && t < tracks -> do
               let on = not (cellAt w t s)
               when on (drum t)
               pure w { wGrid = wGrid w U.// [(t * steps + s, on)] }
           | otherwise -> pure w
  EventMotion pos | envDown env ->
    pure (maybe w (\b -> w { wBpm = b }) (tempoAt env (fromG env pos)))
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let on = not (wPlaying w)
    in pure (if on then w { wPlaying = True, wNext = envTime env + 0.05, wHead = -1 }
                   else w { wPlaying = False })
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> do
        cells <- forM [1 .. tracks * steps] (\_ -> (< (0.25 :: Float)) <$> randomRIO (0, 1))
        pure w { wGrid = U.fromList cells }
    | c == 'c' || c == 'C' -> pure w { wGrid = U.map (const False) (wGrid w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. キックを 1・3 拍、スネアを 2・4 拍に置く（ロックの基本）。キックを 4 つ全部に（ダンス）
-- 2. ハイハットを「裏拍」（2, 6, 10, 14 マス）だけにすると、ノリが変わる
-- 3. steps を 12 にすると 3 拍子（ワルツ）になる（initialGrid も 12 マスずつに）

-- ---------- 道具 ----------

-- ★ ドラムの音。Art.Sound は決まった高さの波しか鳴らせないので、高い音や四角い波でノイズの感じを出す
drum :: Int -> IO ()
drum t = case t of
  0 -> playTone 60 0.9 0.35 Sine                                        -- キック
  1 -> playTone 1800 0.25 0.15 Square >> playTone 190 0.4 0.1 Triangle  -- スネア
  2 -> playTone 7000 0.15 0.05 Square                                   -- ハイハット
  _ -> playTone 1500 0.4 0.12 Sawtooth                                  -- クラップ

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
