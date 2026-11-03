-- ============================================
-- 12 ルールで作曲する（自由制作のベース）
-- 「音階の中からランダムに」「でも前の音から遠くへは行かない」「4 拍目は休む」…
-- 小さなルールを組み合わせると、聞いていられる音楽になる。映像も音に合わせて動く
-- クリックで再生・停止 / マウス左右 = テンポ / 1〜3 で音階
-- ============================================
-- ★ のついたところを変えて、自分の作曲ルールをつくろう！
module Main (main) where

import Art
import Control.Monad (when)

data Bubble = Bubble
  { bX :: !Float, bY :: !Float, bVy :: !Float, bSize :: !Float, bHue :: !Float, bLife :: !Float }

data World = World
  { wScale   :: !Int        -- 音階の番号（0〜2）
  , wPlaying :: !Bool
  , wNext    :: !Float      -- 次の音を鳴らす時刻（秒）
  , wStep    :: !Int
  , wDegree  :: !Int        -- いま音階の何番目か
  , wBubbles :: ![Bubble]
  }

-- ★ 音階
scales :: [(String, [Int])]
scales =
  [ ("Pentatonic", [0, 2, 4, 7, 9])
  , ("Major scale", [0, 2, 4, 5, 7, 9, 11])
  , ("Miyako-bushi (Japan)", [0, 1, 5, 7, 8])
  ]

main :: IO ()
main = runSketch (sketch "07-12 Generative Music" (World 0 False 0 0 0 []))
  { skBack  = hsb 260 40 8
  , skStep  = \env _ w -> scheduler env (moveBubbles w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- テンポ：マウスの左右で 60〜160
bpmOf :: Env -> Float
bpmOf env = clampF 60 160 (remap (fst (fromG env (envMouse env))) 0 (envW env) 60 160)

stepsOf :: World -> [Int]
stepsOf w = snd (scales !! wScale w)

-- ★ 作曲ルール：次の音の「音階の番号」を返す
nextDegree :: Int -> Int -> Int -> IO Int
nextDegree n prev idx
  | idx `mod` 8 == 0 = pure 0                       -- ★ 2 拍ごとに根音に戻る（安定感）
  | otherwise = do
      let moves = [-2, -1, -1, 0, 1, 1, 2]          -- ★ 近い音へ行きやすい
      i <- randomRIO (0, length moves - 1)
      pure (max 0 (min (n * 2 - 1) (prev + moves !! i)))   -- 2 オクターブぶん

-- ★ リズムのルール：この位置で鳴らすか
shouldPlay :: Int -> IO Bool
shouldPlay idx
  | idx `mod` 16 == 15 = pure False                 -- ★ 4 拍目の裏は休む
  | otherwise = (< (0.8 :: Float)) <$> randomRIO (0, 1)   -- ★ 8 割の確率で鳴らす

-- 粒：音ごとに生まれ、ゆっくり落ちる
moveBubbles :: World -> World
moveBubbles w = w { wBubbles = [ b { bY = bY b + bVy b, bVy = bVy b + 0.05, bLife = bLife b - 1 }   -- ★ 重力
                               | b <- wBubbles w, bLife b > 1 ] }

-- 時刻が来たら次の音を鳴らす
scheduler :: Env -> World -> IO World
scheduler env w
  | wPlaying w && wNext w <= envTime env = do
      let dur = 60 / bpmOf env / 4                  -- 16 分音符
          idx = wStep w
          st = stepsOf w
          n = length st
          beat = idx `mod` 4 == 0
      d <- nextDegree n (wDegree w) idx
      playIt <- shouldPlay idx
      when playIt $ do
        let midi = 60 + st !! (d `mod` n) + 12 * (d `div` n)
            len = if beat then dur * 3 else dur * 1.5   -- ★ 拍の頭は長く
        playTone (hzOf midi) (if beat then 0.3 else 0.18) len Triangle   -- ★ 音色
      -- ★ ベース：1 拍ごとに根音
      when beat (playTone (hzOf 48) 0.15 (dur * 3.5) Sine)
      let top = fromIntegral (max 10 (n * 2 - 1))   -- どの音階でも画面に収まるように
          bubble = Bubble (remap (fromIntegral d) 0 top 100 (envW env - 100)) (envH env * 0.3) (-2)
                          (if beat then 40 else 20) (wrapF 360 (fromIntegral d * 36)) 100
      scheduler env w
        { wDegree = d
        , wBubbles = if playIt then bubble : wBubbles w else wBubbles w
        , wStep = idx + 1
        , wNext = wNext w + dur }
  | otherwise = pure w

hzOf :: Int -> Float
hzOf m = 440 * 2 ** (fromIntegral (m - 69) / 12)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concatMap bubble (wBubbles w) ++ hud)
  where
    -- うすい残像を 3 つ重ねる（p5.js は背景を半透明で塗って残している）
    bubble b =
      [ Color (hsba (bHue b) 70 100 (clampF 0 1 (bLife b / 100) * 0.6 ** k))
              (at (toG env (bX b, bY b - bVy b * k)) (disc (bSize b / 2)))
      | k <- [3, 2, 1, 0] ]
    hud =
      [ txt env 20 40 18 white
          (fst (scales !! wScale w) ++ " (1-3)   Tempo " ++ show (round (bpmOf env) :: Int) ++ " (mouse X)   Click: "
           ++ (if wPlaying w then "stop" else "play") ++ "   Step " ++ show (wStep w))
      , txt env 20 68 15 (hsb 0 0 60) "Rules: scale notes only / nearby notes / root every 2 beats / rest on the last 16th / 80% chance to play"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let on = not (wPlaying w)
    in pure (if on then w { wPlaying = True, wNext = envTime env + 0.05 } else w { wPlaying = False })
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure w { wScale = fromEnum c - fromEnum '1' }
  _ -> pure w

-- ---------- 自由制作のアイデア ----------
-- ・nextDegree のルールを変える：「必ず上がる」「3 回同じ音が続いたら跳ぶ」
-- ・shouldPlay をユークリッド・リズム（03）にする
-- ・08 のマルコフ連鎖で次の音を決める
-- ・06 のクロマで、聞こえた音を音階に選ぶ（声に合わせて伴奏）
-- ・粒を、音の高さで色、長さで大きさに

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
