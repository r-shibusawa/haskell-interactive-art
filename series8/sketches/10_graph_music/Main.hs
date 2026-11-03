-- ============================================
-- 10 グラフの上を歩く音楽
-- 点 = 音、線 = 「次に行ける音」。サーファーが線をたどって歩くと、グラフの形が曲になる
-- 線をつなぎかえるとメロディのルールが変わる。08 のマルコフ連鎖（シリーズ7）をグラフで描いたもの
-- ドラッグで線を追加 / 点をクリックで線を消す / スペースで再生・停止 / 1〜3 でお手本のグラフ
-- ============================================
module Main (main) where

import Art

data World = World
  { wLinks    :: ![(Int, Int)]
  , wAt       :: !Int           -- 今いる音
  , wFlash    :: !Float         -- 光の残り（0 から 1 へ増える。1 で消える）
  , wPlaying  :: !Bool
  , wDragFrom :: !Int
  , wNextTime :: !Float         -- 次の音を鳴らす時刻
  }

notes :: [String]
notes = ["C4", "D4", "E4", "G4", "A4", "C5", "D5", "E5"]

freqs :: [Float]
freqs = [261.6, 293.7, 329.6, 392.0, 440.0, 523.3, 587.3, 659.3]

beat :: Float
beat = 0.3                 -- ★ 1 音の間隔（秒）

main :: IO ()
main = runSketch (sketch "08-10 Graph Music" (World (preset 1) 0 1 False (-1) 0))
  { skBack  = hsb 260 40 10
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- お手本のグラフ
preset :: Int -> [(Int, Int)]
preset 1 = [(i, (i + 1) `mod` 8) | i <- [0 .. 7]]                               -- 輪：音階を上がり続ける
preset 2 = [(0, 2), (2, 4), (4, 5), (5, 3), (3, 1), (1, 0), (0, 4), (4, 0)]     -- 跳躍
preset _ = concat [[(i, (i + 1) `mod` 8), (i, (i + 7) `mod` 8), (i, i)] | i <- [0 .. 7]]  -- 上下・同じ音

nodePos :: Env -> Int -> Point
nodePos env i =
  let a = tau * fromIntegral i / 8 - pi / 2
  in (envW env / 2 + cos a * 240, envH env * 0.55 + sin a * 240)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let w = w0 { wFlash = min 1 (wFlash w0 + 0.05) }
  if wPlaying w && envTime env >= wNextTime w
    then do
      let out = [b | (a, b) <- wLinks w, a == wAt w]
      next <- if null out then randomRIO (0, 7) else choose out   -- ★ 線がなければランダムに飛ぶ
      playTone (itemAt 261.6 freqs next) 0.3 0.35 Triangle
      pure w { wAt = next, wFlash = 0, wNextTime = wNextTime w + beat }
    else pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map linkPic (wLinks w) ++ dragPic ++ concatMap nodePic [0 .. 7] ++ hud)
  where
    gpos i = let (x, y) = nodePos env i in toG env x y
    linkPic (a, b)
      | a == b =
          let (x, y) = nodePos env a
          in Color (hsb 0 0 50) (at (toG env x (y - 30)) (ring 18 2))
      | otherwise =
          let (ax, ay) = nodePos env a
              (bx, by) = nodePos env b
              ang = atan2 (by - ay) (bx - ax)
              ex = bx - cos ang * 30
              ey = by - sin ang * 30
              tip d = toG env (ex - cos (ang + d) * 12) (ey - sin (ang + d) * 12)
          in Color (hsb 0 0 50) (Pictures
               [ thickLine 2 (toG env ax ay) (toG env ex ey)
               , Polygon [toG env ex ey, tip (-0.4), tip 0.4] ])
    dragPic
      | wDragFrom w >= 0 = [Color (hsb 45 90 100) (thickLine 2 (gpos (wDragFrom w)) (envMouse env))]
      | otherwise = []
    nodePic i =
      let on = wAt w == i && wFlash w < 1
          (gx, gy) = gpos i
      in [ Color (hsb (fromIntegral ((i * 40) `mod` 360)) 70 (if on then 100 else 55)) (at (gx, gy) (disc (if on then 32 else 25)))
         , labelC gx gy 16 (hsb 0 0 10) (itemAt "" notes i) ]
    hud =
      [ txt env 20 32 18 (hsb 0 0 100) ("Drag: add arrow   Click node: delete its arrows   Space: " ++ (if wPlaying w then "stop" else "play"))
      , txt env 20 56 15 (hsb 0 0 100) "1: ring   2: leaps   3: up, down and same note"
      , txt env 20 78 15 (hsb 0 0 60) "One arrow: always go there. Many arrows: pick one at random. The graph's shape = the rules of the tune."
      ]

nearest :: Env -> Point -> Int
nearest env m = case [i | i <- [0 .. 7], dist2 m (nodePos env i) < 30] of
  (i : _) -> i
  [] -> -1

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd m > 80 -> pure w { wDragFrom = nearest env m }
    | otherwise -> pure w
  EventKey (MouseButton LeftButton) Up _ _ -> pure (release (nearest env m))
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure w { wPlaying = not (wPlaying w), wNextTime = envTime env + 0.05 }
  EventKey (Char '1') Down _ _ -> pure w { wLinks = preset 1 }
  EventKey (Char '2') Down _ _ -> pure w { wLinks = preset 2 }
  EventKey (Char '3') Down _ _ -> pure w { wLinks = preset 3 }
  _ -> pure w
  where
    m = mouseXY env
    from = wDragFrom w
    release to
      | from < 0 = w { wDragFrom = -1 }
      | to == from = w { wDragFrom = -1, wLinks = [l | l@(a, _) <- wLinks w, a /= to] }   -- 同じ点でクリック → その点からの線を消す
      | to >= 0 && (from, to) `notElem` wLinks w = w { wDragFrom = -1, wLinks = wLinks w ++ [(from, to)] }
      | otherwise = w { wDragFrom = -1 }

-- ---------- やってみよう ----------
-- 1. 3 のお手本は自分へ戻る矢印（小さな輪）つき。同じ音が続きやすくなる
-- 2. 行き止まりの点を作ると、そこからランダムに飛ぶ。どんな曲になる？
-- 3. 線に「太さ（選ばれやすさ）」をつけると、マルコフ連鎖そのもの

-- ---------- 道具 ----------

-- i 番目の要素（なければ d）
itemAt :: a -> [a] -> Int -> a
itemAt d xs i = case drop i xs of
  (x : _) -> x
  [] -> d

-- リストからランダムに 1 つ選ぶ
choose :: [Int] -> IO Int
choose xs = do
  k <- randomRIO (0, length xs - 1)
  pure (itemAt 0 xs k)

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
