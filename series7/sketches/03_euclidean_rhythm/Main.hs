-- ============================================
-- 03 ユークリッド・リズム（世界の民族音楽のリズムが 1 つの式で）
-- n 個のマスに k 個の音を「できるだけ均等に」配る。ユークリッドの互除法と同じ計算
-- E(3,8) = キューバのトレシージョ、E(5,8) = キンティージョ、E(7,12) = 西アフリカのベル…
-- マウス左右 = 音の数 k / 上下 = マスの数 n / クリックで再生・停止 / 1〜4 でお手本
-- ============================================
module Main (main) where

import Art
import Data.List (intersperse, elemIndex, foldl')

data World = World
  { wN       :: !Int          -- マスの数
  , wK       :: !Int          -- 音の数
  , wPos     :: !Int          -- いま鳴らしているマス
  , wNext    :: !Float        -- 次のマスを鳴らす時刻（秒）
  , wPlaying :: !Bool
  , wMouseKN :: !(Int, Int)   -- マウスの位置から求めた (k, n) の前回の値
  }

bpm :: Float
bpm = 100                  -- ★ テンポ

main :: IO ()
main = runSketch (sketch "07-03 Euclidean Rhythm" (World 8 3 0 0 False (-1, -1)))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> scheduler (envTime env) (followMouse env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ ビョークランドのアルゴリズム（簡易版）：k 個の 1 を n マスに均等に散らす
euclid :: Int -> Int -> [Bool]
euclid k n = rotateToFirst (reverse (snd (foldl' place (0, []) [1 .. n])))
  where
    place (bucket, out) _ =
      let b = bucket + k
      in if b >= n then (b - n, True : out) else (b, False : out)
    -- 最初が 1 になるように回転
    rotateToFirst out = case elemIndex True out of
      Just i  -> drop i out ++ take i out
      Nothing -> out

-- マウスが動いて値が変わったときだけ k と n を変える（1〜4 のお手本を消さないため）
followMouse :: Env -> World -> World
followMouse env w
  | envDown env || mk == wMouseKN w = w
  | otherwise = w { wN = nn, wK = min nk nn, wMouseKN = mk }
  where
    (mx, my) = fromG env (envMouse env)
    nk = floor (clampF 1 13 (remap mx 0 (envW env) 1 13)) :: Int
    nn = floor (clampF 4 16 (remap my 0 (envH env) 16 4)) :: Int
    mk = (nk, nn)

-- ★ 時刻が来たら次のマスへ進んで鳴らす
scheduler :: Float -> World -> IO World
scheduler now w
  | wPlaying w && wNext w <= now = do
      let s = (wPos w + 1) `mod` wN w
          on = case drop s (euclid (wK w) (wN w)) of
                 (p : _) -> p
                 []      -> False
      if on then hitNote (if s == 0 then 220 else 440) else tickNote
      scheduler now w { wPos = s, wNext = wNext w + 60 / bpm / 2 }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([outline, shape] ++ dots ++ right ++ hud)
  where
    k = wK w
    n = wN w
    pattern_ = euclid k n
    cx = envW env * 0.35
    cy = envH env * 0.55
    r = min (envW env) (envH env) * 0.3
    posOf :: Int -> Point
    posOf i =
      let a = -pi / 2 + tau * fromIntegral i / fromIntegral n
      in toG env (cx + cos a * r, cy + sin a * r)
    outline = Color (hsb 0 0 30) (at (toG env (cx, cy)) (ring r 2))
    -- 音どうしをつなぐ多角形
    shape =
      let ps = [posOf i | (i, True) <- zip [0 ..] pattern_]
      in Color (hsba 170 60 100 0.5) (case ps of
           []      -> Blank
           (p : _) -> thickPath 2 (ps ++ [p]))
    dots =
      [ Color (hsb (if on then 45 else 260) (if on then 90 else 30)
                   (if now then 100 else if on then 85 else 25))
              (at (posOf i) (disc (if now then 22 else if on then 17 else 9)))
      | (i, on) <- zip [0 ..] pattern_
      , let now = wPlaying w && i == wPos w ]
    -- ---- 右：パターンを文字で ----
    rx = envW env * 0.62
    ry = envH env * 0.3
    known = lookup (k, n)
      [ ((3, 8), "Tresillo (Cuba)"), ((5, 8), "Cinquillo (Cuba)")
      , ((7, 12), "Bell pattern (West Africa)"), ((5, 12), "Bembe (West Africa)")
      , ((4, 9), "Aksak (Turkey)"), ((5, 16), "Bossa nova")
      , ((2, 5), "Caribbean"), ((7, 16), "Samba") ]
    right =
      [ txt env rx ry 30 white ("E(" ++ show k ++ ", " ++ show n ++ ")")
      , txt env rx (ry + 50) (if n > 12 then 20 else 26) (hsb 45 90 100)
          (intersperse ' ' (map (\p -> if p then 'x' else '.') pattern_))
      , maybe Blank (\s -> txt env rx (ry + 90) 15 (hsb 0 0 60) ("-> " ++ s)) known
      ]
      ++ zipWith (\i s -> txt env rx (ry + 140 + i * 22) 15 (hsb 0 0 60) s) [0 ..]
           [ "1-4: E(3,8) E(5,8) E(7,12) E(5,16)"
           , "Click: play / stop"
           , ""
           , "Look at the gaps between the notes: nearly"
           , "equal gaps like 2, 3 alternate. This is the"
           , "same calculation as Euclid's GCD algorithm."
           ]
    hud = [txt env 20 40 18 white "Mouse X = notes k, Y = cells n: spread k notes over n cells as evenly as possible"]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let on = not (wPlaying w)
    in pure (if on then w { wPlaying = True, wNext = envTime env + 0.05, wPos = -1 }
                   else w { wPlaying = False })
  EventKey (Char c) Down _ _ -> pure (case lookup c presets of
    Just (k, n) -> w { wK = k, wN = n }
    Nothing     -> w)
  _ -> pure w
  where presets = [('1', (3, 8)), ('2', (5, 8)), ('3', (7, 12)), ('4', (5, 16))]

-- ---------- やってみよう ----------
-- 1. E(3,8) を聞いて、手を叩いてみよう。「タン・タ・タン」のラテンのリズム
-- 2. k と n が割り切れるとき（E(4,8)）は等間隔でつまらない。割り切れないときが面白い
-- 3. 2 つのユークリッド・リズムを重ねる（E(3,8) ＋ E(5,8)）

-- ---------- 道具 ----------

hitNote :: Float -> IO ()
hitNote f = playTone f 0.5 0.25 Triangle

tickNote :: IO ()
tickNote = playTone 2000 0.05 0.03 Sine

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str
