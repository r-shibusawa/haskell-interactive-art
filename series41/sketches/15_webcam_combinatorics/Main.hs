-- ============================================
-- 41-15 カメラ映像で数える（おまけ）
-- 映像を粗いセルに分け、明るいセルが k 個あれば「そこから r 個選ぶ組合せ」は C(k, r) 通り
-- 各行の明るいセルの数を並べて大きい順にすると「k の分割」（ヤング図形）になる
-- 操作：マウス左右 = 明るさのしきい値 / ↑↓ = 選ぶ数 r / N = ノイズ画像に切り替え
--       （カメラが使えないときは自動でノイズ）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import Data.List (intercalate, sortBy)
import qualified Data.Vector.Unboxed as U

cols, rows, vw, vh :: Int
cols = 16               -- ★ セルの横の数
rows = 12               -- ★ セルの縦の数
vw = 64                 -- カメラ画像の幅（4×4 画素で 1 セル）
vh = 48                 -- カメラ画像の高さ

data World = World
  { wCam    :: !Camera
  , wBright :: !(U.Vector Float)   -- 各セルの明るさ 0〜255
  , wR      :: !Int                -- ★ 選ぶ数
  , wNoise  :: !Bool               -- True ならノイズ画像
  , wWaited :: !Float              -- カメラを待った秒数
  , wBlink  :: ![Int]              -- いま光らせている組合せ（セル番号）
  , wTimer  :: !Float
  }

main :: IO ()
main = runSketch (sketch "41-15 Counting with a camera" (error "skStart"))
  { skBack  = hsb 228 28 10
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure (World cam (U.replicate (cols * rows) 0) 3 False 0 [] 0)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- ★ セルの明るさを読む（カメラ）：セルの中の画素の平均
fromCamera :: B.ByteString -> U.Vector Float -> U.Vector Float
fromCamera f old
  | B.length f < vw * vh * 3 = old
  | otherwise = U.generate (cols * rows) cell
  where
    bw = vw `div` cols
    bh = vh `div` rows
    cell i =
      let cx = i `mod` cols
          cy = i `div` cols
          px x y = let o = (x + y * vw) * 3
                       g k = fromIntegral (B.index f (o + k)) :: Float
                   in (g 0 + g 1 + g 2) / 3
          vals = [px x y | y <- [cy * bh .. (cy + 1) * bh - 1], x <- [cx * bw .. (cx + 1) * bw - 1]]
      in sum vals / fromIntegral (bw * bh)

-- ★ セルの明るさを読む（ノイズ）
fromNoise :: Float -> U.Vector Float
fromNoise time =
  let t = time * 0.4
  in U.generate (cols * rows) (\i ->
       noise3 (fromIntegral (i `mod` cols) * 0.25) (fromIntegral (i `div` cols) * 0.25) t * 255)

thresholdOf :: Env -> Float
thresholdOf env = clampF 60 220 (remap (fst (mouseP env)) 0 (envW env) 60 220)

onCells :: Env -> World -> [Int]
onCells env w = let thr = thresholdOf env in [i | (i, b) <- zip [0 ..] (U.toList (wBright w)), b > thr]

step :: Env -> Float -> World -> IO World
step env dt w0 = do
  live <- camLive (wCam w0)
  -- カメラが来ないまま 4 秒たったらノイズに切り替える
  let waited = if not (wNoise w0) && not live then wWaited w0 + dt else wWaited w0
      useNoise = wNoise w0 || (not live && waited > 4)
  bright <- if live && not useNoise
              then (\f -> fromCamera f (wBright w0)) <$> cameraFrame (wCam w0)
              else pure (fromNoise (envTime env))
  let w = w0 { wWaited = waited, wNoise = useNoise, wBright = bright }
      t = wTimer w + dt
  -- ランダムな r 個の組合せを点滅
  if t > 0.25
    then do
      b <- pickSubset (onCells env w) (wR w)
      pure w { wTimer = 0, wBlink = b }
    else pure w { wTimer = t }

-- ★ 巨大な数も正確に（Integer の二項係数）
chooseBig :: Int -> Int -> Integer
chooseBig a b
  | b < 0 || b > a = 0
  | otherwise = foldl (\res i -> res * fromIntegral (a - i) `div` fromIntegral (i + 1)) 1 [0 .. b - 1]

fmtBig :: Integer -> String
fmtBig v =
  let s = show v
  in if length s <= 12 then commas s
     else take 1 s ++ "." ++ take 3 (drop 1 s) ++ " x 10^" ++ show (length s - 1) ++ " (" ++ show (length s) ++ " digits)"

draw :: Env -> World -> Picture
draw env w =
  let thr = thresholdOf env
      cs = min (envW env * 0.55 / fromIntegral cols) ((envH env - 200) / fromIntegral rows)
      ox = 30
      oy = 110
      cellAt i = (ox + fromIntegral (i `mod` cols) * cs, oy + fromIntegral (i `div` cols) * cs)
      cell (i, b) =
        let (x, y) = cellAt i
            on = b > thr
            c = if on then hsb 50 80 (60 + b / 255 * 40) else hsb 220 20 (15 + b / 255 * 25)
        in box env c x y (cs - 2) (cs - 2)
      on = onCells env w
      k = length on
      r = wR w
      total = cols * rows
      blinkPic i =
        let (x, y) = cellAt i
        in Color (hsb 330 80 100) (frameBox env 3 (x + 1) (y + 1) (cs - 4) (cs - 4))
      px = envW env * 0.62
      py = 130
      -- 分割：各行の明るいセル数 → 降順 → ヤング図形
      rowCounts = [length (filter (\i -> i `div` cols == cy) on) | cy <- [0 .. rows - 1]]
      lam = sortBy (flip compare) (filter (> 0) rowCounts)
      ys = min 14 ((envH env - py - 340) / fromIntegral (max 1 (length lam)))
      young = [ box env (hsb (wrapF 360 (fromIntegral i * 30 + 180)) 55 95)
                    (px + fromIntegral j * ys) (py + 278 + fromIntegral i * ys) (ys - 2) (ys - 2)
              | (i, v) <- zip [0 :: Int ..] lam, j <- [0 .. v - 1] ]
      plain dy s = txt env px (py + dy) 14 (hsb 0 0 70) s
      log2 = round (fromIntegral total * logBase 10 2 :: Float) :: Int
  in Pictures
       [ Pictures (map cell (zip [0 :: Int ..] (U.toList (wBright w))))
       , Pictures (map blinkPic (wBlink w))
       , txt env px py 16 (hsb 0 0 100) ("Bright cells k = " ++ show k ++ " of " ++ show total)
       , txt env px (py + 22) 16 (hsb 0 0 100) ("Threshold " ++ show (round thr :: Int) ++ " (mouse X)")
       , txt env px (py + 58) 22 (hsb 50 80 100) ("C(" ++ show k ++ ", " ++ show r ++ ") =")
       , txt env px (py + 86) 20 (hsb 50 80 100) (fmtBig (chooseBig k r))
       , plain 112 ("Ways to choose r = " ++ show r ++ " (Up/Down) bright cells.")
       , plain 132 "Pink frames show one of them (random)."
       , plain 158 ("Layouts of " ++ show k ++ " bright among " ++ show total ++ " cells:")
       , plain 178 ("C(" ++ show total ++ "," ++ show k ++ ") = " ++ fmtBig (chooseBig total k))
       , plain 198 ("All on/off patterns: 2^" ++ show total ++ " = about 10^" ++ show log2)
       , txt env px (py + 236) 15 (hsb 0 0 100) ("Bright cells per row, sorted = a partition of " ++ show k)
       , txt env px (py + 258) 14 (hsb 0 0 80)
           (if null lam then "(none)" else "(" ++ intercalate ", " (map show lam) ++ ")")
       , Pictures young
       , txt env px (py + 278 + fromIntegral (length lam) * ys + 20) 14 (hsb 0 0 60)
           "Young diagram (09). Lower threshold: longer rows."
       , txt env 20 40 16 (hsb 0 0 100)
           ((if wNoise w then "Noise image" else "Camera") ++ " (N: switch)   bright cells k = " ++ show k
            ++ "   r = " ++ show r ++ " (Up/Down)")
       , txt env 20 68 14 (hsb 0 0 60)
           "Move your hand or face: k changes and the count of combinations explodes or collapses."
       ]

-- リストから r 個をランダムに選ぶ（1 つずつ取り出す）
pickSubset :: [Int] -> Int -> IO [Int]
pickSubset xs cnt
  | cnt <= 0 || null xs = pure []
  | otherwise = do
      j <- randomRIO (0, length xs - 1)
      case splitAt j xs of
        (a, v : b) -> (v :) <$> pickSubset (a ++ b) (cnt - 1)
        _ -> pure []

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wR w < 20 -> w { wR = wR w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wR w > 1 -> w { wR = wR w - 1 }
  EventKey (Char c) Down _ _ | c `elem` "nN" -> w { wNoise = not (wNoise w), wWaited = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. r = k/2 のとき C(k, r) は最大。r を動かして C(k,r) が左右対称に増減するのを見よう（01 とつなぐ）
-- 2. cols, rows を 32×24 にすると C(768, k) は何桁になる？ Integer なら正確に出る
-- 3. 「明るいセルが隣り合うペア」の数を数えてみよう（グラフの辺の数。10 のラムゼーとつなぐ）
-- 4. 点滅する組合せを「ヤング図形の形」や「格子路（02）」に置きかえて、映像から別の数え方を作ろう

-- ---------- 道具 ----------
-- 3 桁ごとにカンマを入れる
commas :: String -> String
commas s = reverse (go (reverse s))
  where
    go t = case splitAt 3 t of
      (a, []) -> a
      (a, b) -> a ++ "," ++ go b

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 太さ t の枠
frameBox :: Env -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env t x y w h =
  let a = toG env x y
      b = toG env (x + w) y
      c = toG env (x + w) (y + h)
      d = toG env x (y + h)
  in thickPath t [a, b, c, d, a]

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
