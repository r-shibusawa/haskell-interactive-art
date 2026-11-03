-- ============================================
-- 10 準結晶と貴金属比の置換 ─ フィボナッチ語
-- 置換ルール A → A…AB（A が n 個）、B → A をくり返すと、周期のない（でも秩序のある）並びができる
--   n=1：A → AB, B → A（フィボナッチ語）/ n=2：A → AAB / n=3：A → AAAB
-- 長いタイル A = φn、短いタイル B = 1 とすると 1 次元の準結晶。下の「回折像」には鋭いピークが立つ
-- ←→ = n / ↑↓ = 置換の回数 / スペース = 1 段ずつ育つアニメーション / P = 周期的な並びと比べる
-- ============================================
module Main (main) where

import Art
import Data.List (mapAccumL)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wN         :: !Int           -- ★ 置換の n
  , wLevel     :: !Int           -- ★ 何回置換するか
  , wWords     :: [String]       -- 各段の文字列
  , wRows      :: [Picture]      -- 各段の絵（文字列が変わったときだけ作り直す）
  , wRowsFor   :: !(Float, Float) -- wRows を作ったときの画面の大きさ
  , wAnim      :: !Int           -- アニメーション（何段目まで表示するか）
  , wAnimating :: !Bool
  , wPeriodic  :: !Bool          -- P：ABABAB… の周期的な並び
  , wDiff      :: !(U.Vector Float) -- 回折像（計算は n・段が変わったときだけ）
  , wTimer     :: !Int
  }

initialWorld :: World
initialWorld = World 1 7 [] [] (0, 0) 0 True False U.empty 0

main :: IO ()
main = runSketch (sketch "33-10 Substitution Words" initialWorld)
  { skBack  = hsb 170 30 9
  , skStart = \env -> pure (rebuild env initialWorld)
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- 置換で語を作り、回折像と絵も作り直す
rebuild :: Env -> World -> World
rebuild env w =
  let rule = replicate (wN w) 'A' ++ "B"
      subst = concatMap (\ch -> if ch == 'A' then rule else "A")
      grow :: Int -> String -> [String]
      grow i s
        | i >= wLevel w = [s]
        | length next > 6000 = [s]
        | otherwise = s : grow (i + 1) next
        where next = subst s
      ws = grow 0 "A"
      w1 = w { wWords = ws }
  in computeDiffraction (buildRows env w1)

-- 各段をタイルで描く。段 i の全長を画面幅にそろえる（= 1 段ごとに 1/φ 倍に縮めて見ている）
buildRows :: Env -> World -> World
buildRows env w = w { wRows = zipWith row [0 ..] (wWords w), wRowsFor = (envW env, envH env) }
  where
    phi = metallic (fromIntegral (wN w))
    x0 = 30
    bigW = envW env - 350
    rowH = rowHeight env (length (wWords w))
    row :: Int -> String -> Picture
    row i s =
      let a = count 'A' s
          b = count 'B' s
          total = fromIntegral a * phi + fromIntegral b
          y = 100 + fromIntegral i * (rowH + 6)
          -- 長すぎる段は先頭だけを拡大して描く
          sc = if length s > 2500 then bigW / (phi * 600) else bigW / total
          tiles = go x0 s
          go _ [] = []
          go x (ch : r)
            | x >= x0 + bigW = []
            | otherwise =
                let l = (if ch == 'A' then phi else 1) * sc
                    col = if ch == 'A' then hsb 45 70 95 else hsb 200 70 95
                    tile = Color col (box env x y (max 0.6 (l - (if l > 4 then 1 else 0))) rowH)
                    letter = [ txt env (x + l / 2 - 5) (y + rowH / 2 + 6) 14 (hsb 0 0 10) [ch] | l > 18, rowH >= 16 ]
                in tile : letter ++ go (x + l) r
          info = show a ++ "A " ++ show b ++ "B" ++ (if b > 0 then " r=" ++ nf 4 (fromIntegral a / fromIntegral b) else "")
      in Pictures (tiles ++ [ txt env (x0 + bigW + 12) (y + rowH / 2 + 6) 14 (gray 0.8) info ])

rowHeight :: Env -> Int -> Float
rowHeight env len = min 34 ((envH env * 0.5 - 100) / fromIntegral (max 1 len))

-- 回折像を計算（境目の点が多いので最大 1500 点、波数 400 本）
computeDiffraction :: World -> World
computeDiffraction w = w { wDiff = U.map (\v -> realToFrac (sqrt (v / mx))) raw }
  where
    lastWord = case reverse (wWords w) of
      (s : _) -> s
      [] -> "A"
    tiles | wPeriodic w = take 1500 (cycle "AB")
          | otherwise = take 1500 lastWord
    phi = realToFrac (metallic (fromIntegral (wN w))) :: Double
    (total, positions) = mapAccumL (\x ch -> (x + (if ch == 'A' then phi else 1), x)) 0 tiles
    xs = U.fromList positions
    len = fromIntegral (U.length xs) :: Double
    mean = total / len        -- タイル 1 枚の平均の長さ
    bigK = 400 :: Int
    raw :: U.Vector Double
    raw = U.generate bigK $ \i0 ->
      let k = (fromIntegral (i0 + 1) / fromIntegral bigK) * 3 / mean     -- 波数 0〜3 / 平均長さ
          w2 = 2 * pi * k
          re = U.foldl' (\s p -> s + cos (w2 * p)) 0 xs
          im = U.foldl' (\s p -> s + sin (w2 * p)) 0 xs
      in (re * re + im * im) / len
    mx = max 1e-9 (U.maximum raw)

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let w = if wRowsFor w0 /= (envW env, envH env) then buildRows env w0 else w0
      lastRow = length (wWords w) - 1
  in if wAnimating w
       then if wTimer w + 1 > 35
              then w { wTimer = 0, wAnim = if wAnim w >= lastRow then 0 else wAnim w + 1 }
              else w { wTimer = wTimer w + 1 }
       else w { wAnim = lastRow }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (hud ++ take (wAnim w + 1) (wRows w) ++ diffPic)
  where
    n = wN w
    phi = metallic (fromIntegral n)
    hud =
      [ txt env 20 24 15 white ("A -> " ++ replicate n 'A' ++ "B, B -> A   n = " ++ show n ++ " (L/R)   substitutions " ++ show (length (wWords w) - 1) ++ " (Up/Dn)   Space anim " ++ onOff (wAnimating w) ++ "   P periodic " ++ onOff (wPeriodic w))
      , txt env 20 46 14 (gray 0.75) "Widen each row by phi and swap tiles to get the next row (self-similar)."
      , txt env 20 68 14 (gray 0.75) ("The count ratio r = A/B approaches phi" ++ show n ++ " = " ++ nf 5 phi ++ ".")
      ]
    -- 回折像（フーリエ変換の強さ）
    gy = envH env * 0.58
    gh = envH env * 0.36
    gx = 30
    gw = envW env - 60
    cnt = U.length (wDiff w)
    diffPic =
      [ Color (withA 0.18 white) (Line (map (uncurry (pt env)) [(gx, gy), (gx + gw, gy), (gx + gw, gy + gh), (gx, gy + gh), (gx, gy)])) ]
      ++ [ Color (if wPeriodic w then hsb 330 60 100 else hsb 50 80 100)
             (Pictures
               [ thickLine 1.5 (pt env x (gy + gh - 10)) (pt env x (gy + gh - 10 - v * (gh - 40)))
               | (i, v) <- zip [0 :: Int ..] (U.toList (wDiff w))
               , let x = gx + 10 + fromIntegral i * (gw - 20) / fromIntegral (max 1 cnt) ])
         , txt env (gx + 10) (gy + 22) 14 (gray 0.8)
             ("Diffraction |sum e^(2 pi i k x)|^2: sharp peaks = long-range order. " ++
              (if wPeriodic w then "ABAB...: evenly spaced." else "Quasicrystal: spaced by phi."))
         ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> (rebuild env w { wN = min (wN w + 1) 4 }) { wAnim = 0 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> (rebuild env w { wN = max (wN w - 1) 1 }) { wAnim = 0 }
  EventKey (SpecialKey KeyUp) Down _ _    -> rebuild env w { wLevel = min (wLevel w + 1) 12 }
  EventKey (SpecialKey KeyDown) Down _ _  -> (rebuild env w { wLevel = max (wLevel w - 1) 1 }) { wAnim = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAnimating = not (wAnimating w) }
  EventKey (Char c) Down _ _
    | c == 'p' || c == 'P' -> computeDiffraction w { wPeriodic = not (wPeriodic w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 1 で、各段の文字数 1, 2, 3, 5, 8, 13 … を確かめよう（フィボナッチ数）
-- 2. フィボナッチ語に「BB」や「AAA」は出てくる？ 探してみよう
-- 3. ルールを A → ABA, B → A などに変えると、比は何に近づく？
-- 4. 13 の音のスケッチは、この並びをリズムにして鳴らしている

-- ---------- 道具 ----------
metallic :: Float -> Float
metallic k = (k + sqrt (k * k + 4)) / 2

count :: Char -> String -> Int
count ch = length . filter (== ch)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]
