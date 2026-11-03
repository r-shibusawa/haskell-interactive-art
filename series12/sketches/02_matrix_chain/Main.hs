-- ============================================
-- 02 行列の連鎖積：かっこの付け方で計算量が何十倍も変わる
-- A1 A2 … An を掛けるとき、どこから掛けるか（かっこの付け方）で掛け算の回数が変わる
-- m[i][j] = min over k ( m[i][k] + m[k+1][j] + p[i-1]·p[k]·p[j] )　短い連鎖から順に表を埋める
-- スペース = 再生・停止 / → = 1 手 / 1〜3 = お手本 / R = ランダムな行列 / ↑↓ = 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

startP :: [Int]
startP = [50, 5, 40, 10, 30, 2, 25]    -- ★ A1 は 50×5、A2 は 5×40 …（p[i-1]×p[i]）。最適 4,800 回、左から順だと 50,500 回

startFrames :: Int
startFrames = 10                        -- ★ 1 手の間のフレーム数

type Table = M.Map (Int, Int) Int

data World = World
  { wP       :: ![Int]
  , wM       :: !Table
  , wS       :: !Table
  , wL       :: !Int           -- 連鎖の長さ
  , wI       :: !Int
  , wK       :: !Int
  , wBest    :: !(Maybe Int)   -- いまの最小（Infinity の代わりに Nothing）
  , wFrames  :: !Int
  , wPlaying :: !Bool
  , wDone    :: !Bool
  , wTimer   :: !Int
  }

initial :: World
initial = restart (World startP M.empty M.empty 2 1 1 Nothing startFrames True False 0)

main :: IO ()
main = runSketch (sketch "12-02 Matrix chain" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> onEvent ev w
  }

sizeN :: World -> Int
sizeN w = length (wP w) - 1

pDim :: World -> Int -> Int
pDim w = nth 1 (wP w)

restart :: World -> World
restart w = w
  { wM = M.fromList [((a, a), 0) | a <- [1 .. n]], wS = M.empty
  , wL = 2, wI = 1, wK = 1, wBest = Nothing, wDone = n < 2 }
  where n = sizeN w

mAt :: World -> Int -> Int -> Int
mAt w a b = M.findWithDefault 0 (a, b) (wM w)

-- ★ 切れ目 k で分けたときの掛け算の回数
costAt :: World -> Int -> Int -> Int -> Int
costAt w i k j = mAt w i k + mAt w (k + 1) j + pDim w (i - 1) * pDim w k * pDim w j

-- 1 手：切れ目 k を 1 つためす
advance :: World -> World
advance w
  | wDone w = w
  | k1 < j = better { wK = k1 }
  | otherwise =
      let m1 = M.insert (i, j) (maybe 0 id (wBest better)) (wM better)
          i1 = i + 1
          (l2, i2) = if i1 > n - l + 1 then (l + 1, 1) else (l, i1)
      in if l2 > n
           then better { wM = m1, wBest = Nothing, wI = i2, wL = l2, wK = k1, wDone = True }
           else better { wM = m1, wBest = Nothing, wI = i2, wL = l2, wK = i2 }
  where
    n = sizeN w
    l = wL w
    i = wI w
    k = wK w
    j = i + l - 1
    q = costAt w i k j
    better = if maybe True (q <) (wBest w)
               then w { wBest = Just q, wS = M.insert (i, j) k (wS w) }
               else w
    k1 = k + 1

step :: World -> World
step w
  | wPlaying w && not (wDone w) =
      if wTimer w + 1 >= wFrames w then advance w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> toggle
  EventKey (MouseButton LeftButton) Down _ _ -> toggle
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wFrames = max 1 (wFrames w - 3) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wFrames = min 40 (wFrames w + 3) }
  EventKey (Char '1') Down _ _ -> pure (restart w { wP = startP })
  EventKey (Char '2') Down _ _ -> pure (restart w { wP = [20, 2, 30, 12] })  -- 3 個だけ：(A1 A2) A3 は 8,400 回、A1 (A2 A3) は 1,200 回
  EventKey (Char '3') Down _ _ -> pure (restart w { wP = [8, 12, 4, 15, 6, 40, 9, 18] })
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    k <- randomRIO (3, 8 :: Int)
    p <- replicateM (k + 1) ((* 5) <$> randomRIO (2, 11))
    pure (restart w { wP = p }) { wPlaying = True }
  _ -> pure w
  where toggle = pure w { wPlaying = not (wPlaying w) }

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  [ txt 26 cFg 32 24 ("Matrix chain: in what order to multiply " ++ show n ++ " matrices")
  , txt 15 cMuted 32 60 "Space: play/pause   Right: 1 step   1-3: samples   R: random   Up/Down: speed"
  , drawMatrices w 100
  , drawTable w 40 gy cellW cellH "m[i][j]: fewest multiplications" True
  , drawTable w (envW env / 2 + 20) gy cellW cellH "s[i][j]: last split k" False
  , bottom
  ]
  where
    n = max 1 (sizeN w)
    gy = 290
    cellW = min 80 ((envW env / 2 - 80) / i2f n)     -- 5 桁の数字が入るよう横長にする
    cellH = min 64 ((envH env - 470) / i2f n)
    ty = gy + cellH * i2f n + 40
    i = wI w
    k = wK w
    j = i + wL w - 1
    q = costAt w i k j
    p = pDim w
    bottom
      | not (wDone w) = Pictures
          [ txt 17 cFg 40 ty ("Chain length " ++ show (wL w) ++ ": computing m[" ++ show i ++ "][" ++ show j ++ "], split k = " ++ show k)
          , txt 17 cCyan 40 (ty + 30) ("m[" ++ show i ++ "][" ++ show k ++ "] = " ++ show (mAt w i k))
          , txt 17 cRasp 260 (ty + 30) ("m[" ++ show (k + 1) ++ "][" ++ show j ++ "] = " ++ show (mAt w (k + 1) j))
          , txt 17 cAmber 480 (ty + 30)
              ("p" ++ show (i - 1) ++ "*p" ++ show k ++ "*p" ++ show j ++ " = " ++ show (p (i - 1)) ++ "*" ++ show (p k) ++ "*" ++ show (p j)
               ++ " = " ++ show (p (i - 1) * p k * p j))
          , txt 17 cFg 40 (ty + 60) ("Total " ++ show q ++ "  (best so far " ++ maybe "-" show (wBest w) ++ ")") ]
      | sizeN w >= 2 = Pictures
          [ txt 22 cLime 40 ty ("Best order: " ++ paren w 1 (sizeN w))
          , txt 17 cFg 40 (ty + 36) (commas (mAt w 1 (sizeN w)) ++ " multiplications  (left to right: " ++ commas (leftToRight w) ++ ")") ]
      | otherwise = Blank

paren :: World -> Int -> Int -> String
paren w a b
  | a >= b = "A" ++ show a
  | otherwise =
      let s = clampI a (b - 1) (M.findWithDefault a (a, b) (wS w))
      in "(" ++ paren w a s ++ " " ++ paren w (s + 1) b ++ ")"
  where clampI lo hi = max lo . min hi

leftToRight :: World -> Int
leftToRight w = sum [pDim w 0 * pDim w k * pDim w (k + 1) | k <- [1 .. sizeN w - 1]]

-- 3 桁ごとにカンマ（toLocaleString の代わり）
commas :: Int -> String
commas v = reverse (go (reverse (show v)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, b) -> a ++ "," ++ go b

drawMatrices :: World -> Float -> Picture
drawMatrices w y = Pictures (concat (zipWith one [1 .. n] xs))
  where
    n = sizeN w
    mx = maximum (1 : wP w)
    sc = min 1.6 (90 / i2f mx)
    size a = (i2f (pDim w (a - 1)) * sc, i2f (pDim w a) * sc)
    xs = scanl (\x a -> x + max (snd (size a)) 54 + 18) 40 [1 .. n]
    j = wI w + wL w - 1
    one a x =
      let (h, wd) = size a
          inRange = not (wDone w) && a >= wI w && a <= j
          col | inRange = if a <= wK w then cCyan else cRasp
              | otherwise = rgb255 0x5a 0x4a 0x66
      in [ rbox col x (y + (100 - h) / 2) wd h
         , txtC 14 cFg (x + wd / 2) (y + 110) ("A" ++ show a)
         , txtC 14 cMuted (x + wd / 2) (y + 128) (show (pDim w (a - 1)) ++ "x" ++ show (pDim w a)) ]

drawTable :: World -> Float -> Float -> Float -> Float -> String -> Bool -> Picture
drawTable w x0 y0 cellW cellH title isM = Pictures
  ( txt 16 cFg x0 (y0 - 42) title
  : [txtR 14 cMuted (x0 + 26) (y0 + i2f (a - 1) * cellH + cellH / 2) (show a) | a <- [1 .. n]]
  ++ [txtC 14 cMuted (x0 + 30 + i2f (b - 1) * cellW + cellW / 2) (y0 - 12) ("j=" ++ show b) | b <- [1 .. n]]
  ++ concat [cell a b | a <- [1 .. n], b <- [a .. n]] )
  where
    n = sizeN w
    table = if isM then wM w else wS w
    vmax = maximum (1 : M.elems (wM w))
    j = wI w + wL w - 1
    cell a b =
      let x = x0 + 30 + i2f (b - 1) * cellW
          y = y0 + i2f (a - 1) * cellH
          v = M.lookup (a, b) table
          hl | wDone w = Nothing
             | a == wI w && b == j = Just cAmber
             | a == wI w && b == wK w = Just cCyan
             | a == wK w + 1 && b == j = Just cRasp
             | otherwise = Nothing
          blank' = not isM && a == b
          col = case v of
            Just vv | not blank' -> if isM then mixC (rgb255 0x2a 0x20 0x33) (rgb255 0x7a 0x2d 0x4a) (i2f vv / i2f vmax) else cPanel
            _ -> cBg
      in [ cellBox col (maybe cGrid id hl) (maybe 1 (const 3) hl) (x + 2) (y + 2) (cellW - 4) (cellH - 4) ]
         ++ [txtC 14 cFg (x + cellW / 2) (y + cellH / 2) (show vv) | Just vv <- [v], not blank']

-- ---------- やってみよう ----------
-- 1. startP を書きかえて、左から順に掛けるのが一番よい例を作る
-- 2. 2 のお手本で、最適と左から順の差を電卓で確かめる
-- 3. costAt の掛け算を足し算に変えると、表はどう変わる？

-- ---------- 道具：色と、左上が原点の座標で描く関数（p5.js の座標のまま書ける） ----------

cBg, cPanel, cFg, cMuted, cRasp, cCyan, cAmber, cLime, cGrid :: Color
cBg    = rgb255 0x16 0x11 0x1c
cPanel = rgb255 0x23 0x1a 0x2b
cFg    = rgb255 0xf3 0xea 0xf0
cMuted = rgb255 0x9a 0x8a 0xa3
cRasp  = rgb255 0xff 0x6b 0x8b
cCyan  = rgb255 0x5e 0xe6 0xe0
cAmber = rgb255 0xff 0xc8 0x57
cLime  = rgb255 0xa3 0xe6 0x35
cGrid  = rgb255 0x3a 0x2f 0x44

-- | 画面全体を「左上が原点」の座標に移す（中の図形は y を下向きで渡す）
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Pictures

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 四角の枠線（太さ t）
outline :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline c t x y w h = case cornerPts x y w h of
  ps@(p : _) | t <= 1.2  -> Color c (Line (ps ++ [p]))
             | otherwise -> Color c (thickPath t (ps ++ [p]))
  [] -> Blank

-- | 塗りと枠線のある表のマス
cellBox :: Color -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
cellBox f s t x y w h = Pictures [rbox f x y w h, outline s t x y w h]

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 文字：(x, y) を右端の真ん中にして書く
txtR :: Float -> Color -> Float -> Float -> String -> Picture
txtR size c x y s = label (x - fromIntegral (length s) * size * 0.8) (-y - size / 2) size c s

-- | 2 色を混ぜる（p5.js の lerpColor）
mixC :: Color -> Color -> Float -> Color
mixC a b t0 =
  let t = clampF 0 1 t0
      (r1, g1, b1, _) = rgbaOfColor a
      (r2, g2, b2, _) = rgbaOfColor b
  in makeColor (lerpF r1 r2 t) (lerpF g1 g2 t) (lerpF b1 b2 t) 1

-- | リストの i 番目（範囲外なら d）
nth :: a -> [a] -> Int -> a
nth d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
