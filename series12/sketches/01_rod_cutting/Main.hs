-- ============================================
-- 01 ロッドカット：棒をどう切れば一番高く売れる？
-- 長さ i の棒は price[i] 円。長さ n の棒を切り分けて売るときの最大収入 r[n] を求める
-- r[j] = max( price[i] + r[j-i] )　─ 「最初に切る長さ i」を全部ためす。短い棒の答えは表にある
-- スペース = 再生・停止 / → = 1 手すすむ / ↑↓ = 棒の長さ n / 値段の箱をクリック = +1 円（Shift で -1） / P = 値段をランダム / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.IntMap.Strict as IM
import Text.Printf (printf)

startPrice :: [Int]
startPrice = [0, 2, 5, 9, 10, 13, 17, 18, 21, 24, 27]  -- ★ 値段表（price[0] は使わない）。長さ 8 なら 2 + 3 + 3 で 23 円

startN :: Int
startN = 8                  -- ★ 棒の長さ

framesPerMove :: Int
framesPerMove = 14          -- ★ 1 手の間のフレーム数（小さいほど速い）

data World = World
  { wPrice   :: !(IM.IntMap Int)
  , wN       :: !Int
  , wR       :: !(IM.IntMap Int)   -- 最大収入 r[j]（計算が終わった分だけ入る）
  , wS       :: !(IM.IntMap Int)   -- 最初に切る長さ s[j]
  , wJ       :: !Int
  , wI       :: !Int
  , wBest    :: !Int
  , wBestI   :: !Int
  , wPlaying :: !Bool
  , wDone    :: !Bool
  , wTimer   :: !Int
  }

initial :: World
initial = restart (World (IM.fromList (zip [0 ..] startPrice)) startN IM.empty IM.empty 1 1 (-1) 0 True False 0)

main :: IO ()
main = runSketch (sketch "12-01 Rod cutting" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

restart :: World -> World
restart w = w { wR = IM.singleton 0 0, wS = IM.singleton 0 0, wJ = 1, wI = 1, wBest = -1, wBestI = 0, wDone = False }

val :: IM.IntMap Int -> Int -> Int
val m k = IM.findWithDefault 0 k m

-- ★ 1 手：長さ j の棒について「最初に i を切る」を 1 つためす
advance :: World -> World
advance w
  | wDone w = w
  | i1 > j =
      let j1 = j + 1
      in w1 { wR = IM.insert j best1 (wR w), wS = IM.insert j bestI1 (wS w)
            , wJ = j1, wI = 1, wBest = -1, wBestI = 0, wDone = j1 > wN w }
  | otherwise = w1
  where
    j = wJ w
    i = wI w
    cand = val (wPrice w) i + val (wR w) (j - i)
    (best1, bestI1) = if cand > wBest w then (cand, i) else (wBest w, wBestI w)
    i1 = i + 1
    w1 = w { wBest = best1, wBestI = bestI1, wI = i1 }

step :: World -> World
step w
  | wPlaying w && not (wDone w) =
      if wTimer w + 1 >= framesPerMove then advance w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (restart w { wN = min 10 (wN w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (restart w { wN = max 1 (wN w - 1) })
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> do
        price <- foldM (\m k -> do
                           d <- randomRIO (0, 6)
                           pure (IM.insert k (val m (k - 1) + d) m)) (wPrice w) [1 .. 10]
        pure (restart w { wPrice = price })
    | c `elem` "rR" -> pure (restart w) { wPlaying = True }
  EventKey (MouseButton LeftButton) Down m _ -> pure (clickPrice env (shift m == Down) w)
  _ -> pure w

clickPrice :: Env -> Bool -> World -> World
clickPrice env shifted w =
  case [k | k <- [1 .. 10], let x = 140 + i2f (k - 1) * cw, mx > x, mx < x + cw - 6, my > 130, my < 160] of
    (k : _) -> restart w { wPrice = IM.insert k (max 0 (val (wPrice w) k + (if shifted then -1 else 1))) (wPrice w) }
    [] -> w
  where
    (mx, my) = mouseP env
    cw = cellW env

cellW :: Env -> Float
cellW env = min 72 ((envW env - 64) / 11)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  [ txt 26 cFg 32 24 ("Rod cutting: best income for a rod of length " ++ show n)
  , txt 15 cMuted 32 60 "Space: play/pause  Right: 1 step  Up/Down: length  Click price: +1 (Shift -1)  P: random  R: restart"
  , priceTable, revenueTable
  , if wDone w then drawRod env w 300 else candidates
  , drawCallChart env n
  ]
  where
    n = wN w
    cw = cellW env
    price = val (wPrice w)
    r = val (wR w)
    j = wJ w
    i = wI w
    y0 = 100
    priceTable = Pictures
      ( [txtL 14 cMuted 32 (y0 + 14) "length i", txtL 14 cMuted 32 (y0 + 44) "price[i]"]
      ++ concat
           [ [ rbox (if k <= n then cPanel else rgb255 0x1c 0x16 0x22) x (y0 + 30) (cw - 6) 30
             , txtC 14 cMuted (x + cw / 2 - 3) (y0 + 14) (show k)
             , txtC 15 (if k <= n then cAmber else cMuted) (x + cw / 2 - 3) (y0 + 45) (show (price k)) ]
           | k <- [1 .. 10], let x = 140 + i2f (k - 1) * cw ] )
    ty = 200
    revenueTable = Pictures
      ( [txtL 14 cMuted 32 (ty + 22) "best r[j]", txtL 14 cMuted 32 (ty + 56) "cut s[j]"]
      ++ concat
           [ [ cellBox (if known then cPanel else cBg) (if cur then cAmber else if used then cCyan else cGrid)
                       (if cur || used then 3 else 1) x (ty + 6) (cw - 6) 32
             , txtC 14 cMuted (x + cw / 2 - 3) (ty - 5) (show k) ]
             ++ (if known
                   then [ txtC 15 cFg (x + cw / 2 - 3) (ty + 22) (show (r k))
                        , txtC 14 cLime (x + cw / 2 - 3) (ty + 56) (if k > 0 then show (val (wS w) k) else "-") ]
                   else [])
           | k <- [0 .. n]
           , let x = 140 + i2f k * cw
           , let known = IM.member k (wR w)
           , let cur = not (wDone w) && k == j
           , let used = not (wDone w) && k == j - i && i <= j ] )
    cy = 300
    -- いま計算中の候補を棒で見せる
    sc = envW env * 0.38 / i2f (max 1 (maxPossible w))
    candidates = Pictures
      ( txt 18 cFg 32 cy ("r[" ++ show j ++ "]: cut i first -> price[i] + r[" ++ show j ++ " - i]")
      : concat
          [ txtR 14 cMuted 90 (y + 10) ("i = " ++ show k)
            : (if k <= i
                 then [ rbox (if k == i then cAmber else if isBest then cLime else rgb255 0x5a 0x4a 0x66)
                             100 y (i2f (price k) * sc) 20
                      , rbox (if k == i then rgb255 0xd9 0xa6 0x3b else if isBest then rgb255 0x7f 0xb5 0x2a else rgb255 0x3f 0x33 0x49)
                             (100 + i2f (price k) * sc) y (i2f (r (j - k)) * sc) 20
                      , txtL 14 cFg (110 + i2f v * sc) (y + 10)
                          (show (price k) ++ " + " ++ show (r (j - k)) ++ " = " ++ show v ++ (if isBest then "  <- best so far" else "")) ]
                 else [])
          | k <- [1 .. j]
          , let y = cy + 36 + i2f (k - 1) * 26
          , let v = price k + r (j - k)
          , let isBest = k == wBestI w ] )

maxPossible :: World -> Int
maxPossible w =
  maximum (1 : IM.elems (wR w)) + maximum (0 : [val (wPrice w) k | k <- [1 .. wN w]])

-- 切り方の復元：s[n] を切り、残りの s[...] を繰り返す
piecesOf :: World -> [Int]
piecesOf w = go (wN w)
  where go rest | rest <= 0 = []
                | otherwise = let c = max 1 (val (wS w) rest) in c : go (rest - c)

drawRod :: Env -> World -> Float -> Picture
drawRod env w cy = Pictures
  ( txt 20 cFg 32 cy ("Answer: r[" ++ show n ++ "] = " ++ show (val (wR w) n) ++ " yen   cuts = " ++ intercalate " + " (map show pieces))
  : blocks ++ [txt 14 cMuted 40 (y + 64) "s[n] records the first cut, so the table alone gives the whole answer"] )
  where
    n = wN w
    pieces = piecesOf w
    unit = min 70 (envW env * 0.6 / i2f n)
    y = cy + 60
    starts = scanl (+) 40 (map (\p -> i2f p * unit) pieces)
    blocks = concat
      [ [ rbox (if odd k then cCyan else cRasp) x y (pw - 4) 46
        , txtC 14 cBg (x + pw / 2 - 2) (y + 23) (show p ++ " (" ++ show (val (wPrice w) p) ++ ")") ]
      | (k, p, x) <- zip3 [0 :: Int ..] pieces starts, let pw = i2f p * unit ]

-- 素朴な再帰は 2^n 回呼ばれる。DP は j の数 × i の数 ≒ n²/2 回の計算
drawCallChart :: Env -> Int -> Picture
drawCallChart env n = Pictures
  [ rbox cPanel (x0 - 16) (y0 - 44) (cw + 32) (ch + 80)
  , txt 14 cFg x0 (y0 - 32) "Number of steps (log scale)"
  , seg cGrid 1 (x0, y0 + ch) (x0 + cw, y0 + ch)
  , curve cRasp (\k -> 2 ** i2f k)
  , curve cCyan (\k -> i2f (k * (k + 1)) / 2)
  , txt 14 cRasp x0 (y0 + ch + 8) ("naive 2^" ++ show n ++ " = " ++ show ((2 :: Int) ^ n))
  , txt 14 cCyan (x0 + 200) (y0 + ch + 8) ("DP " ++ show (n * (n + 1) `div` 2))
  , seg cAmber 1 (xx, y0) (xx, y0 + ch)
  ]
  where
    cw = 300
    ch = 200
    x0 = envW env - cw - 32
    y0 = envH env - ch - 48
    ymax = log (2 ** 10)
    curve c f = pathL c 2.5 [(x0 + i2f (k - 1) * cw / 9, y0 + ch - log (f k) / ymax * ch) | k <- [1 .. 10]]
    xx = x0 + i2f (n - 1) * cw / 9

-- ---------- やってみよう ----------
-- 1. startPrice を書きかえて、切らないほうが得な値段表を作る
-- 2. framesPerMove を 2 にして、表が埋まる速さを変える
-- 3. 切るたびに手数料がかかるとしたら？ advance の cand から手数料を引いてみる

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

-- | マウスの位置を左上原点の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

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

-- | 線分（太さ t）
seg :: Color -> Float -> Point -> Point -> Picture
seg c t (x1, y1) (x2, y2)
  | t <= 1.2  = Color c (Line [(x1, -y1), (x2, -y2)])
  | otherwise = Color c (thickLine t (x1, -y1) (x2, -y2))

-- | 折れ線（太さ t）
pathL :: Color -> Float -> [Point] -> Picture
pathL c t ps = Color c (thickPath t [(x, -y) | (x, y) <- ps])

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を左端の真ん中にして書く
txtL :: Float -> Color -> Float -> Float -> String -> Picture
txtL size c x y = label x (-y - size / 2) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 文字：(x, y) を右端の真ん中にして書く
txtR :: Float -> Color -> Float -> Float -> String -> Picture
txtR size c x y s = label (x - fromIntegral (length s) * size * 0.8) (-y - size / 2) size c s

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
