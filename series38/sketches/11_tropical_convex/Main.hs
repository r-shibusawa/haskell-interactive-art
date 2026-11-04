-- ============================================
-- 38-11 トロピカル凸包
-- トロピカルな線分 [p, q] = { (λ ⊗ p) ⊕ (μ ⊗ q) } = { min(λ + p, μ + q) }：折れ線（最大 2 回曲がる）
--   方向は (1,0), (0,1), (1,1) のどれか。トロピカル凸包 = トロピカルな線分で閉じた集合 = 点の「トロピカル結合」全部
--   射影の公式：x が凸包にある ⇔ x = min_i (λ_i + p_i)，λ_i = max_j (x_j − p_ij)
-- 操作：クリック = 点を置く / ドラッグ = 点を動かす / H = ふつうの凸包を重ねる / C = 消す / R = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', sort)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

sc, scale4 :: Float
sc = 60
scale4 = 4

type P = (Double, Double)

data World = World
  { wPts :: [P]            -- 凸包の判定は 1e-6 の精度なので Double で持つ
  , wHull :: !Bool
  , wDrag :: !Int
  , wFill :: Picture }     -- 凸包を塗った画像（点が動いたときだけ作り直す）

randomPts :: IO [P]
randomPts = replicateM 5 $ do
  x <- randomRIO (-4, 4)
  y <- randomRIO (-3, 3)
  pure (x, y)

-- 点を置きかえて画像も作り直す
setPts :: Env -> [P] -> World -> World
setPts env ps w = w { wPts = ps, wFill = fillPic env ps }

main :: IO ()
main = runSketch (sketch "38-11 Tropical Convex Hull" (World [] True (-1) Blank))
  { skBack  = bgCol
  , skStart = \env -> do
      ps <- randomPts
      pure (setPts env ps (World [] True (-1) Blank))
  , skDraw  = draw
  , skEvent = onEvent
  }

ox, oy :: Env -> Float
ox env = envW env * 0.45
oy env = envH env * 0.55

toX, toY, fromX, fromY :: Env -> Float -> Float
toX env x = ox env + x * sc
toY env y = oy env - y * sc
fromX env s = (s - ox env) / sc
fromY env s = (oy env - s) / sc

sp :: Env -> P -> Point
sp env (x, y) = (toX env (realToFrac x), toY env (realToFrac y))

-- ★ トロピカル線分 p–q の折れ点：同次座標 (0, px, py) と (s, s + qx, s + qy) の min を s で動かす
tropSegment :: P -> P -> [P]
tropSegment p@(px, py) q@(qx, qy) =
  [q] ++ map hom (sort [0, px - qx, py - qy]) ++ [p]    -- s がこの値を通るたびに「どちらが min か」が変わる
  where
    hom s = let h0 = min 0 s
                h1 = min px (s + qx)
                h2 = min py (s + qy)
            in (h1 - h0, h2 - h0)                       -- 同次座標 → 最初の座標を引いて戻す

-- ★ 点 (X, Y) がトロピカル凸包に入っているか（射影して動かなければ入っている）
inHull :: U.Vector P -> Double -> Double -> Bool
inHull ps x y =
  let step (!h0, !h1, !h2) (px, py) =
        let lam = max 0 (max (x - px) (y - py))         -- λ_i = max_j (x_j − p_ij)
        in (min h0 lam, min h1 (lam + px), min h2 (lam + py))
      big = 1 / 0
      (a, b, c) = U.foldl' step (big, big, big) ps
  in abs (b - a - x) < 1e-6 && abs (c - a - y) < 1e-6

-- 凸包の中を塗る（粗い画像）。マンゴーを透明度 110/255 で背景に重ねた色
fillPic :: Env -> [P] -> Picture
fillPic _ [] = Blank
fillPic env ps = pixelImage bw bh scale4 pixel
  where
    pv = U.fromList ps
    bw = ceiling (envW env / scale4)
    bh = ceiling (envH env / scale4)
    pixel qx qy =
      let x = realToFrac (fromX env (fromIntegral qx * scale4))
          y = realToFrac (fromY env (fromIntegral qy * scale4))
      in if inHull pv x y then (116, 86, 41) else (11, 23, 26)

-- ふつうの凸包
ordinaryHull :: [P] -> [P]
ordinaryHull ps
  | length ps < 3 = ps
  | otherwise = reverse (drop 1 (chain sorted)) ++ reverse (drop 1 (chain (reverse sorted)))
  where
    sorted = sort ps
    turn (ox', oy') (ax, ay) (bx, by) = (ax - ox') * (by - oy') - (ay - oy') * (bx - ox')
    chain = foldl' push []
    push (b : a : rest) p | turn a b p <= 0 = push (a : rest) p
    push st p = p : st

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ wFill w ]
  ++ [ seg env 1 (hsba 0 0 100 0.2) (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 (hsba 0 0 100 0.2) (0, toY env g) (envW env, toY env g) | g <- grid ]
  -- すべての組のトロピカル線分
  ++ [ polyline 2.5 (hsba 35 80 100 0.8) (tropSegment p q) | (i, p) <- ips, (j, q) <- ips, i < j ]
  -- ふつうの凸包
  ++ (if wHull w && length ps >= 3
        then [ polyline 2 (hsba 90 80 100 0.8) (h ++ take 1 h) ]
        else [])
  -- マウス位置の線分（最後の点との）
  ++ (case reverse ps of
        (l : _) -> [ polyline 1.5 (hsba 330 70 100 0.6) (tropSegment l mp) ]
        [] -> [])
  ++ [ let (x, y) = sp env p in dotO env x y 16 (hsb (if k == wDrag w then 55 else 330) 80 100) | (k, p) <- ips ]
  ++ [ txt env 20 26 16 (gray 1) ("Click: add point   Drag: move   H: ordinary hull (green) " ++ (if wHull w then "ON" else "off") ++ "   C: clear   R: random   Points " ++ show (length ps))
     , txt env 20 48 14 (gray 0.65) "Mango polylines = tropical segments of each pair (directions (1,0), (0,1), (1,1) only)."
     , txt env 20 68 14 (gray 0.65) "Filled = tropical hull (it can have 2D parts that segments alone do not fill)."
     , txt env 20 88 14 (gray 0.65) ("Pink = tropical segment, last point to mouse. Mouse (" ++ nfd 2 (fst mp) ++ ", " ++ nfd 2 (snd mp) ++ ") is " ++ (if not (null ps) && inHull (U.fromList ps) (fst mp) (snd mp) then "inside" else "outside"))
     , txt env 20 (envH env - 44) 14 (gray 0.65) "min(lambda + p, mu + q) = the min of p raised by lambda and q raised by mu."
     , txt env 20 (envH env - 24) 14 (gray 0.65) "It bends twice, where the winner of the min changes in a coordinate." ] )
  where
    grid = [fromIntegral g | g <- [-12 .. 12 :: Int]] :: [Float]
    ps = wPts w
    ips = zip [0 :: Int ..] ps
    h = ordinaryHull ps
    (mx, my) = mouseP env
    mp = (realToFrac (fromX env mx), realToFrac (fromY env my)) :: P
    polyline wd c qs = Color c (thickPath wd [ let (x, y) = sp env q in pt env x y | q <- qs ])

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my < 100 -> pure w
    | otherwise ->
        let ps = wPts w
            hit = [k | (k, p) <- zip [0 ..] ps, dist2 (mx, my) (sp env p) < 14]
        in pure (case reverse hit of
             (k : _) -> w { wDrag = k }
             [] | length ps < 24 ->
                    setPts env (ps ++ [(realToFrac (fromX env mx), realToFrac (fromY env my))]) w { wDrag = length ps }
                | otherwise -> w { wDrag = -1 })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let p = ( realToFrac (clampF (-10) 10 (fromX env mx)), realToFrac (clampF (-8) 8 (fromY env my)) )
        ps = [ if k == wDrag w then p else q | (k, q) <- zip [0 ..] (wPts w) ]
    in pure (setPts env ps w)
  EventResize _ -> pure (setPts env (wPts w) w)
  EventKey (Char ch) Down _ _
    | ch `elem` "hH" -> pure w { wHull = not (wHull w) }
    | ch `elem` "cC" -> pure (setPts env [] w { wDrag = -1 })
    | ch `elem` "rR" -> do
        ps <- randomPts
        pure (setPts env ps w { wDrag = -1 })
  _ -> pure w
  where (mx, my) = mouseP env

-- ---------- やってみよう ----------
-- 1. 2 点だけ置く。トロピカル線分はいつ「まっすぐ」になる？（同じ x，同じ y，または対角線上）
-- 2. 3 点でトロピカル凸包が 2 次元（面）になる配置と、折れ線だけ（1 次元）になる配置を探そう
-- 3. ふつうの凸包とトロピカル凸包、どちらが広い？ 点を増やして比べよう
-- 4. max 流儀（min → max, λ の max → min）に書き換えると線分の曲がり方が変わる。方向はどうなる？


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str
