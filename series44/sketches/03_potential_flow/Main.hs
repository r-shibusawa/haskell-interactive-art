-- ============================================
-- 44-03 ポテンシャル流の重ね合わせ
-- 粘性がなく渦なしの流れは、速度ポテンシャル φ（速度 = ∇φ）で書ける。式は ∇²φ = 0（ラプラス方程式）
-- 線形なので「足し算」ができる：一様流・湧き出し・吸い込み・渦・二重極を足すだけで、いろいろな流れになる
--   水色 = 流線（流れ関数 ψ の等高線）　　砂色 = 等ポテンシャル線（φ の等高線）。2 つはいつも直交する
-- 操作：1〜5 = 置く部品 / クリック = 置く・ドラッグ = 動かす / 左右キー = 一様流 U / E = 等ポテンシャル線
--       R = お手本を順に / Z = 1 つ戻す / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (findIndex, foldl')
import qualified Data.IntSet as IS
import Data.Array (listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

mStr, kappa :: Float
mStr = 40000        -- ★ 湧き出し・吸い込み・渦の強さ
kappa = 500000      -- ★ 二重極の強さ（一様流の中では半径 √(κ/U) の円柱になる）

levels :: Float
levels = 16         -- ★ 部品 1 個から出る流線の本数

gx :: Int
gx = 129            -- ★ 等高線を計算する格子の横の点の数

-- 部品：場所と種類（1 湧き出し, 2 吸い込み, 3 渦（時計回り）, 4 渦（反時計回り）, 5 二重極）
data Elem = Elem { eX :: !Float, eY :: !Float, eK :: !Int }

data World = World
  { wU       :: !Float
  , wKind    :: !Int
  , wShowPhi :: !Bool
  , wElems   :: ![Elem]
  , wPreset  :: !Int
  , wGrab    :: !Int
  , wPsi     :: ![[Point]]    -- 流線（描き直したときだけ計算しておく）
  , wPhi     :: ![[Point]]    -- 等ポテンシャル線
  , wDots    :: ![Point]
  }

kindNames :: [String]
kindNames = ["", "source", "sink", "vortex cw", "vortex ccw", "doublet"]

main :: IO ()
main = runSketch (sketch "44-03 Superposition of potential flows" (World 100 1 True [] 0 (-1) [] [] []))
  { skBack  = hsb 215 60 10
  , skStart = \env -> do
      ds <- replicateM 300 ((,) <$> randomRIO (0, envW env) <*> randomRIO (0, envH env))
      pure (preset env (World 100 1 True [] 0 (-1) [] [] ds))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- お手本：R を押すたびに次へ
preset :: Env -> World -> World
preset env w =
  let cx = envW env / 2
      c2 = envH env / 2
      list = [ [Elem (cx - 120) c2 1, Elem (cx + 120) c2 2]   -- ランキンの卵形（湧き出し + 吸い込み + 一様流）
             , [Elem cx c2 1]                                 -- 半無限物体
             , [Elem cx c2 5]                                 -- 円柱（二重極 + 一様流）
             , [Elem cx c2 5, Elem cx c2 3] ]                 -- 回る円柱（+ 渦）→ 04 へ
  in rebuild env w { wElems = list !! (wPreset w `mod` 4), wPreset = wPreset w + 1 }

-- ★ 重ね合わせ：全部品の φ・ψ・速度を足すだけ
fieldAt :: World -> Float -> Float -> (Float, Float, Float, Float)
fieldAt w x y = foldl' add (u * x, u * y, u, 0) (wElems w)
  where
    u = wU w
    s = mStr / (2 * pi)
    add (phi, psi, vx, vy) (Elem ex ey k) =
      let dx = x - ex
          dy = y - ey
          r2 = dx * dx + dy * dy + 1
          th = atan2 dy dx
          lr = 0.5 * log r2
      in case k of
           _ | k == 1 || k == 2 ->   -- 湧き出し（吸い込みは符号が逆）：速さ m/(2πr) で放射状
               let q = if k == 1 then s else -s
               in (phi + q * lr, psi + q * th, vx + q * dx / r2, vy + q * dy / r2)
             | k == 3 || k == 4 ->   -- 渦：速さ Γ/(2πr) で回る
               let q = if k == 3 then s else -s
               in (phi + q * th, psi - q * lr, vx - q * dy / r2, vy + q * dx / r2)
             | otherwise ->          -- 二重極：湧き出しと吸い込みをくっつけた極限
               ( phi + kappa * dx / r2, psi - kappa * dy / r2
               , vx + kappa * (dy * dy - dx * dx) / (r2 * r2), vy - kappa * 2 * dx * dy / (r2 * r2) )

-- 流線と等ポテンシャル線を計算し直す（部品が変わったときだけ）
rebuild :: Env -> World -> World
rebuild env w =
  let cell = envW env / fromIntegral (gx - 1)
      gy = ceiling (envH env / cell) + 1
      d = mStr / levels   -- 等高線の間かく。部品の強さ m がちょうど levels 本ぶんになる
      vals = U.generate (gx * gy) (\n -> let (j, i) = n `divMod` gx
                                             (phi, psi, _, _) = fieldAt w (fromIntegral i * cell) (fromIntegral j * cell)
                                         in (phi / d, psi / d))
      pos i j = (fromIntegral i * cell, fromIntegral j * cell)
  in w { wPhi = if wShowPhi w then contourChains gx gy pos (U.map fst vals) levels else []
       , wPsi = contourChains gx gy pos (U.map snd vals) levels }

step :: Env -> Float -> World -> IO World
step env dt0 w = do
  let dt = min dt0 0.033
  ds <- mapM (moveDot env w dt) (wDots w)
  pure w { wDots = ds }

-- 流れに乗る粒
moveDot :: Env -> World -> Float -> Point -> IO Point
moveDot env w dt (x0, y0) = do
  let (_, _, vx, vy) = fieldAt w x0 y0
      sp = sqrt (vx * vx + vy * vy)
      lim = min 1 (900 / (sp + 1))
      x = x0 + vx * lim * dt
      y = y0 + vy * lim * dt
      eaten = any (\e -> (eK e == 2 || eK e == 5) && dist2 (x, y) (eX e, eY e) < 12) (wElems w)
  r <- randomRIO (0, 1 :: Float)
  if eaten || x < 0 || x > envW env || y < 0 || y > envH env || r < 0.002 || isNaN x || isNaN y
    then respawn
    else pure (x, y)
  where
    respawn = do
      let born = filter ((== 1) . eK) (wElems w)
      r1 <- randomRIO (0, 1 :: Float)
      r2 <- randomRIO (0, 1 :: Float)
      if not (null born) && r1 < 0.3
        then do
          n <- randomRIO (0, length born - 1)
          a <- randomRIO (0, 6.283)
          let e = born !! n
          pure (eX e + 14 * cos a, eY e + 14 * sin a)
        else if wU w > 0 && r2 < 0.6
          then (,) 1 <$> randomRIO (0, envH env)
          else (,) <$> randomRIO (0, envW env) <*> randomRIO (0, envH env)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (hsbA 42 50 90 30) (Pictures (map Line (wPhi w)))
  , Color (hsbA 185 70 100 80) (Pictures (map Line (wPsi w))) ]
  ++ [ dot (hsbA 0 0 100 70) x y 3.5 | (x, y) <- wDots w ]
  -- 部品の印
  ++ concat [ [ dot (hsb ([0, 12, 200, 290, 130, 42] !! k) 70 95) x y 26
              , Color (hsb 0 0 100) (Translate x y (ring 13 2))
              , txtC x (y + 7) 15 (hsb 0 0 10) (["", "+", "-", "R", "L", "D"] !! k) ]
            | Elem x y k <- wElems w ]
  -- マウスの場所の速さと圧力（ベルヌーイ：Cp = 1 − (v/U)²）
  ++ [ rectTL (hsbA 215 60 10 85) 0 0 (envW env) 82
     , txt 20 40 18 (hsb 0 0 100)
         ("Place: " ++ show (wKind w) ++ " " ++ (kindNames !! wKind w) ++ " (1-5, click)   uniform U = " ++ f0 u
          ++ " (Left/Right)   E equipotentials   R examples   Z undo   C clear")
     , txt 20 68 15 (hsb 0 0 65)
         ("Just the sum of phi and psi of " ++ show (length (wElems w)) ++ " elements.  At the mouse: speed " ++ f0 sp
          ++ (if u > 0 then "   pressure coefficient Cp = " ++ showFFloat (Just 2) (max (1 - sp * sp / (u * u)) (-99)) "" else ""))
     ]
  where
    u = wU w
    (mx, my) = mouseP env
    (_, _, vx, vy) = fieldAt w mx my
    sp = sqrt (vx * vx + vy * vy)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        g = maybe (-1) id (findIndex (\e -> dist2 (mx, my) (eX e, eY e) < 18) (wElems w))
    in if g < 0 && my > 80 && length (wElems w) < 14
         then rebuild env w { wGrab = -1, wElems = wElems w ++ [Elem mx my (wKind w)] }
         else w { wGrab = g }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = -1 }
  -- つかんだ部品をドラッグで動かす
  EventMotion _ | envDown env && wGrab w >= 0 ->
    let (mx, my) = mouseP env
    in rebuild env w { wElems = [ if n == wGrab w then e { eX = mx, eY = my } else e | (n, e) <- zip [0 ..] (wElems w) ] }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wU = min 300 (wU w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wU = max 0 (wU w - 20) }
  EventKey (Char k) Down _ _
    | k >= '1' && k <= '5' -> w { wKind = fromEnum k - fromEnum '0' }
    | k `elem` "eE" -> rebuild env w { wShowPhi = not (wShowPhi w) }
    | k `elem` "rR" -> preset env w { wGrab = -1 }
    | k `elem` "zZ" -> rebuild env w { wGrab = -1, wElems = take (length (wElems w) - 1) (wElems w) }
    | k `elem` "cC" -> rebuild env w { wGrab = -1, wElems = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. R でお手本を順に見る。湧き出し + 吸い込み + 一様流で「卵形の物体」のまわりの流れになる
-- 2. 二重極だけを一様流に置く → 円柱。半径 √(κ/U) を U（左右キー）で変えて確かめる
-- 3. 円柱の上に渦（3）を重ねる。流線が上下で非対称になる（→ 04 の揚力）
-- 4. C で消して U = 0 にし、湧き出しと吸い込みを並べる。磁石や電荷の絵と同じ形になるのはなぜ？
-- 5. 流線（水色）と等ポテンシャル線（砂色）がどこでも直角に交わっていることを確かめる

-- ---------- 道具（等高線を描く） ----------
-- 等高線（マーチングスクエア法）：nx × ny の格子の値 f が「整数」になる所を線で結ぶ。
-- pos i j = 格子点の画面座標。wrap > 0 なら、値が wrap だけ飛ぶ「切れ目」をまたいでつなぐ。
-- 小さな線分をたくさん描くと遅いので、端がつながる線分を 1 本の折れ線にまとめて返す
contourChains :: Int -> Int -> (Int -> Int -> Point) -> U.Vector Float -> Float -> [[Point]]
contourChains nx ny pos f wrap = chainSegments segs
  where
    segs = concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ]
    -- 格子の辺の番号（横の辺は偶数、縦の辺は奇数）
    hEdge i j = 2 * (i + j * nx)
    vEdge i j = 2 * (i + j * nx) + 1
    cellSegs i j =
      let raw = [f U.! (i + j * nx), f U.! (i + 1 + j * nx), f U.! (i + 1 + (j + 1) * nx), f U.! (i + (j + 1) * nx)]
          c0 = head raw
          c = c0 : [ if wrap > 0 then v - wrap * fromIntegral (round ((v - c0) / wrap) :: Int) else v | v <- drop 1 raw ]
          lo = minimum c
          hi = maximum c
          ps = [pos i j, pos (i + 1) j, pos (i + 1) (j + 1), pos i (j + 1)]
          es = [hEdge i j, vEdge (i + 1) j, hEdge i (j + 1), vEdge i j]
          corner = zip3 c ps es
          sides = zip corner (drop 1 corner ++ take 1 corner)
          crossings lv = [ ((e, lv), (ax + s * (bx - ax), ay + s * (by - ay)))
                         | ((a, (ax, ay), e), (b, (bx, by), _)) <- sides
                         , (a < fromIntegral lv) /= (b < fromIntegral lv)
                         , let s = (fromIntegral lv - a) / (b - a) ]
          pairUp (p : q : rest) = (p, q) : pairUp rest
          pairUp _ = []
      in if not (hi - lo < 5) then []   -- 特異点のすぐそば（値が急変）は描かない
         else concat [ pairUp (crossings lv) | lv <- [ceiling lo .. floor hi :: Int] ]

-- 端点（辺の番号と高さの組）を共有する線分をつないで折れ線にする
chainSegments :: [(((Int, Int), Point), ((Int, Int), Point))] -> [[Point]]
chainSegments segList = go 0 IS.empty
  where
    n = length segList
    segV = listArray (0, max 0 (n - 1)) segList
    key (e, lv) = e * 65536 + (lv `mod` 65536)   -- 辺と高さの組を 1 つの整数に
    adj = IM.fromListWith (++) (concat [ [(key ka, [s]), (key kb, [s])] | (s, ((ka, _), (kb, _))) <- zip [0 ..] segList ])
    seg s = segV ! s
    go i used
      | i >= n = []
      | IS.member i used = go (i + 1) used
      | otherwise =
          let ((ka, pa), (kb, pb)) = seg i
              (fwd, u1) = extend kb (IS.insert i used)
              (back, u2) = extend ka u1
          in (reverse back ++ [pa, pb] ++ fwd) : go (i + 1) u2
    -- 端点 k から、まだ使っていない線分をたどれるだけたどる
    extend k used = case [ t | t <- IM.findWithDefault [] (key k) adj, not (IS.member t used) ] of
      (t : _) ->
        let ((ka, pa), (kb, pb)) = seg t
            (nk, np) = if ka == k then (kb, pb) else (ka, pa)
            (rest, u') = extend nk (IS.insert t used)
        in (np : rest, u')
      [] -> ([], used)

-- ---------- 道具 ----------
f0 :: Float -> String
f0 v = show (round v :: Int)

rectTL :: Color -> Float -> Float -> Float -> Float -> Picture
rectTL c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
