-- ============================================
-- 08 臍点（へそ）のカタストロフィー（双曲・楕円・放物）
-- 状態が 2 つ (x, y) ある地形 V(x, y)。左 = 地形の等高線と平衡点（● 谷、▲ 山、✕ 峠）
--   右 = コントロール (u, v) の平面。黄の線（分岐集合）をこえると平衡点が 2 つ生まれる / 消える
--   楕円 = 三角形（デルトイド）、双曲 = 1 本の折り目と 1 つのカスプ、放物 = その中間。光のコースティクス（09）にも同じ形が出る
-- マウス（右の平面）= (u, v) / 1〜3 = 種類 / ↑↓ = w（形の大きさ） / G = 等高線の線
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

tP, rangeR, rangeU :: Float
tP = -0.5                  -- ★ 放物の追加パラメータ
rangeR = 1.6               -- 状態 (x, y) の表示範囲
rangeU = 2.5               -- コントロール (u, v) の表示範囲

mapN :: Int
mapN = 150                 -- 等高線の絵の大きさ

data CpType = Saddle | Min | Max deriving Eq

data World = World
  { wKind  :: !Int         -- 1 楕円, 2 双曲, 3 放物
  , wW     :: !Float       -- ★ 形の大きさ
  , wU     :: !Float       -- コントロール
  , wV     :: !Float
  , wLines :: !Bool
  , wMap   :: !Picture     -- 等高線の絵
  , wSet   :: !Picture     -- 分岐集合の絵
  , wCps   :: ![(Float, Float, CpType)]
  }

world0 :: World
world0 = World 1 1.2 0.3 0.2 True Blank Blank []

main :: IO ()
main = runSketch (sketch "35-08 Umbilics" world0)
  { skBack  = hsb 200 25 10
  , skStart = \env -> pure (refreshSet env (refresh env world0))
  , skDraw  = drawAll
  , skEvent = onEvent
  }

kindName :: Int -> String
kindName 1 = "Elliptic  V = x^3 - 3xy^2 + w(x^2+y^2) - ux - vy"
kindName 2 = "Hyperbolic  V = x^3 + y^3 + wxy - ux - vy"
kindName _ = "Parabolic  V = x^2y + y^4 + wx^2 + ty^2 - ux - vy"

-- ★ 地形（u, v の 1 次の項を除いた部分）と、その勾配・ヘッセ行列（2 階微分）
v0 :: Int -> Float -> Float -> Float -> Float
v0 1 w x y = x * x * x - 3 * x * y * y + w * (x * x + y * y)
v0 2 w x y = x * x * x + y * y * y + w * x * y
v0 _ w x y = x * x * y + y * y * y * y + w * x * x + tP * y * y

grad0 :: Floating a => Int -> a -> a -> a -> (a, a)
grad0 1 w x y = (3 * x * x - 3 * y * y + 2 * w * x, -6 * x * y + 2 * w * y)
grad0 2 w x y = (3 * x * x + w * y, 3 * y * y + w * x)
grad0 _ w x y = (2 * x * y + 2 * w * x, x * x + 4 * y * y * y + 2 * realToFrac tP * y)

-- [xx, xy, yy]
hess0 :: Floating a => Int -> a -> a -> a -> (a, a, a)
hess0 1 w x y = (6 * x + 2 * w, -6 * y, -6 * x + 2 * w)
hess0 2 w x y = (6 * x, w, 6 * y)
hess0 _ w x y = (2 * y + 2 * w, 2 * x, 12 * y * y + 2 * realToFrac tP)

layout :: Env -> (Float, Float, Float, Float)     -- (lx, rx, py, sz)
layout env =
  let py = 110
      sz = min (envH env - py - 50) ((envW env - 90) / 2)
      lx = 30
  in (lx, lx + sz + 30, py, sz)

-- 左の等高線の絵と平衡点を作り直す
refresh :: Env -> World -> World
refresh env w =
  let (_, _, _, sz) = layout env
      kind = wKind w
      ww = wW w
      n = mapN
      vals = U.generate (n * n) $ \k ->
        let (j, i) = k `divMod` n
            x = negate rangeR + 2 * rangeR * (fromIntegral i + 0.5) / fromIntegral n
            y = rangeR - 2 * rangeR * (fromIntegral j + 0.5) / fromIntegral n
        in v0 kind ww x y - wU w * x - wV w * y
      lo = U.minimum vals
      hi = U.maximum vals
      -- 高さを圧縮した「段」の番号。となりの画素と段がちがう所 = 等高線
      lev = U.map (\v -> floor (asinh ((v - (lo + hi) / 2) * 3) * 2.5) :: Int) vals
      px i j =
        let k = j * n + i
            s = (vals U.! k - lo) / (hi - lo + 1e-9)
            edge = wLines w && ((i < n - 1 && lev U.! k /= lev U.! (k + 1))
                                || (j < n - 1 && lev U.! k /= lev U.! (k + n)))
            l = if edge then 70 else 0
            q v = fromIntegral (max 0 (min 255 (round v :: Int)))
        in (q (30 + 200 * s ** 0.6 + l), q (50 + 80 * s + l), q (140 - 100 * s + l))
  in w { wMap = pixelImage n n (sz / fromIntegral n) px
       , wCps = criticalPoints kind ww (wU w) (wV w) }

-- 分岐集合：det H = 0 の曲線を (x, y) → 勾配 = (u, v) へ写す
refreshSet :: Env -> World -> World
refreshSet env w =
  let (_, rx, py, sz) = layout env
      kind = wKind w
      ww = wW w
      n = 260 :: Int
      r1 = rangeR * 1.6
      coord i = negate r1 + 3.2 * rangeR * i / fromIntegral n
      det = U.generate ((n + 1) * (n + 1)) $ \k ->
        let (i, j) = k `divMod` (n + 1)
            (a, b, c) = hess0 kind ww (coord (fromIntegral i)) (coord (fromIntegral j))
        in a * c - b * b :: Float
      at2 i j = det U.! (i * (n + 1) + j)
      dots = [ box (rx + (gx + rangeU) / (2 * rangeU) * sz - 1.25) (py + (rangeU - gy) / (2 * rangeU) * sz - 1.25) 2.5 2.5
             | i <- [0 .. n - 1], j <- [0 .. n - 1]
             , (di, dj) <- [(1, 0), (0, 1)]
             , let d0 = at2 i j
                   d1 = at2 (i + di) (j + dj)
             , d0 * d1 < 0
             , let s = d0 / (d0 - d1)
                   x = coord (fromIntegral i + fromIntegral di * s)
                   y = coord (fromIntegral j + fromIntegral dj * s)
                   (gx, gy) = grad0 kind ww x y
             , abs gx < rangeU, abs gy < rangeU ]
  in length dots `seq` w { wSet = Color (rgb255 255 220 90) (Pictures dots) }

-- 平衡点（勾配 = (u, v)）をニュートン法で探す（誤差 1e-6 を見るので、ここだけ Double で計算する）
criticalPoints :: Int -> Float -> Float -> Float -> [(Float, Float, CpType)]
criticalPoints kind wf uf vf = reverse (foldl add [] starts)
  where
    ww, u, v, r :: Double
    ww = realToFrac wf
    u = realToFrac uf
    v = realToFrac vf
    r = realToFrac rangeR
    starts = [ (-r + 2 * r * fromIntegral i / 6, -r + 2 * r * fromIntegral j / 6) | i <- [0 .. 6 :: Int], j <- [0 .. 6 :: Int] ]
    newton :: Int -> Double -> Double -> (Double, Double)
    newton 0 x y = (x, y)
    newton k x y =
      let (gx, gy) = grad0 kind ww x y
          (a, b, c) = hess0 kind ww x y
          fx = gx - u
          fy = gy - v
          det = a * c - b * b
      in if abs det < 1e-9 then (x, y)
         else newton (k - 1) (x - clampD ((c * fx - b * fy) / det)) (y - clampD ((negate b * fx + a * fy) / det))
    clampD = max (-0.5) . min 0.5
    add out (x0, y0) =
      let (x, y) = newton 30 x0 y0
          (gx, gy) = grad0 kind ww x y
          ok = abs (gx - u) + abs (gy - v) < 1e-6 && abs x < r && abs y < r
          fx = realToFrac x :: Float
          fy = realToFrac y :: Float
          dup = any (\(px, py, _) -> abs (px - fx) + abs (py - fy) < 1e-3) out
      in if ok && not dup then (fx, fy, classify x y) : out else out
    classify x y =
      let (a, b, c) = hess0 kind ww x y
          det = a * c - b * b
      in if det < 0 then Saddle else if a > 0 then Min else Max

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      (lx, rx, py, sz) = layout env
      cps = wCps w
      mins = length [ () | (_, _, Min) <- cps ]
      cpMark (x, y, ty) =
        let sx = lx + (x + rangeR) / (2 * rangeR) * sz
            sy = py + (rangeR - y) / (2 * rangeR) * sz
        in case ty of
             Min -> Color (hsb 160 80 100) (at (sx, sy) (disc 8))
             Max -> Color (hsb 0 80 100) (Polygon [(sx, sy - 10), (sx - 9, sy + 7), (sx + 9, sy + 7)])
             Saddle -> Color white (Pictures [thickLine 3 (sx - 7, sy - 7) (sx + 7, sy + 7), thickLine 3 (sx - 7, sy + 7) (sx + 7, sy - 7)])
  in Pictures
    [ Translate (lx + sz / 2 - wd / 2) (h / 2 - py - sz / 2) (wMap w)
    , onP5 env (Pictures
        [ Pictures (map cpMark cps)
        , Color (rgba255 0 0 0 130) (box lx py sz 52)
        , Color (hsb 200 25 16) (box rx py sz sz)
        , Color (hsbA 0 0 100 20) (Pictures [Line [(rx + sz / 2, py), (rx + sz / 2, py + sz)], Line [(rx, py + sz / 2), (rx + sz, py + sz / 2)]])
        , wSet w
        , Color white (at (rx + (wU w + rangeU) / (2 * rangeU) * sz, py + (rangeU - wV w) / (2 * rangeU) * sz) (disc 7))
        ])
    , txt env (lx + 10) (py + 20) 14 white ("V(x, y): dark low, bright high. Valleys " ++ show mins ++ ", total " ++ show (length cps))
    , txt env (lx + 10) (py + 42) 14 white "Dot valley, triangle hill, X saddle"
    , txt env (rx + 10) (py + 20) 14 (gray 0.9) "Control (u, v): mouse. Yellow = bifurcation set"
    , txt env 20 36 14 white (kindName (wKind w) ++ "   w = " ++ nf 2 (wW w) ++ " (Up/Dn)   1-3: kind   G: contours")
    , txt env 20 62 14 (gray 0.6) "Move the point across the yellow line and a valley (or hill) and a saddle are born as a pair."
    , txt env 20 84 14 (gray 0.6) "At a pointed tip (cusp), three of them merge at once."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _
    | mx > rx && mx < rx + sz && my > py && my < py + sz ->
        refresh env w { wU = (mx - rx) / sz * 2 * rangeU - rangeU, wV = rangeU - (my - py) / sz * 2 * rangeU }
  EventKey (SpecialKey KeyUp) Down _ _   -> both w { wW = min 2.5 (wW w + 0.2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> both w { wW = max (-2.5) (wW w - 0.2) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> both w { wKind = fromEnum c - 48 }
    | c `elem` "gG" -> refresh env w { wLines = not (wLines w) }
  _ -> w
  where
    (mx, my) = mouseP5 env
    (_, rx, py, sz) = layout env
    both = refreshSet env . refresh env

-- ---------- やってみよう ----------
-- 1. 楕円（1）で三角形の外・中に点を置いて、平衡点の数を数えよう（外 = 峠 2 つ、中 = 谷 1 つ + 峠 3 つ）
-- 2. w = 0 にすると三角形は 1 点につぶれる。そこが「臍点（へそ）」
-- 3. 双曲（2）では、カスプの近くで ● と ✕ と ✕ がどう合体するか、ゆっくり見よう
-- 4. 放物（3）で tP を −1 や +0.5 に変えて、楕円と双曲の間の形を探そう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
