-- ============================================
-- 38-02 トロピカル直線
-- a ⊗ x ⊕ b ⊗ y ⊕ c = min(a + x, b + y, c)。「直線」= 最小値が 2 回以上とられる場所
--   頂点 (c − a, c − b) から 3 本の半直線が出る。min 流儀では方向 (1,0), (0,1), (−1,−1)
--   平面は 3 つの領域に分かれ、各領域で「どの項が最小か」が決まっている
-- 操作：頂点をドラッグ / 1 2 3 = 係数 a, b, c を選ぶ / ↑↓ = 選んだ係数を変える / M = max 流儀 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Word (Word8)
import Numeric (showFFloat)

sc, scale4 :: Float
sc = 70                    -- ★ 1 単位 = 何 px
scale4 = 4                 -- 領域の色分けの粗さ

data World = World
  { wA, wB, wC :: !Float   -- ★ 係数（a ⊗ x ⊕ b ⊗ y ⊕ c）
  , wMax :: !Bool          -- ★ True なら max 流儀（半直線の向きが反対になる）
  , wSel :: !Int           -- ↑↓ で変える係数（0 = a, 1 = b, 2 = c）
  , wDrag :: !Bool }

world0 :: World
world0 = World 0 0 0 False 2 False

main :: IO ()
main = runSketch (sketch "38-02 Tropical Line" world0)
  { skBack  = bgCol
  , skDraw  = draw
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

ox, oy :: Env -> Float
ox env = envW env * 0.42
oy env = envH env * 0.55

toX, toY, fromX, fromY :: Env -> Float -> Float
toX env x = ox env + x * sc
toY env y = oy env - y * sc
fromX env s = (s - ox env) / sc
fromY env s = (oy env - s) / sc

-- ★ 3 つの項の値と、どれが最小（max 流儀なら最大）か
bestOf :: World -> Float -> Float -> Int
bestOf w x y =
  let v0 = wA w + x
      v1 = wB w + y
      v2 = wC w
      better p q = if wMax w then p > q else p < q
      b01 = if better v1 v0 then 1 else 0 :: Int
      vb = if b01 == 1 then v1 else v0
  in if better v2 vb then 2 else b01

-- 領域を塗る：粗い画像の各ピクセルで「どの項が最小か」を調べて色を決める
regions :: Env -> World -> Picture
regions env w = pixelImage bw bh scale4 pixel
  where
    bw = ceiling (envW env / scale4)
    bh = ceiling (envH env / scale4)
    pixel px py =
      let x = fromX env (fromIntegral px * scale4)
          y = fromY env (fromIntegral py * scale4)
          v0 = wA w + x
          v1 = wB w + y
          v2 = wC w
          lo = min v0 (min v1 v2)
          hi = max v0 (max v1 v2)
          mid = v0 + v1 + v2 - lo - hi
          gap = if wMax w then hi - mid else mid - lo   -- 1 位と 2 位の差：小さいほど直線に近い
          k = 0.25 + 0.55 * min 1 (gap / 1.5)
          q :: Float -> Word8
          q c = truncate (c * k)
      in case bestOf w x y of
           0 -> (q 255, q 170, q 60)      -- a⊗x = マンゴー
           1 -> (q 255, q 80, q 160)      -- b⊗y = ピンク
           _ -> (q 160, q 230, q 70)      -- c = ライム

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ regions env w
  , seg env 1 axisC (toX env 0, 0) (toX env 0, envH env)
  , seg env 1 axisC (0, toY env 0) (envW env, toY env 0) ]
  ++ concat (zipWith rayPic dirs ties)
  ++ [ dotP env (toX env vx) (toY env vy) 18 (gray 1)
     , txt env (toX env vx + 12) (toY env vy - 12) 14 (gray 1) ("(" ++ nf 1 vx ++ ", " ++ nf 1 vy ++ ")") ]
  ++ zipWith3 regionLabel lpos hues names
  ++ [ txt env 20 28 16 (gray 1) ("F(x, y) = " ++ sym ++ "(a + x, b + y, c)   a = " ++ nf 1 (wA w) ++ mark 0 ++ "   b = " ++ nf 1 (wB w) ++ mark 1 ++ "   c = " ++ nf 1 (wC w) ++ mark 2)
     , txt env 20 50 14 (gray 0.9) ("1 2 3: choose, Up/Down: change   M: " ++ sym ++ " style   R: reset   Drag the vertex")
     , txt env 20 72 14 (gray 0.9) ("Mouse (" ++ nf 2 mxv ++ ", " ++ nf 2 myv ++ "):  a + x = " ++ nf 2 (wA w + mxv) ++ ",  b + y = " ++ nf 2 (wB w + myv) ++ ",  c = " ++ nf 2 (wC w) ++ "  -> " ++ word ++ " is " ++ termName (bestOf w mxv myv))
     , txt env 20 94 14 (gray 0.9) "Vertex = (c - a, c - b). Brightness = gap between 1st and 2nd term (dark at borders)"
     , txt env 20 (envH env - 24) 14 (gray 0.9) "Dragging changes a, b (c fixed). Ray directions sum: (1,0)+(0,1)+(-1,-1) = 0 (balancing, see 12)" ] )
  where
    axisC = hsba 0 0 100 0.3
    vx = wC w - wA w
    vy = wC w - wB w
    sym = if wMax w then "max" else "min"
    word = if wMax w then "largest" else "lowest"
    mark :: Int -> String
    mark k = if wSel w == k then " <" else ""
    dirs = if wMax w then [(-1, 0), (0, -1), (1, 1)] else [(1, 0), (0, 1), (-1, -1)]
    -- 各半直線は「2 つの項が同点」の場所
    ties = ["b (x) y = c", "a (x) x = c", "a (x) x = b (x) y"]
    rayPic (dx, dy) tie =
      [ seg env 5 (gray 1) (toX env vx, toY env vy) (toX env (vx + dx * 100), toY env (vy + dy * 100))
      , txt env (toX env (vx + dx * 1.6) + 6) (toY env (vy + dy * 1.6) - 6) 14 (gray 1) tie ]
    -- 領域のラベル。min：a⊗x は左、b⊗y は下、c は右上。max では反対
    names = map (\s -> s ++ " " ++ word) ["a (x) x", "b (x) y", "c"]
    hues = [35, 330, 90]
    lpos | wMax w = [(vx + 2.5, vy - 1.2), (vx - 1.2, vy + 2.5), (vx - 2, vy - 2)]
         | otherwise = [(vx - 2.5, vy + 1.2), (vx + 1.2, vy - 2.5), (vx + 2, vy + 2)]
    regionLabel (lx, ly) h s = txt env (toX env lx - 60) (toY env ly) 18 (hsb h 30 100) s
    (mx, my) = mouseP env
    mxv = fromX env mx
    myv = fromY env my

termName :: Int -> String
termName 0 = "a (x) x"
termName 1 = "b (x) y"
termName _ = "c"

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wDrag = dist2 (mouseP env) (toX env (wC w - wA w), toY env (wC w - wB w)) < 30 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventMotion _ | wDrag w ->
    let (mx, my) = mouseP env
    in w { wA = clampF (-8) 8 (wC w - fromX env mx), wB = clampF (-8) 8 (wC w - fromY env my) }
  EventKey (Char '1') Down _ _ -> w { wSel = 0 }
  EventKey (Char '2') Down _ _ -> w { wSel = 1 }
  EventKey (Char '3') Down _ _ -> w { wSel = 2 }
  EventKey (SpecialKey KeyUp) Down _ _ -> nudge 0.5
  EventKey (SpecialKey KeyDown) Down _ _ -> nudge (-0.5)
  EventKey (Char ch) Down _ _
    | ch `elem` "mM" -> w { wMax = not (wMax w) }
    | ch `elem` "rR" -> w { wA = 0, wB = 0, wC = 0 }
  _ -> w
  where
    nudge dv = case wSel w of
      0 -> w { wA = clampF (-8) 8 (wA w + dv) }
      1 -> w { wB = clampF (-8) 8 (wB w + dv) }
      _ -> w { wC = clampF (-8) 8 (wC w + dv) }

-- ---------- やってみよう ----------
-- 1. c だけを変えると頂点はどう動く？ a だけなら？ 式 (c − a, c − b) で説明しよう
-- 2. 3 つの係数に同じ数を足しても直線は変わらない（射影的）。確かめよう
-- 3. M で max 流儀にすると半直線の向きが反対になる。領域の形はどう変わる？
-- 4. 項を 1 つ増やして min(a + x, b + y, c, e + x + y) にすると「直線」はどんな形になる？（04 の予告）


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
