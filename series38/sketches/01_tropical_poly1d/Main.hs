-- ============================================
-- 38-01 一変数トロピカル多項式
-- p(x) = ⊕ (a_i ⊗ x^{⊗i}) = min (a_i + i·x)：各項は直線 a_i + i x、p はその「下包絡線」（折れ線）
--   折れ点 = トロピカルな「根」。根 r の重複度 = そこで変わる傾きの差。根は全部で d 個（基本定理）
-- 操作：係数（●）を上下にドラッグ / ↑↓ = 次数 d / R = ランダム / 0 = 全部 0
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

xMin, xMax, yMin, yMax :: Float
xMin = -4                  -- 表示する範囲
xMax = 4
yMin = -8
yMax = 8

coef0 :: U.Vector Float
coef0 = U.fromList [0, -1, -1, 2, 4, 7, 11]   -- ★ 係数 a_0 … a_6（高さ）

data World = World
  { wD :: !Int                 -- ★ 次数（1 〜 6）
  , wCoef :: !(U.Vector Float)
  , wDrag :: !Int }

world0 :: World
world0 = World 3 coef0 (-1)

main :: IO ()
main = runSketch (sketch "38-01 Tropical Polynomial in One Variable" world0)
  { skBack  = bgCol
  , skDraw  = draw
  , skEvent = onEvent
  }

-- グラフの領域
px0, py0 :: Float
px0 = 60
py0 = 100

pw, ph :: Env -> Float
pw env = envW env * 0.62
ph env = envH env - 160

sx, sy, mxOf, myOf :: Env -> Float -> Float
sx env x = px0 + (x - xMin) / (xMax - xMin) * pw env
sy env y = py0 + ph env - (y - yMin) / (yMax - yMin) * ph env
mxOf env s = xMin + (s - px0) / pw env * (xMax - xMin)
myOf env s = yMin + (py0 + ph env - s) / ph env * (yMax - yMin)

hueOf :: Int -> Float
hueOf i = fromIntegral ((i * 55 + 20) `mod` 360)

-- 係数 i のつまみを置く x（−2.5 〜 2.5 に並べる）
handleX :: Int -> Int -> Float
handleX d i = -2.5 + (fromIntegral i + 0.5) * 5 / fromIntegral (d + 1)

-- ★ 多項式の値（min 流儀）
polyAt :: World -> Float -> Float
polyAt w x = minimum [wCoef w U.! i + fromIntegral i * x | i <- [0 .. wD w]]

data Root = Root { rR :: !Float, rM :: !Int, rI0, rI1 :: !Int }

-- ★ 根と重複度：点 (i, a_i) の下側凸包の辺から求める（辺の傾き s → 根 −s、横の長さ → 重複度）
roots :: World -> ([Int], [Root])
roots w = (hull, zipWith mk hull (drop 1 hull))
  where
    d = wD w
    c = wCoef w
    hull = reverse (foldl push [0] [1 .. d])
    push :: [Int] -> Int -> [Int]
    push st@(q : o : rest) i
      | (c U.! q - c U.! o) * fromIntegral (i - o) >= (c U.! i - c U.! o) * fromIntegral (q - o) = push (o : rest) i
      | otherwise = i : st
    push st i = i : st
    mk i0 i1 = Root (negate (c U.! i1 - c U.! i0) / fromIntegral (i1 - i0)) (i1 - i0) i0 i1

-- つまみの画面位置
handlePos :: Env -> World -> Int -> Point
handlePos env w i =
  let hx = handleX (wD w) i
  in (sx env hx, clampF (py0 + 8) (py0 + ph env - 8) (sy env (wCoef w U.! i + fromIntegral i * hx)))

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ box env px0 py0 (pw env) (ph env) (hsba 0 0 100 0.06)
  , seg env 1 axisC (sx env 0, py0) (sx env 0, py0 + ph env)
  , seg env 1 axisC (px0, sy env 0) (px0 + pw env, sy env 0) ]
  ++ [ txt env (sx env (fromIntegral x) - 5) (py0 + ph env + 20) 14 (gray 0.6) (show x) | x <- [-4 .. 4 :: Int] ]
  ++ [ txt env (px0 - 32) (sy env (fromIntegral y) + 6) 14 (gray 0.6) (show y) | y <- [-8, -6 .. 8 :: Int] ]
  ++ concatMap termLine idx
  ++ [ Color (gray 1) (thickPath 4 envelope) ]
  ++ concatMap rootPic rs
  ++ concatMap handlePic idx
  ++ [ txt env 20 40 16 (gray 1) ("p(x) = min(" ++ intercalate ", " [ nf 1 (c U.! i) ++ (if i > 0 then " + " ++ show i ++ "x" else "") | i <- idx ] ++ ")")
     , txt env 20 66 14 (gray 0.65) ("Drag dots up/down   Up/Down: degree d = " ++ show d ++ "   R: random   0: all zero   White = p(x), red = roots")
     , txt env rx ry 15 (gray 1) ("Roots, multiplicity (total " ++ show d ++ ")") ]
  ++ zipWith rootRow [0 ..] rs
  ++ [ txt env rx fy 14 (gray 1) "Factorization (fundamental thm):" ]
  ++ zipWith (\k str -> txt env rx (fy + 22 + k * 20) 14 (hsb 90 60 95) str) [0 ..] factorLines
  ++ [ txt env rx (fy2 + 2) 14 (gray 0.6) ("= " ++ nf 1 (c U.! d) ++ " + Sum m_k min(x, r_k)")
     , txt env rx (fy2 + 22) 14 (gray 0.6) ("Mult total = " ++ show (sum (map rM rs)) ++ ". All roots real.") ]
  ++ newton )
  where
    d = wD w
    c = wCoef w
    idx = [0 .. d]
    (hull, rs) = roots w
    axisC = hsba 0 0 100 0.25
    -- 各項の直線 a_i + i x（グラフの枠の中だけ描く）
    termLine i =
      let ci = c U.! i
          fi = fromIntegral i :: Float
          xa = if i > 0 then max xMin ((yMin - ci) / fi) else xMin
          xb = if i > 0 then min xMax ((yMax - ci) / fi) else xMax
          visible = (xa < xb && ci >= yMin && ci <= yMax) || (i > 0 && xa < xb)
      in [ seg env 1.5 (hsba (hueOf i) 60 90 0.45) (sx env xa, sy env (ci + fi * xa)) (sx env xb, sy env (ci + fi * xb)) | visible ]
    -- 下包絡線 p(x)（太く）
    envelope = [ pt env s (clampF (py0 - 2) (py0 + ph env + 2) (sy env (polyAt w (mxOf env s))))
               | k <- [0 .. floor (pw env / 3) :: Int], let s = px0 + fromIntegral k * 3 ]
    -- 根（折れ点）
    rootPic q =
      let x = sx env (rR q)
          y = sy env (polyAt w (rR q))
      in [ dotP env x y (10 + fromIntegral (rM q) * 6) (hsb 0 85 100)
         , txt env (x + 12) (y - 10) 14 (gray 1) ("r = " ++ nf 2 (rR q) ++ (if rM q > 1 then "  x" ++ show (rM q) else "")) ]
    -- 係数のつまみ
    handlePic i =
      let (x, y) = handlePos env w i
      in [ dotO env x y (if wDrag w == i then 22 else 16) (hsb (hueOf i) 70 100)
         , txt env (x + 12) (y + 5) 14 (hsb (hueOf i) 50 100) ("a" ++ show i ++ " = " ++ nf 1 (c U.! i)) ]
    -- 右：式と「ニュートン多角形」（1 次元版）
    rx = px0 + pw env + 40
    ry = 110
    rootRow :: Float -> Root -> Picture
    rootRow k q = txt env rx (ry + 26 + k * 22) 14 (hsb 0 80 100)
      ("r" ++ show (round k + 1 :: Int) ++ " = " ++ nf 2 (rR q) ++ "  mult " ++ show (rM q) ++ "  (" ++ show (rI0 q) ++ " <-> " ++ show (rI1 q) ++ ")")
    fy = ry + 26 + fromIntegral (length rs) * 22 + 16
    factors = [ "(x (+) " ++ nf 2 (rR q) ++ ")" ++ (if rM q > 1 then "^" ++ show (rM q) else "") | q <- rs ]
    factorLines = ("p(x) = " ++ nf 1 (c U.! d)) : map (intercalate " (x) ") (chunks2 factors)
    fy2 = fy + 22 + fromIntegral (length factorLines) * 20
    -- 1 次元ニュートン多角形：点 (i, a_i) と下側凸包
    nx = rx
    ny = fy2 + 82
    nw = envW env - rx - 40
    nh = max 60 (envH env - ny - 40)
    vals = map (c U.!) idx
    lo = minimum vals - 1          -- idx は空にならない（d >= 1）
    hi = maximum vals + 1
    gx :: Int -> Float
    gx i = nx + 30 + fromIntegral i * (nw - 60) / fromIntegral (max d 1)
    gy v = ny + nh - 20 - (v - lo) / (hi - lo) * (nh - 40)
    newton =
      [ box env nx ny nw nh (hsba 0 0 100 0.06)
      , txt env nx (ny - 28) 14 (gray 1) "Dual: lower hull of points (i, a_i)"
      , txt env nx (ny - 8) 14 (gray 1) "Slope -s = root, width = mult"
      , Color (hsb 0 85 100) (thickPath 3 [ pt env (gx i) (gy (c U.! i)) | i <- hull ]) ]
      ++ concat [ [ dotP env (gx i) (gy (c U.! i)) 12 (hsb (hueOf i) 70 100)
                  , txt env (gx i - 16) (gy (c U.! i) - 12) 14 (gray 0.8) (nf 1 (c U.! i)) ] | i <- idx ]

chunks2 :: [a] -> [[a]]
chunks2 [] = []
chunks2 xs = let (a, b) = splitAt 2 xs in a : chunks2 b

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hit = [i | i <- [0 .. wD w], dist2 m (handlePos env w i) < 16]
    in pure w { wDrag = if null hit then -1 else last hit }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let i = wDrag w
        (_, my) = mouseP env
        v = clampF (-12) 12 (myOf env my - fromIntegral i * handleX (wD w) i)
    in pure w { wCoef = wCoef w U.// [(i, v)] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wD = min 6 (wD w + 1), wDrag = -1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wD = max 1 (wD w - 1), wDrag = -1 }
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> do
        vs <- mapM (\i -> do r <- randomRIO (-4, 4 :: Float)
                             pure (fromIntegral (floor (r * 2 + 0.5) :: Int) / 2 + fromIntegral (i :: Int) * 0.5)) [0 .. 6]
        pure w { wCoef = U.fromList vs }
    | ch == '0' -> pure w { wCoef = U.replicate 7 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 係数を全部 0 にすると根は 1 つ（x = 0）で重複度 d。なぜ？（下側凸包が 1 本の線分になる）
-- 2. a_1 をうんと上げると、項 1 は包絡線から消える。そのとき根はどうなる？ 重複度は？
-- 3. 根がちょうど 1, 2, 3 になるように係数を決めてみよう（ヒント：因数分解の式から逆算）
-- 4. minimum を maximum に変えると（上包絡線）、根は点 (i, a_i) の「上側」凸包から出る。コードを書き換えて確かめよう


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
