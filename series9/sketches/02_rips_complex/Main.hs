-- ============================================
-- 02 リップス複体（点を線と三角形でつなぐ）
-- 円の重なりを、もっと計算しやすい形にする：
--   距離が 2r 以下の 2 点 → 線（辺）、3 点が全部 2r 以下 → 三角形（面）
-- 点・辺・面の数から オイラー標数 χ = V − E + F を計算。穴の数の手がかりになる
-- マウス左右 = 半径 r / 1〜3 で形 / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randomRs)

data World = World
  { wPts   :: ![Point]
  , wShape :: !Int
  , wSeed  :: !Int
  }

main :: IO ()
main = runSketch (sketch "09-02 Rips Complex" (World [] 1 2))
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (makePoints env (World [] 1 2))
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

makePoints :: Env -> World -> World
makePoints env w = w { wPts = map sample (take 30 (randomChunks (wSeed w) 7)) }
  where
    cx = envW env / 2
    cy = envH env / 2 + 30
    bigR = min (envW env) (envH env) * 0.25
    sample (ua : us : ur : g1 : g2 : g3 : g4 : _) =
      let a = ua * tau
          s = if us < 0.5 then -1 else 1
          (x, y) = case wShape w of
            1 -> (cx + cos a * bigR, cy + sin a * bigR)
            2 -> (cx + s * bigR * 1.1 + cos a * bigR * 0.6, cy + sin a * bigR * 0.6)
            _ -> let rr = bigR * sqrt ur in (cx + cos a * rr, cy + sin a * rr)
      in (x + gaussian g1 g2 * 8, y + gaussian g3 g4 * 8)
    sample _ = (cx, cy)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawComplex env pts r 0.25 0.7          -- ★ 道具を使う
  -- 円（うすく）と点
  , Color (hsba 0 0 100 0.06) (Pictures [at (toG env x y) (disc r) | (x, y) <- pts])
  , Color (hsb 0 0 100) (Pictures [at (toG env x y) (disc 3.5) | (x, y) <- pts])
  , txt env 20 40 18 (hsb 0 0 100)
      ("r = " ++ show (round r :: Int) ++ " (mouse X)  V = " ++ show v ++ "  E = " ++ show e ++ "  F = " ++ show f
        ++ "  chi = V - E + F = " ++ show (v - e + f) ++ "  1-3: shape  Click: new")
  , txt env 20 68 15 (hsb 0 0 60)
      "Euler characteristic chi = (blobs) - (holes). One blob with chi = 0: one hole. chi = -1: two holes."
  , txt env 20 90 15 (hsb 0 0 60)
      "(Overlapping triangles are all counted, so chi stops being accurate when r is large.)"
  ]
  where
    pts = wPts w
    r = mapC (fst (mouseXY env)) 0 (envW env) 2 160
    cx = ripsComplex pts r
    v = length pts
    e = length (cxEdges cx)
    f = length (cxTriangles cx)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure (makePoints env w { wShape = fromEnum c - fromEnum '0' })
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure (makePoints env w { wSeed = s })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 円（1）で r を上げていき、χ が 0 になる区間を探す（塊 1 つ・穴 1 つ）
-- 2. 2 つの円（2）で χ = −1 になる区間は？
-- 3. r をとても大きくすると三角形だらけで χ が大きく狂う。なぜ？（4 点が全部つながると三角形 4 つ＝中身の詰まった四面体のはずが…）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 範囲からはみ出さない remap（p5.js の map(v, a, b, c, d, true)）
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- 種から決まる 0〜1 の乱数を k 個ずつ区切った列（p5.js の randomSeed + random）。
-- 同じ種なら毎回同じ点群になる
randomChunks :: Int -> Int -> [[Float]]
randomChunks seed k = go (randomRs (0, 1) (mkStdGen seed))
  where go us = let (a, b) = splitAt k us in a : go b

-- 0〜1 の乱数 2 つから正規分布の乱数を作る（p5.js の randomGaussian）
gaussian :: Float -> Float -> Float
gaussian u v = sqrt (-2 * log (max 1e-6 u)) * cos (tau * v)


-- ここでは点は 2 次元
type Pt = Point

ptDist :: Pt -> Pt -> Float
ptDist = dist2
-- ==================================================
-- ここから下は「TDA の道具」です（リップス複体とパーシステント・ホモロジーの計算）
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- リップス複体：edges は (i, j, 長さ)、triangles は (i, j, k, 最長辺の長さ)
data Complex = Complex
  { cxEdges     :: [(Int, Int, Float)]
  , cxTriangles :: [(Int, Int, Int, Float)]
  }

-- 半径 r（直径 2r）以内の点どうしをつなぐ
ripsComplex :: [Pt] -> Float -> Complex
ripsComplex pts r = Complex edges triangles
  where
    n = length pts
    lim = 2 * r
    table = U.fromList [ptDist a b | a <- pts, b <- pts]
    d i j = table U.! (i * n + j)
    edges = [(i, j, d i j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], d i j <= lim]
    triangles =
      [ (i, j, k, max dij (max (d j k) (d i k)))
      | (i, j, dij) <- edges, k <- [j + 1 .. n - 1], d i k <= lim, d j k <= lim ]

-- 複体を描く：三角形（透明度 triA）と辺（透明度 edgeA）
drawComplex :: Env -> [Point] -> Float -> Float -> Float -> Picture
drawComplex env pts r triA edgeA = Pictures
  [ Color (hsba 170 60 60 triA) (Pictures [Polygon [p i, p j, p k] | (i, j, k, _) <- cxTriangles cx])
  , Color (hsba 170 60 100 edgeA) (Pictures [thickLine 1.5 (p i) (p j) | (i, j, _) <- cxEdges cx])
  ]
  where
    cx = ripsComplex pts r
    table = U.fromList (map (uncurry (toG env)) pts)
    p i = table U.! i
