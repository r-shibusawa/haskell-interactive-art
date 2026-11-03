-- ============================================
-- 07 群の作用と軌道 ─ バーンサイドの補題
-- n 個のビーズに k 色を塗る。回したり裏返したりして重なるものは「同じネックレス」= 同じ軌道
--   異なるネックレスの数 = （各対称性 g で変わらない塗り方の数）の平均 = (1/|G|) Σ k^(g の巡回の数)
-- ←→ = ビーズの数 n / ↑↓ = 色の数 k / D = 裏返しも同じとみなす / ビーズをクリック = 色を変える / X = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Array
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U

-- 群の元：ビーズの位置の置換
data GroupEl = GroupEl
  { gPerm    :: ![Int]
  , gName    :: String
  , gCyc     :: !Int              -- 巡回の数
  , gWeights :: !(U.Vector Int)   -- この元で動かした塗り方の番号を計算するための重み
  }

data World = World
  { wN     :: !Int         -- ★ ビーズの数
  , wK     :: !Int         -- ★ 色の数
  , wFlip  :: !Bool        -- 裏返しを含める（二面体群）か、回転だけ（巡回群）か
  , wBeads :: ![Int]
  , wG     :: ![GroupEl]
  , wCount :: !Int         -- 軌道の数（実際に全部調べた結果）
  , wReps  :: !Picture     -- 代表のネックレスを並べた絵（パラメータが変わったときだけ描く）
  }

palette :: Array Int Color
palette = listArray (0, 3) [rgb255 0xf4 0xf1 0xe8, rgb255 0xe2 0x48 0x3d, rgb255 0x2f 0x6f 0xdf, rgb255 0xf0 0xb4 0x29]

main :: IO ()
main = runSketch (sketch "23-07 Group Action and Burnside" (rebuild (World 6 2 True [] [] 0 Blank)))
  { skBack  = rgb255 28 30 40
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

rebuild :: World -> World
rebuild w =
  let n = wN w
      k = wK w
      r = [0 .. n - 1]
      mk p nm = GroupEl p nm (cycleCount p) (U.fromList [k ^ q | q <- p])
      rots = [mk [(i + j) `mod` n | i <- r] ("rot " ++ show j) | j <- r]
      flips = if wFlip w then [mk [(j - i + n) `mod` n | i <- r] ("flip " ++ show j) | j <- r] else []
      g = rots ++ flips
      beads | length (wBeads w) == n = map (`mod` k) (wBeads w)
            | otherwise = map (`mod` k) r
      -- 全部の塗り方を調べ、軌道の代表（いちばん小さい数）だけ集める
      total = k ^ n
      pows = U.fromList [k ^ i | i <- r]
      decodeV code = U.map (\p -> (code `div` p) `mod` k) pows
      isMin code =
        let c = decodeV code
        in all (\e -> U.sum (U.zipWith (*) c (gWeights e)) >= code) g
      reps = filter isMin [0 .. total - 1]
      count = length reps
      -- 表と絵の配置
      layerX = width * 0.5
      layerY = repLayerY n
      layerW = fromIntegral (floor (width * 0.42) :: Int) :: Float
      layerH = height - 400
      s = 46 :: Float
      cols = max 1 (floor (layerW / s)) :: Int
      maxShow = cols * floor (layerH / s)
      decodeL code = U.toList (decodeV code)
      repPics =
        [ necklace (layerX + fromIntegral (i `mod` cols) * s + s / 2)
                   (layerY + fromIntegral (i `div` cols) * s + s / 2) (s * 0.32) (decodeL c) 4
        | (i, c) <- zip [0 ..] (take maxShow reps) ]
      more = [ txt (layerX + 4) (layerY + layerH - 6) 14 white ("... and " ++ show (count - maxShow) ++ " more") | count > maxShow ]
  in w { wG = g, wBeads = beads, wCount = count, wReps = Pictures (repPics ++ more) }

-- 表の下の「代表のネックレス」の位置
rowHeight :: Int -> Float
rowHeight n = min 22 (height * 0.42 / fromIntegral n)

repLayerY :: Int -> Float
repLayerY n = 110 + 40 + rowHeight n * fromIntegral n + 60

cycleCount :: [Int] -> Int
cycleCount p =
  let a = listArray (0, length p - 1) p :: Array Int Int
      go seen i cnt
        | i >= length p = cnt
        | IS.member i seen = go seen (i + 1) cnt
        | otherwise = go (mark seen i) (i + 1) (cnt + 1)
      mark seen j | IS.member j seen = seen
                  | otherwise = mark (IS.insert j seen) (a ! j)
  in go IS.empty 0 (0 :: Int)

-- 塗り方の番号（k 進数）
encode :: Int -> [Int] -> Int
encode k = foldr (\d v -> v * k + d) 0

-- 置換を塗り方に作用させる：out[p[i]] = c[i]
applyG :: [Int] -> [Int] -> [Int]
applyG p c = IM.elems (IM.fromList (zip p c))

necklace :: Float -> Float -> Float -> [Int] -> Float -> Picture
necklace x y r c br = Pictures
  ( Color (makeColorI 255 255 255 60) (at (pt (x, y)) (Circle r))
  : concat
    [ [ discAt bx by (br + 1) (gray (20 / 255))
      , discAt bx by br (palette ! (v `mod` 4)) ]
    | (i, v) <- zip [0 :: Int ..] c
    , let a = -pi / 2 + fromIntegral i * tau / fromIntegral (length c)
          bx = x + r * cos a
          by = y + r * sin a ] )

-- 大きなネックレスの位置
bigCx, bigCy, bigR :: Float
bigCx = width * 0.2
bigCy = height * 0.36
bigR = min (width * 0.13) (height * 0.2)

drawWorld :: World -> Picture
drawWorld w = Pictures ([necklace bigCx bigCy bigR (wBeads w) 22] ++ orbitPart ++ table ++ [wReps w] ++ hud)
  where
    n = wN w
    k = wK w
    g = wG w
    size = length g
    -- その軌道（重ならないものだけ）と安定化群
    images = [applyG (gPerm e) (wBeads w) | e <- g]
    orbit = IM.fromList [(encode k c, c) | c <- images]
    stab = length (filter (== encode k (wBeads w)) (map (encode k) images))
    orb = IM.elems orbit
    orbitPart =
      txt 30 (bigCy + bigR + 60) 16 white
        ("Orbit: " ++ show (length orb) ++ "   Stabilizer: " ++ show stab ++ "   -> " ++ show (length orb) ++ " x " ++ show stab ++ " = " ++ show size ++ " = |G|")
      : [ necklace (50 + fromIntegral (i `mod` 12) * 50) (bigCy + bigR + 100 + fromIntegral (i `div` 12) * 50) 16 c 5
        | (i, c) <- zip [0 :: Int ..] orb ]
    -- バーンサイドの表（左の列 = 回転、右の列 = 鏡映）
    px = width * 0.5
    py = 110 :: Float
    rowH = rowHeight n
    fixes = [k ^ gCyc e | e <- g]
    total = sum fixes
    y = py + 40 + rowH * fromIntegral n
    avg = total `div` size
    table =
      txt px py 16 white "Burnside: count the colorings each symmetry g leaves fixed"
      : [ txt (px + (if i < n then 0 else 300)) (py + 30 + fromIntegral (i `mod` n) * rowH) 14 (gray 0.78)
            (gName e ++ ": " ++ show (gCyc e) ++ " cyc -> " ++ show k ++ "^" ++ show (gCyc e) ++ " = " ++ show f)
        | (i, e, f) <- zip3 [0 ..] g fixes ]
      ++ [ txt px (y + 10) 22 (rgb255 255 210 90) ("(" ++ show total ++ ") / " ++ show size ++ " = " ++ show avg)
         , txt px (y + 40) 16 white ("Checked all " ++ show (k ^ n) ++ " colorings: " ++ show (wCount w) ++ " orbits"
             ++ (if wCount w == avg && total `mod` size == 0 then "   OK, they match" else "")) ]
    hud =
      [ txt 20 36 18 white ("Left/Right: n = " ++ show n ++ "   Up/Down: k = " ++ show k ++ "   D: "
          ++ (if wFlip w then "rotations + flips (D" else "rotations only (C") ++ show n ++ ")   Click bead: color   X: random")
      , txt 20 62 15 (gray (170 / 255)) "A group acting on things splits them into orbits. Orbit size x stabilizer size = |G|"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let mouse = toP5 (envMouse env)
        n = wN w
        hit i = let a = -pi / 2 + fromIntegral i * tau / fromIntegral n
                in dist2 mouse (bigCx + bigR * cos a, bigCy + bigR * sin a) < 24
    in pure w { wBeads = [if hit i then (b + 1) `mod` wK w else b | (i, b) <- zip [0 :: Int ..] (wBeads w)] }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (rebuild w { wN = min 8 (wN w + 1) })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (rebuild w { wN = max 3 (wN w - 1) })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuild w { wK = min 4 (wK w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuild w { wK = max 2 (wK w - 1) })
  EventKey (Char c) Down _ _
    | c == 'd' || c == 'D' -> pure (rebuild w { wFlip = not (wFlip w) })
    | c == 'x' || c == 'X' -> do
        bs <- replicateM (wN w) (randomRIO (0, wK w - 1))
        pure w { wBeads = bs }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 6, k = 2、回転だけだと 14 個、裏返しも入れると 13 個。どのネックレスが「裏返して初めて同じ」になる？
-- 2. 全部同じ色のネックレスの軌道は 1 個、安定化群は |G| 個。軌道 × 安定化群 = |G| を確かめよう
-- 3. 回転 j の巡回の数は gcd(j, n)。表で確かめよう
-- 4. 立方体の面を k 色で塗る数も同じ方法で数えられる（立方体の回転群は 24 個）。式をつくってみよう

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

discAt :: Float -> Float -> Float -> Color -> Picture
discAt x y r c = Color c (at (pt (x, y)) (disc r))
