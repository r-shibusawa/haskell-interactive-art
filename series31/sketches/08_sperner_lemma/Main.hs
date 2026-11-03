-- ============================================
-- 08 スペルナーの補題
-- 大きな三角形を小さな三角形に分け、頂点を 3 色でぬる。ルール：辺の上の点はその辺の両端の色のどちらか
--   すると 3 色そろった小三角形が必ずある（しかも奇数個）。赤−緑の辺を「ドア」として部屋をたどると必ず見つかる
--   M = ブラウワー版：色を写像 f で決める。3 色の小三角形は不動点のすぐ近く → 分割を細かくすると不動点に縮む
-- クリック = 内側の点の色を変える / ↑↓ = 分割の細かさ / スペース = ドアをたどる / M = ふつう ↔ ブラウワー版 / ←→ = 写像を変える / R = ぬり直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (foldl', nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

type Vtx = (Int, Int)            -- 頂点 (i, j)
type Tri = (Vtx, Vtx, Vtx)       -- 小三角形
type Lam = (Float, Float, Float) -- 重心座標

data World = World
  { wN      :: !Int              -- ★ 1 辺の分割数
  , wCols   :: !(U.Vector Int)   -- 色 0/1/2（colIx で引く）
  , wMode   :: !Int              -- 0：手でぬる、1：写像 f でぬる（ブラウワー版）
  , wPhi    :: !Float            -- ★ 写像のパラメータ
  , wTris   :: !(V.Vector Tri)
  , wWalk   :: ![Int]            -- たどった部屋
  , wShown  :: !Int
  , wPick   :: !Int
  , wFixed  :: !Lam
  }

main :: IO ()
main = runSketch (sketch "31-08 Sperner's Lemma" (World 8 U.empty 0 0.8 V.empty [] 0 0 (0, 0, 0)))
  { skBack  = hsb 230 30 11
  , skStart = \_ -> build (World 8 U.empty 0 0.8 V.empty [] 0 0 (0, 0, 0))
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

colIx :: Int -> Int -> Int -> Int
colIx n i j = i * (n + 1) + j

colorOf :: World -> Vtx -> Int
colorOf w (i, j) = wCols w U.! colIx (wN w) i j

-- 使ってよい色：i が A の重み、j が B、k = n − i − j が C
allowed :: Int -> Int -> Int -> [Int]
allowed n i j = [0 | i > 0] ++ [1 | j > 0] ++ [2 | n - i - j > 0]

build :: World -> IO World
build w = recolor w { wTris = V.fromList (makeTris (wN w)) }

makeTris :: Int -> [Tri]
makeTris n = concat
  [ ((i, j), (i + 1, j), (i, j + 1)) : [((i + 1, j), (i, j + 1), (i + 1, j + 1)) | i + j < n - 1]
  | i <- [0 .. n - 1], j <- [0 .. n - 1 - i] ]

recolor :: World -> IO World
recolor w = do
  let n = wN w
      vs = [(i, j) | i <- [0 .. n], j <- [0 .. n - i]]
  cs <- forM vs $ \(i, j) ->
    if wMode w == 1
      then pure (brouwerColor (wPhi w) (lamOf n i j))
      else case allowed n i j of
        [] -> pure 0
        al -> do
          k <- randomRIO (0, length al - 1)
          pure (al `at'` k)
  let cols = U.replicate ((n + 1) * (n + 1)) 0 U.// zip [colIx n i j | (i, j) <- vs] cs
  pure w { wCols = cols, wWalk = [], wShown = 0
         , wFixed = if wMode w == 1 then findFixed (wPhi w) else wFixed w }

lamOf :: Int -> Int -> Int -> Lam
lamOf n i j = let nf' = fromIntegral n in (fromIntegral i / nf', fromIntegral j / nf', fromIntegral (n - i - j) / nf')

-- ★ 三角形を自分自身へ写す連続写像（重心座標で）。f の成分の和は 1 のまま
fmap' :: Float -> Lam -> Lam
fmap' phi (l0, l1, l2) =
  let a0 = l0 + 0.9 * l1 * (1 + sin (5 * l2 + phi))
      a1 = l1 + 0.9 * l2 * (1 + sin (4 * l0 + 2 * phi))
      a2 = l2 + 0.9 * l0 * (1 + sin (3 * l1 - phi))
      sm = a0 + a1 + a2
  in (a0 / sm, a1 / sm, a2 / sm)

-- 色 = f で「減る（増えない）」座標の番号。その座標が 0 でない中から選ぶ → スペルナーのルールを自動で満たす
brouwerColor :: Float -> Lam -> Int
brouwerColor phi l@(l0, l1, l2)
  | l0 > 0 && f0 <= l0 = 0
  | l1 > 0 && f1 <= l1 = 1
  | l2 > 0 && f2 <= l2 = 2
  | l0 > 0 = 0
  | l1 > 0 = 1
  | otherwise = 2
  where (f0, f1, f2) = fmap' phi l

-- 細かい格子で |f(λ) − λ| が最小の点を探す
findFixed :: Float -> Lam
findFixed phi = snd (foldl' pick (1e9, (0, 0, 0)) grid)
  where
    gridN = 120 :: Int
    grid = [lamOf gridN i j | i <- [0 .. gridN], j <- [0 .. gridN - i]]
    pick best@(bd, _) l@(a, b, c) =
      let (fa, fb, fc) = fmap' phi l
          d = abs (fa - a) + abs (fb - b) + abs (fc - c)
      in if d < bd then (d, l) else best

triVerts :: Tri -> [Vtx]
triVerts (a, b, c) = [a, b, c]

isFull :: World -> Tri -> Bool
isFull w t = length (nub (map (colorOf w) (triVerts t))) == 3

triEdges :: Tri -> [(Vtx, Vtx)]
triEdges (a, b, c) = [(a, b), (b, c), (c, a)]

edgeKey :: (Vtx, Vtx) -> (Vtx, Vtx)
edgeKey (u, v) = if u < v then (u, v) else (v, u)

-- ドアをたどる：底辺 AB（k = 0、色は赤か緑）の上の赤−緑の辺から入り、反対のドアから出る、をくり返す
startWalk :: World -> World
startWalk w = case doors of
  [] -> w
  _ ->
    let door = doors `at'` (wPick w `mod` length doors)
        doorK = edgeKey door
        edgeTris = M.fromListWith (flip (++))
          [ (edgeKey e, [ti]) | (ti, t) <- zip [0 ..] (V.toList (wTris w)), e <- triEdges t ]
        go :: Int -> Int -> (Vtx, Vtx) -> [Int]
        go 0 _ _ = []
        go guard cur inDoor =
          let t = wTris w V.! cur
              outs = [ edgeKey e | e@(u, v) <- triEdges t, sort [colorOf w u, colorOf w v] == [0, 1], edgeKey e /= inDoor ]
              -- 外へ出た（別の底辺のドア）なら終わり
              next = do
                out <- lastMaybe outs
                nb <- M.lookup out edgeTris
                c <- firstMaybe (filter (/= cur) nb)
                pure (c, out)
          in cur : if isFull w t then [] else case next of
               Nothing -> []
               Just (c, out) -> go (guard - 1) c out
        walk = case M.lookup doorK edgeTris of
          Just (c : _) -> go (V.length (wTris w) + 2) c doorK
          _ -> []
    in w { wWalk = walk, wShown = 0, wPick = wPick w + 1 }
  where
    n = wN w
    doors = [ (u, v) | i <- [0 .. n - 1], let u = (i, n - i), let v = (i + 1, n - i - 1)
                     , colorOf w u /= colorOf w v, colorOf w u /= 2, colorOf w v /= 2 ]

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wShown w < length (wWalk w) && envFrame env `mod` 8 == 0 = pure w { wShown = wShown w + 1 }
  | otherwise = pure w

-- レイアウト：A 左下、B 右下、C 上
triSize :: Env -> Float
triSize env = min (envW env * 0.55) ((envH env - 140) / 0.866)

posOf :: Env -> Int -> Int -> Int -> Point
posOf env n i j =
  let s = triSize env
      ax = 50
      ay = 100 + s * 0.866
      bx = 50 + s
      cx = 50 + s / 2
      cy = 100
      nn = fromIntegral n
      fi = fromIntegral i
      fj = fromIntegral j
      fk = fromIntegral (n - i - j)
  in ((fi * ax + fj * bx + fk * cx) / nn, (fi * ay + fj * ay + fk * cy) / nn)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ Color (hsba 45 90 100 0.85) (Polygon (triPts t)) | t <- fulls ]
  ++ [ Color (hsb 0 0 45) (Line (let ps = triPts t in ps ++ take 1 ps)) | t <- V.toList (wTris w) ]
  ++ [ Color (hsba 0 0 100 0.3) (Polygon (triPts (wTris w V.! ti))) | ti <- shownWalk ]
  ++ [ ln 3 white (center (wTris w V.! a)) (center (wTris w V.! b)) | (a, b) <- zip shownWalk (drop 1 shownWalk) ]
  ++ [ spot (hsb (hueOf (wCols w U.! colIx n i j)) 80 95) (posOf env n i j) r | i <- [0 .. n], j <- [0 .. n - i] ]
  ++ crossMark
  ++ [ txt 20 34 15 white ((if wMode w == 0 then "Paint by hand" else "Brouwer version (colored by f)") ++ "   n = " ++ show n ++ " (Up/Down)   click: change color   Space: walk the doors   M: switch" ++ (if wMode w == 1 then "   Left/Right: map" else "") ++ "   R: repaint")
     , txt 20 62 14 (hsb 0 0 60) "Edge rule: bottom edge red or green, left edge red or blue, right edge green or blue (corners fixed). Inside is free."
     , txt tx 150 20 (hsb 45 90 100) ("3-color triangles: " ++ show full ++ " (" ++ (if odd full then "odd" else "even") ++ ")")
     ]
  ++ [ txt tx (190 + fromIntegral i * 26) 14 (hsb 0 0 80) l | (i, l) <- zip [0 :: Int ..] notes ]
  where
    n = wN w
    fulls = filter (isFull w) (V.toList (wTris w))
    full = length fulls
    triPts t = [posOf env n i j | (i, j) <- triVerts t]
    center t = let ps = triPts t
               in (sum (map fst ps) / 3, sum (map snd ps) / 3)
    shownWalk = take (wShown w) (wWalk w)
    r = clampF 5 16 (140 / fromIntegral n)
    hueOf :: Int -> Float
    hueOf 0 = 0
    hueOf 1 = 130
    hueOf _ = 215
    s = triSize env
    tx = 50 + s + 50
    -- ブラウワー版：本当の不動点
    crossMark
      | wMode w == 1 =
          let (l0, l1, l2) = wFixed w
              ax = 50
              ay = 100 + s * 0.866
              bx = 50 + s
              cx = 50 + s / 2
              cy = 100
              px = l0 * ax + l1 * bx + l2 * cx
              py = l0 * ay + l1 * ay + l2 * cy
          in [ hoop 2 white (px, py) 24, ln 2 white (px - 16, py) (px + 16, py), ln 2 white (px, py - 16) (px, py + 16) ]
      | otherwise = []
    notes
      | wMode w == 0 =
          [ "However you paint, the number is odd: never 0."
          , ""
          , "Door argument: call a red-green edge a \"door\"."
          , "- The bottom edge has an odd number of doors."
          , "- A 3-color room has 1 door, others 0 or 2."
          , "- Enter by a bottom door, leave by the other"
          , "  door, repeat: you exit by another bottom door"
          , "  or stop in a 3-color room."
          , "- Bottom doors are odd in number, so some walk"
          , "  ends in a 3-color room."
          , ""
          , "Space: walk from each bottom door (white line)"
          ]
      | otherwise =
          [ "Color c: coordinate c does not grow under f"
          , "(f_c <= lambda_c)."
          , "This obeys the edge rule automatically."
          , ""
          , "At the 3 corners of a 3-color triangle, each of"
          , "the 3 coordinates does not grow. If the triangle"
          , "is small, then f(lambda) is about lambda there."
          , ""
          , "Up: larger n, and the 3-color triangles shrink"
          , "to the white cross (the true fixed point)."
          , "This is one proof of Brouwer's theorem."
          ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ | wMode w == 0 ->
    let n = wN w
        m = mouseP env
        hits = [(i, j) | i <- [0 .. n], j <- [0 .. n - i], dist2 m (posOf env n i j) < 12]
        cycleC cols (i, j) =
          let al = allowed n i j
              c = cols U.! colIx n i j
              next = case dropWhile (/= c) al of
                (_ : x : _) -> x
                _ -> case al of { (x : _) -> x; [] -> c }
          in cols U.// [(colIx n i j, next)]
    in pure (if null hits then w else w { wCols = foldl' cycleC (wCols w) hits, wWalk = [], wShown = 0 })
  EventKey (SpecialKey KeyUp) Down _ _ ->
    build w { wN = min 40 (wN w + (if wN w >= 16 then 4 else 1)) }
  EventKey (SpecialKey KeyDown) Down _ _ ->
    build w { wN = max 2 (wN w - (if wN w > 16 then 4 else 1)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (startWalk w)
  EventKey (SpecialKey KeyLeft) Down _ _ | wMode w == 1 -> recolor w { wPhi = wPhi w - 0.2 }
  EventKey (SpecialKey KeyRight) Down _ _ | wMode w == 1 -> recolor w { wPhi = wPhi w + 0.2 }
  EventKey (Char c) Down _ _
    | toLower c == 'm' -> recolor w { wMode = 1 - wMode w }
    | toLower c == 'r' -> recolor w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 内側の点の色をクリックで変えて、3 色の三角形を 0 個にしようとしてみよう。1 個まで減らせる？
-- 2. 底辺の色を見て、赤−緑のドアの枚数を数えよう。いつも奇数になるのはなぜ？（端が赤と緑だから）
-- 3. ブラウワー版で n = 4 → 40 と細かくしていき、黄色の三角形が白い十字に集まるのを確かめよう
-- 4. 3 次元（四面体と 4 色）でも同じことが成り立つ。ドアは何色の面になる？

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

-- 添字が範囲内と分かっているときのリスト参照
at' :: [a] -> Int -> a
at' xs i = case drop i xs of
  (x : _) -> x
  [] -> error "index out of range"

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

firstMaybe :: [a] -> Maybe a
firstMaybe (x : _) = Just x
firstMaybe [] = Nothing
