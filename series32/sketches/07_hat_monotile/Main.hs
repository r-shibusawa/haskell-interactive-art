-- ============================================
-- 07 アインシュタイン・タイル（帽子）
-- 2023 年に見つかった「1 種類だけで、くり返さない模様しか作れない」タイル（ドイツ語 ein Stein = 1 つの石）
--   形は六角形の格子を 6 つに割った「カイト（凧形）」8 枚をつないだ 13 角形。裏返したタイルも使う
--   ここでは帽子を 1 枚ずつ置いて、すき間も重なりもない円形の「パッチ」を探索で作る（青 = 裏返した帽子）
-- スペース = 新しく探す / F = 一気に完成 / ↑↓ = パッチの大きさ / G = カイトの格子 / S = 保存
-- ============================================
module Main (main) where

import Art
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTime)
import System.Random (StdGen, mkStdGen, randomR)

type Kite = (Int, Int, Int)

data Place = Place { pO, pDi, pDj :: !Int }    -- 向きの番号と、ずらす量

-- 探索の記録：残りの候補（先頭がいま置いている候補）
type Frame = [Place]

data World = World
  { wRadius :: !Int        -- ★ パッチの半径（カイトの短い辺 = 1）
  , wGrid   :: !Bool
  , wSeed   :: !Int
  , wRegion :: [Kite]      -- パッチの中のカイト
  , wCover  :: !IS.IntSet  -- 埋まっているカイト
  , wStack  :: [Frame]     -- 探索の記録（先頭が最新）
  , wDone   :: !Bool
  , wSteps  :: !Int
  , wGen    :: !StdGen
  , wTurbo  :: !Double     -- F：この時刻まで全力で探す
  }

speed :: Int
speed = 2                  -- ★ 1 フレームに何手進めるか

world0 :: World
world0 = restart (World 10 True 1 [] IS.empty [] False 0 (mkStdGen 1) 0)

main :: IO ()
main = runSketch (sketch "32-07 Hat Monotile" world0)
  { skBack  = rgb255 244 240 230
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawAll
  , skEvent = onEvent
  }

restart :: World -> World
restart w =
  let r = fromIntegral (wRadius w) :: Float
      region = [ (i, j, k) | i <- [-14 .. 14], j <- [-14 .. 14], k <- [0 .. 5]
               , norm2 (kiteCenter (i, j, k)) < r ]
  in w { wRegion = region, wCover = IS.empty, wStack = [], wDone = False, wSteps = 0, wGen = mkStdGen (wSeed w) }

stepWorld :: World -> IO World
stepWorld w
  | wDone w = pure w
  | wTurbo w > 0 = do
      t <- getMonotonicTime
      if t > wTurbo w then pure w { wTurbo = 0 } else turbo (t + 0.02) w
  | otherwise = pure (foldl' (\a _ -> if wDone a then a else stepSearch a) w [1 .. speed])

-- 画面が止まらないよう、1 フレームに 20 ミリ秒まで探す
turbo :: Double -> World -> IO World
turbo tEnd w
  | wDone w = pure w
  | otherwise = do
      t <- getMonotonicTime
      if t > tEnd then pure w else turbo tEnd (stepSearch w)

-- ★ 探索の 1 手：まだ埋まっていないカイトのうち「置き方がいちばん少ない」所に帽子を置く。置けなければ戻る
stepSearch :: World -> World
stepSearch w0
  | wSteps w0 >= 6000 = reseed w0              -- 長引いたら別の順番でやり直す
  | otherwise =
      let w = w0 { wSteps = wSteps w0 + 1 }
      in case bestKite w of
           Nothing -> w { wDone = True }
           Just (_, []) -> backtrack w
           Just (_, cs) ->
             -- 表の帽子を先に、同じ中ではランダムな順番
             let (shuffled, g') = shuffle cs (wGen w)
                 ordered = sortOn (\p -> oRef (orientAt (pO p))) shuffled
             in case ordered of
                  [] -> w
                  (p : _) -> w { wGen = g', wStack = ordered : wStack w, wCover = place p True (wCover w) }

reseed :: World -> World
reseed w = restart w { wSeed = wSeed w + 1 }

bestKite :: World -> Maybe (Kite, [Place])
bestKite w = go Nothing (wRegion w)
  where
    cov = wCover w
    go best [] = best
    go best (kt@(i, j, k) : rest)
      | IS.member (kkey i j k) cov = go best rest
      | otherwise =
          let c = candidates cov kt
              better = case best of
                Nothing -> True
                Just (_, bc) -> shorter c bc
              best' = if better then Just (kt, c) else best
          in if null c then best' else go best' rest
    shorter (_ : a) (_ : b) = shorter a b
    shorter [] (_ : _) = True
    shorter _ _ = False

-- 行き止まり → 1 つ前に戻って次の候補へ
backtrack :: World -> World
backtrack w = case wStack w of
  [] -> reseed w
  (fr : rest) -> case fr of
    [] -> backtrack w { wStack = rest }
    (p : others) ->
      let cov = place p False (wCover w)
      in case others of
           (q : _) -> w { wStack = others : rest, wCover = place q True cov }
           [] -> backtrack w { wStack = rest, wCover = cov }

-- カイト (I, J, K) を覆う帽子の置き方の候補
candidates :: IS.IntSet -> Kite -> [Place]
candidates cov (ci, cj, ck) =
  [ Place oi di dj
  | (oi, o) <- zip [0 ..] (V.toList orients)
  , (i, j, k) <- oKs o
  , k == ck
  , let di = ci - i
        dj = cj - j
  , not (any (\(a, b, c) -> IS.member (kkey (a + di) (b + dj) c) cov) (oKs o)) ]

place :: Place -> Bool -> IS.IntSet -> IS.IntSet
place p on cov =
  foldl' (\s (a, b, c) -> let k = kkey (a + pDi p) (b + pDj p) c
                          in if on then IS.insert k s else IS.delete k s)
         cov (oKs (orientAt (pO p)))

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      r = fromIntegral (wRadius w) :: Float
      z = min (wd * 0.55) (h - 200) / (2 * r + 6)   -- 画面に収まる大きさ
      cx = wd * 0.62
      cy = 90 + (h - 200) / 2
      toPx (x, y) = (cx + x * z, cy + y * z)
      gridPic = if wGrid w
        then Color (rgb255 210 200 185) (Pictures [ Line (closed (map toPx (kitePoly kt))) | kt <- wRegion w ])
        else Blank
      placed = [ p | (p : _) <- wStack w ]
      hatPic p =
        let o = orientAt (pO p)
            col | oRef o = rgb255 40 90 170
                | otherwise = case oRot o `mod` 3 of
                    0 -> rgb255 250 200 120
                    1 -> rgb255 240 150 110
                    _ -> rgb255 150 200 160
        in Pictures
          [ Color col (Pictures [ Polygon (map (toPx . placeV p) part) | part <- hatParts ])
          , Color (rgb255 40 36 30) (thickPath 1.5 (closed (map (toPx . placeV p) hatOutline))) ]
      nRefl = length (filter (oRef . orientAt . pO) placed)
      status = "Placed " ++ show (length placed) ++ " hats (" ++ show nRefl ++ " flipped)   steps " ++ show (wSteps w)
               ++ (if wDone w then "   Done!" else "   searching...")
      dark = gray 0.27
  in Pictures
    [ onP5 env (Pictures
        ( gridPic
        : map hatPic (reverse placed)
        ++ [ at (cx, cy) (Color (rgb255 180 60 60) (ring (r * z) 2))
           , drawSingleHat 130 200 30 ] ))
    , txt env 70 (200 + 3.4 * 30) 14 dark "1 hat = 8 kites (13-gon)"
    , txt env 20 36 20 (gray 0.12) "The hat: one tile that only makes non-repeating patterns"
    , txt env 20 62 15 dark status
    , txt env 20 (h - 84) 15 dark "2023: Smith (amateur), Myers, Kaplan, Goodman-Strauss"
    , txt env 20 (h - 60) 15 dark "The true infinite tiling groups hats into 4 clusters (H, T, P, F) and keeps enlarging them"
    , txt env 20 (h - 36) 15 dark "Here: a patch searched only by no gaps / no overlaps. Few flipped hats since flipped ones are tried last"
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (reseed w)
  EventKey (SpecialKey KeyUp) Down _ _    -> pure (restart w { wRadius = min 13 (wRadius w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _  -> pure (restart w { wRadius = max 5 (wRadius w - 1) })
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> do
        t <- getMonotonicTime
        pure w { wTurbo = t + 1.5 }
    | c == 'g' || c == 'G' -> pure w { wGrid = not (wGrid w) }
    | c == 's' || c == 'S' -> saveSvg "hat" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 左の帽子のカイトを数えよう（8 枚）。13 本の辺の長さは 1 と √3 と 2 の 3 種類
-- 2. スペースで何度も探す。裏返しの帽子（青）は、まわりの表の帽子に囲まれてポツンと現れる
-- 3. ↑ で大きくすると、行き止まりと戻りが増えて時間がかかる。くり返し模様が作れないことの感覚
-- 4. 調べてみよう：裏返しを使わない「スペクター」タイル（2023 年）


-- ==================================================
-- ここから下は「帽子の道具」（三角格子・カイト・向き）
-- （中身は読まなくても OK）
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

closed :: [a] -> [a]
closed p = p ++ take 1 p

hr3 :: Float
hr3 = sqrt 3 / 2

hp :: Float -> Float -> Point
hp x y = (x + 0.5 * y, hr3 * y)

-- ★ 帽子の輪郭（三角格子の点 hp(x, y) をつないだ 13 角形）
hat :: [Point]
hat = [hp 0 0, hp (-1) (-1), hp 0 (-2), hp 2 (-2), hp 2 (-1), hp 4 (-2), hp 5 (-1), hp 4 0, hp 3 0, hp 2 2, hp 0 3, hp 0 2, hp (-1) 2]

-- 六角形の中心の格子
h1, h2 :: Point
h1 = hp 2 2
h2 = hp (-2) 4

kkey :: Int -> Int -> Int -> Int
kkey i j k = ((i + 60) * 200 + (j + 60)) * 6 + k

-- 六角形 (i, j) の k 番目のカイト：中心 → 辺の中点 → 頂点 → 辺の中点
kitePoly :: Kite -> [Point]
kitePoly (i, j, k) =
  let (ax, ay) = h1
      (bx, by) = h2
      c@(cx, cy) = (fromIntegral i * ax + fromIntegral j * bx, fromIntegral i * ay + fromIntegral j * by)
      s3 = sqrt 3
      d a r = (cx + r * cos (a * pi / 180), cy + r * sin (a * pi / 180))
      a0 = 60 * fromIntegral k
  in [c, d (a0 - 30) s3, d a0 2, d (a0 + 30) s3]

kiteCenter :: Kite -> Point
kiteCenter kt =
  let ps = kitePoly kt
      (sx, sy) = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) ps
  in (sx / 4, sy / 4)

-- 点に一番近いカイト
locateKite :: Point -> Kite
locateKite p@(px, py) =
  let (ax, ay) = h1
      (bx, by) = h2
      det = ax * by - ay * bx
      fi = floor ((px * by - py * bx) / det) :: Int
      fj = floor ((ax * py - ay * px) / det) :: Int
      visit (best, bd) kt =
        let d = dist2 (kiteCenter kt) p
        in if d < bd then (kt, d) else (best, bd)
  in fst (foldl' visit ((0, 0, 0), 1e9)
           [ (i, j, k) | i <- [fi - 1 .. fi + 2], j <- [fj - 1 .. fj + 2], k <- [0 .. 5] ])

-- 裏返し（y → −y）してから 60° × r 回す
rotRef :: Int -> Bool -> Point -> Point
rotRef r ref (x, y0) =
  let y = if ref then negate y0 else y0
      a = fromIntegral r * pi / 3
  in (x * cos a - y * sin a, x * sin a + y * cos a)

insidePoly :: Point -> [Point] -> Bool
insidePoly (px, py) poly = odd (length (filter crosses (zip poly (drop 1 poly ++ take 1 poly))))
  where
    crosses ((ax, ay), (bx, by)) = (ay > py) /= (by > py) && px < (bx - ax) * (py - ay) / (by - ay) + ax

-- 帽子に含まれる 8 枚のカイト
hatKites :: [Kite]
hatKites = [ (i, j, k) | i <- [-3 .. 3], j <- [-3 .. 3], k <- [0 .. 5], insidePoly (kiteCenter (i, j, k)) hat ]

-- 12 通りの向き（回転 6 × 裏返し 2）
data Orient = Orient { oRef :: !Bool, oRot :: !Int, oKs :: [Kite] }

orients :: V.Vector Orient
orients = V.fromList
  [ Orient ref r [ locateKite (rotRef r ref (kiteCenter kt)) | kt <- hatKites ]
  | ref <- [False, True], r <- [0 .. 5] ]

orientAt :: Int -> Orient
orientAt i = orients V.! (i `mod` 12)

-- 置いた帽子の点：向きを変えてから、格子の分だけずらす
placeV :: Place -> Point -> Point
placeV p v =
  let o = orientAt (pO p)
      (x, y) = rotRef (oRot o) (oRef o) v
      (ax, ay) = h1
      (bx, by) = h2
  in (x + fromIntegral (pDi p) * ax + fromIntegral (pDj p) * bx, y + fromIntegral (pDi p) * ay + fromIntegral (pDj p) * by)

hatOutline :: [Point]
hatOutline = hat

-- 帽子は凹んだ形なので、凸の部分に分けて塗る
hatParts :: [[Point]]
hatParts = convexParts hat

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs g0 = go (length xs) xs g0 []
  where
    go _ [] g acc = (acc, g)
    go n l g acc =
      let (i, g1) = randomR (0, n - 1) g
      in case splitAt i l of
           (a, x : b) -> go (n - 1) (a ++ b) g1 (x : acc)
           (a, []) -> go (n - 1) (drop 1 a) g1 acc

-- 左上：帽子 1 枚と、その中の 8 枚のカイト
drawSingleHat :: Float -> Float -> Float -> Picture
drawSingleHat cx cy z =
  let toPx (x, y) = (cx + x * z, cy + y * z)
  in Pictures
    [ Color (rgb255 250 200 120) (Pictures [ Polygon (map toPx part) | part <- hatParts ])
    , Color (gray 0.16) (thickPath 3 (closed (map toPx hat)))
    , Color (rgb255 120 100 80) (Pictures [ Line (closed (map toPx (kitePoly kt))) | kt <- hatKites ]) ]

-- gloss の Polygon は凸の形しか正しく塗れないので、
-- 三角形に切って（耳切り法）、くっつけられるものをくっつけて凸の部分に分ける
convexParts :: [Point] -> [[Point]]
convexParts pts0 = foldl' addTri [] (earClip [] (ccw pts0))

ccw :: [Point] -> [Point]
ccw ps = if sum [ ax * by - bx * ay | ((ax, ay), (bx, by)) <- cyc ps ] < 0 then reverse ps else ps

cyc :: [a] -> [(a, a)]
cyc p = zip p (drop 1 p ++ take 1 p)

cross3p :: Point -> Point -> Point -> Float
cross3p (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - by) - (by - ay) * (cx - bx)

earClip :: [[Point]] -> [Point] -> [[Point]]
earClip acc ps = case ps of
  (_ : _ : _ : _ : _) ->
    let n = length ps
        turned i = drop i ps ++ take i ps
        isEar (a : b : c : rest) = cross3p a b c > 1e-4 && not (any (inTri a b c) rest)
        isEar _ = False
        ears = filter isEar (map turned [0 .. n - 1])
    in case (ears, ps) of
         ((a : b : c : rest) : _, _) -> earClip ([a, b, c] : acc) (a : c : rest)
         (_, a : b : c : rest) -> earClip ([a, b, c] : acc) (a : c : rest)
         _ -> acc
  [_, _, _] -> ps : acc
  _ -> acc

inTri :: Point -> Point -> Point -> Point -> Bool
inTri a b c p = cross3p a b p >= 0 && cross3p b c p >= 0 && cross3p c a p >= 0

addTri :: [[Point]] -> [Point] -> [[Point]]
addTri polys tri = go polys
  where
    go [] = [tri]
    go (q : qs) = case mergeTri q tri of
      Just m -> m : qs
      Nothing -> q : go qs

mergeTri :: [Point] -> [Point] -> Maybe [Point]
mergeTri poly tri = case tri of
  [x, y, z] -> firstJust [try b a c | (a, b, c) <- [(x, y, z), (y, z, x), (z, x, y)]]
  _ -> Nothing
  where
    -- tri の辺 a→b が、poly の辺 b→a と同じなら、頂点 c をはさむ
    try a b c = firstJust
      [ if isConvex m then Just m else Nothing
      | (i, (p, q)) <- zip [1 ..] (cyc poly), p == a, q == b
      , let m = drop i poly ++ take i poly ++ [c] ]
    isConvex m = all (\(a, b, c) -> cross3p a b c >= -1e-3) (zip3 m (drop 1 m ++ take 1 m) (drop 2 m ++ take 2 m))
    firstJust xs = case [ v | Just v <- xs ] of
      (v : _) -> Just v
      [] -> Nothing
