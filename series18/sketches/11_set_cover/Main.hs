-- ============================================
-- 11 集合被覆の貪欲法
-- 点（町）を全部カバーするのに、四角（基地局の範囲）を最少で何個選べばいい？
-- 貪欲法：「まだカバーされていない点をいちばん多く含む四角」を選び続ける
-- 最適の H(最大の四角の点数)（約 ln n）倍以内が保証される。これより良い比は（P≠NP なら）多項式時間では無理
-- スペース：貪欲法を 1 手ずつ / O：最適解（総当たり）/ 四角をクリック：自分で選ぶ / R：ランダム / 2：貪欲法が負ける例
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, setBit, testBit, (.|.))
import qualified Data.IntSet as IS
import Data.List (elemIndex, find, sortOn)
import Numeric (showFFloat)

nSets :: Int
nSets = 12          -- ★ 四角の数（最適の総当たりは 2^nSets）

data Dot = Dot { dX :: !Float, dY :: !Float, dC :: !Int, dR :: !Int }

data SetBox = SetBox
  { bX :: !Float, bY :: !Float, bW :: !Float, bH :: !Float
  , bMembers :: [Int]
  , bHue :: !Float
  }

data World = World
  { wPts     :: [Dot]
  , wSets    :: [SetBox]
  , wChosen  :: [Int]
  , wManual  :: [Int]
  , wOpt     :: Maybe [Int]
  , wShowOpt :: !Bool
  }

main :: IO ()
main = runSketch (sketch "18-11 Set Cover Greedy" (World [] [] [] [] Nothing False))
  { skBack  = hsb 150 25 9
  , skStart = randomInstance
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- 格子のマスの中心と大きさ
cellXY :: Env -> Int -> Int -> Int -> Int -> (Float, Float, Float)
cellXY env gw gh c r =
  let s = min ((envW env - 200) / fromIntegral gw) ((envH env - 260) / fromIntegral gh)
  in (100 + fromIntegral c * s + s / 2, 170 + fromIntegral r * s + s / 2, s)

makePts :: Env -> Int -> Int -> [Dot]
makePts env gw gh =
  [ Dot x y c r | r <- [0 .. gh - 1], c <- [0 .. gw - 1], let (x, y, _) = cellXY env gw gh c r ]

mkSet :: Env -> Int -> Int -> [Dot] -> Float -> (Int, Int, Int, Int) -> SetBox
mkSet env gw gh pts hue (c0, r0, c1, r1) =
  let (ax, ay, s) = cellXY env gw gh c0 r0
      (bx, by, _) = cellXY env gw gh c1 r1
      members = [i | (i, p) <- zip [0 ..] pts, dC p >= c0, dC p <= c1, dR p >= r0, dR p <= r1]
  in SetBox (ax - s * 0.45) (ay - s * 0.45) (bx - ax + s * 0.9) (by - ay + s * 0.9) members hue

randomInstance :: Env -> IO World
randomInstance env = do
  let gw = 14
      gh = 8
      pts = makePts env gw gh
      -- まず格子を 5 つの四角に切り分ける（これで必ず全部カバーできる）
      split ps
        | length ps >= 5 = pure ps
        | otherwise = case sortOn (\(a, b, c, d) -> negate ((c - a + 1) * (d - b + 1))) ps of
            [] -> pure ps
            ((c0, r0, c1, r1) : rest)
              | c1 - c0 >= r1 - r0 -> do
                  m <- randomRIO (c0, max c0 (c1 - 1))
                  split (rest ++ [(c0, r0, m, r1), (m + 1, r0, c1, r1)])
              | otherwise -> do
                  m <- randomRIO (r0, max r0 (r1 - 1))
                  split (rest ++ [(c0, r0, c1, m), (c0, m + 1, c1, r1)])
      -- 残りはおとり（大きめの四角をばらまく）
      decoy _ = do
        w <- randomRIO (3, 7)
        h <- randomRIO (2, 5)
        c0 <- randomRIO (0, gw - w)
        r0 <- randomRIO (0, gh - h)
        pure (c0, r0, c0 + w - 1, r0 + h - 1)
  pieces <- split [(0, 0, gw - 1, gh - 1)]
  decoys <- mapM decoy [length pieces + 1 .. nSets]
  mixed <- shuffle (pieces ++ decoys)
  let sets = [mkSet env gw gh pts (fromIntegral ((k * 47) `mod` 360)) q | (k, q) <- zip [0 :: Int ..] mixed]
  pure (World pts sets [] [] Nothing False)

-- 2 行。上の行・下の行の 2 つで足りるのに、貪欲法は大きなブロックにつられる
badInstance :: Env -> World
badInstance env =
  let gw = 14
      gh = 2
      pts = makePts env gw gh
      qs = [(0, 0, 13, 0), (0, 1, 13, 1), (0, 0, 1, 1), (2, 0, 5, 1), (6, 0, 13, 1)]  -- 最適の 2 個と、4, 8, 16 個のブロック
      sets = [mkSet env gw gh pts (fromIntegral ((k * 47) `mod` 360)) q | (k, q) <- zip [0 :: Int ..] qs]
  in World pts sets [] [] Nothing False

setAt :: World -> Int -> Maybe SetBox
setAt w k = case drop k (wSets w) of
  (s : _) -> Just s
  [] -> Nothing

uncoveredBy :: World -> [Int] -> IS.IntSet
uncoveredBy w list =
  let covered = IS.fromList (concat [bMembers s | k <- list, Just s <- [setAt w k]])
  in IS.fromList [i | i <- [0 .. length (wPts w) - 1], not (IS.member i covered)]

greedyStep :: World -> World
greedyStep w =
  let un = uncoveredBy w (wChosen w)
      gain s = length (filter (`IS.member` un) (bMembers s))
      pick (bk, bg) (k, s) = let g = gain s in if g > bg then (k, g) else (bk, bg)
      (bestK, _) = foldl' pick (-1, 0) (zip [0 ..] (wSets w))
  in if IS.null un || bestK < 0 then w else w { wChosen = wChosen w ++ [bestK] }

-- 全部の組み合わせを調べる（2^四角の数 通り）。点の集合は Integer のビット列
computeOpt :: World -> Maybe [Int]
computeOpt w =
  let m = length (wSets w)
      wordOf s = foldl' setBit (0 :: Integer) (bMembers s)
      wordsAll = zip [0 ..] (map wordOf (wSets w))
      full = foldl' setBit (0 :: Integer) [0 .. length (wPts w) - 1]
      test best mask =
        let cnt = popCount mask
        in case best of
             Just b | cnt >= popCount b -> best
             _ ->
               let u = foldl' (.|.) 0 [x | (k, x) <- wordsAll, testBit mask k]
               in if u == full then Just mask else best
      found = foldl' test Nothing [1 .. (2 ^ m) - 1 :: Int]
  in fmap (\mask -> [k | k <- [0 .. m - 1], testBit mask k]) found

harmonic :: Int -> Float
harmonic n = sum [1 / fromIntegral i | i <- [1 .. n]]

drawW :: Env -> World -> Picture
drawW env w =
  let active = if null (wManual w) then wChosen w else wManual w
      un = uncoveredBy w active
      outline env' c t x y bw bh =
        Color c (thickPath t (map (pt env') [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh), (x, y)]))
      setPic (k, s) =
        let on = k `elem` active
            o = wShowOpt w && maybe False (k `elem`) (wOpt w)
            x = bX s
            y = bY s
            bw = bW s
            bh = bH s
            corners = [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh), (x, y)]
            edge
              | o = Pictures (zipWith (dashed env (hsb 0 0 100) 3 10 6) corners (drop 1 corners))
              | otherwise = outline env (hsba (bHue s) 70 100 (if on then 1 else 0.4)) (if on then 4 else 1.5) x y bw bh
            order = case elemIndex k active of
              Just i | on -> txt env (x + 6) (y + 18) 16 (hsb (bHue s) 60 100) ("#" ++ show (i + 1))
              _ -> Blank
        in Pictures [box env (hsba (bHue s) 70 90 (if on then 0.28 else 0.06)) x y bw bh, edge, order]
      ptPic (i, p) =
        let u = IS.member i un
        in dot env (if u then hsb 0 0 100 else hsb 130 70 100) (dX p, dY p) (if u then 10 else 8)
      maxS = foldl' max 0 (map (length . bMembers) (wSets w))
      total = length (wPts w)
      optLine = case wOpt w of
        Just o ->
          let c = length o
          in txt env 20 102 18 (hsb 0 0 100)
               ("Optimum " ++ show c ++ " (dashed)   Guarantee: greedy <= H(" ++ show maxS ++ ") x opt = "
                 ++ fix 2 (harmonic maxS) ++ " x " ++ show c ++ " = " ++ fix 1 (harmonic maxS * fromIntegral c))
        Nothing -> Blank
  in Pictures
       ( map setPic (zip [0 :: Int ..] (wSets w))
      ++ map ptPic (zip [0 ..] (wPts w))
      ++ [ txt env 20 34 15 (hsb 0 0 100) "Space: 1 greedy step   O: optimum (brute force)   Click a box: choose   R: random   2: greedy loses"
         , txt env 20 70 18 (hsb 45 85 100)
             ((if null (wManual w) then "Greedy: " ++ show (length (wChosen w)) else "Your choice: " ++ show (length (wManual w)))
               ++ " chosen   uncovered " ++ show (IS.size un) ++ " / " ++ show total)
         , optLine
         , txt env 20 (envH env - 22) 15 (hsb 0 0 60) "H(d) = 1 + 1/2 + ... + 1/d, about ln d. Proof: charge each point 1/(number newly covered)."
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = mouseP5 env p
        inside (k, s) =
          mx > bX s && mx < bX s + bW s && my > bY s && my < bY s + bH s && k `notElem` wManual w
    in pure $ if my < 120
         then w
         else case find inside (reverse (zip [0 ..] (wSets w))) of
           Just (k, _) -> w { wManual = wManual w ++ [k] }
           Nothing -> w { wManual = [] }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let w1 = w { wManual = [] }
    in pure $ if IS.null (uncoveredBy w1 (wChosen w1)) then w1 { wChosen = [] } else greedyStep w1
  EventKey (Char c) Down _ _
    | c `elem` "oO" ->
        let w1 = case wOpt w of
              Nothing -> w { wOpt = computeOpt w }
              Just _ -> w
        in pure w1 { wShowOpt = not (wShowOpt w1) }
    | c `elem` "rR1" -> randomInstance env
    | c == '2' -> pure (badInstance env)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 2 の例で、貪欲法は最初に何を選ぶ？ なぜ失敗する？
-- 2. 自分でクリックして、貪欲法より少ない個数で全部カバーできる？
-- 3. 2 の例のブロックを 4・8・16・32 個…と増やすと、貪欲法の個数は log に比例して増えていく

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 破線（線の長さ on、すき間 off）
dashed :: Env -> Color -> Float -> Float -> Float -> Point -> Point -> Picture
dashed env c t on off a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      p d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      starts = takeWhile (< l) (iterate (+ (on + off)) 0)
  in if l < 1 then Blank else Pictures [seg env c t (p d) (p (min l (d + on))) | d <- starts]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- ---------- 道具：その他 ----------
-- リストをランダムに並べかえる（p5.js の shuffle）
shuffle :: [a] -> IO [a]
shuffle xs = do
  ks <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortOn fst (zip ks xs)))

-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""
