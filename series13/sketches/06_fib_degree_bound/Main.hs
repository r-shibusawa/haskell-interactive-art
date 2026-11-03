-- ============================================
-- 06 なぜ「フィボナッチ」？ ─ 次数の上限
-- 連鎖的な切り取りのおかげで、次数 k の木はどんなにやせても F(k+2) 個以上の節点を持つ
-- F(k+2) ≥ φ^k なので、n 個の節点なら次数は log_φ n 以下。EXTRACT-MIN が O(log n) になる理由
-- 左：印の規則ありの一番やせた木 / 右：規則なし（子を何人でも失える）の一番やせた木
-- ←→ で次数 k / スペースで自動 / H でヒープ比較表の表示切りかえ
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', mapAccumL)
import qualified Data.Map.Strict as M

startK :: Int
startK = 4            -- ★ 次数

kMax :: Int
kMax = 9

phi :: Float
phi = (1 + sqrt 5) / 2

-- 節点は文字列の名前を持つ（親の名前 + 1 文字。新しい節点が親の位置から生えてくるように）
data T = T { tId :: !String, tKids :: ![T] }

data World = World
  { wK     :: !Int
  , wAuto  :: !Bool
  , wTimer :: !Float
  , wPos   :: !(M.Map String Point)
  , wTable :: !Bool
  }

main :: IO ()
main = runSketch (sketch "13-06 Why Fibonacci" (World startK False 0 M.empty True))
  { skBack  = hsb 45 25 9
  , skStep  = \env dt w -> pure (stepWorld env dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 一番やせた次数 k の木：子の次数は 0, 0, 1, 2, …, k−2（i 番目の子は次数 ≥ i−2）
minimal :: Int -> String -> T
minimal k i =
  T i ([minimal 0 (i ++ "a") | k >= 1] ++ [minimal c (i ++ [toEnum (98 + c)]) | c <- [0 .. k - 2]])

-- 規則なしの一番やせた木：子はみんな葉
star :: Int -> String -> T
star k i = T i [T (i ++ show c) [] | c <- [0 .. k - 1]]

treeSize, leaves, depthOf :: T -> Int
treeSize n = 1 + sum (map treeSize (tKids n))
leaves n = if null (tKids n) then 1 else sum (map leaves (tKids n))
depthOf n = if null (tKids n) then 0 else 1 + foldl' max 0 (map depthOf (tKids n))

fib :: Int -> Int
fib n = fst (foldl' (\(a, b) _ -> (b, a + b)) (0, 1) [1 .. n])

-- 配置：((名前, (位置, 深さ)) の列, 辺の列, 1 葉あたりの幅)
place :: T -> Float -> Float -> Float -> Float -> ([(String, (Point, Int))], [(String, String)], Float)
place root x0 x1 y0 y1 = (snd res, edgesOf root, u)
  where
    u = min 40 ((x1 - x0) / fromIntegral (leaves root))
    lh = min 70 ((y1 - y0) / fromIntegral (max 1 (depthOf root)))
    left = (x0 + x1) / 2 - fromIntegral (leaves root) * u / 2
    res = go root left 0
    go n l d
      | null (tKids n) = emit n (l + u / 2) d []
      | otherwise =
          let (_, sub) = mapAccumL (\a c -> (a + fromIntegral (leaves c) * u, go c a (d + 1))) l (tKids n)
          in emit n (midOf l (map fst sub)) d (concatMap snd sub)
    emit n cx d sub = (cx, sub ++ [(tId n, ((cx, y0 + fromIntegral (d :: Int) * lh), d))])
    edgesOf n = [(tId n, tId c) | c <- tKids n] ++ concatMap edgesOf (tKids n)

-- 木の配置に使う列
geom :: Env -> World -> (Float, Float, Float)
geom env w =
  let tableW = if wTable w then 400 else 0
  in (tableW, (envW env - tableW) * 0.62, envH env * 0.58)

trees :: Env -> World -> (([(String, (Point, Int))], [(String, String)], Float), ([(String, (Point, Int))], [(String, String)], Float))
trees env w =
  let (tableW, midX, y1) = geom env w
      a = place (minimal (wK w) "S") 30 (midX - 20) 170 y1
      b = place (star (wK w) "T") (midX + 20) (envW env - tableW - 30) 170 y1
  in (a, b)

stepWorld :: Env -> Float -> World -> World
stepWorld env dt w0 =
  let w1 | wAuto w0 =
             let t = wTimer w0 + dt
             in if t > 1.4 then w0 { wTimer = 0, wK = (wK w0 + 1) `mod` (kMax + 1) } else w0 { wTimer = t }
         | otherwise = w0
      ((la, _, _), (lb, _, _)) = trees env w1
      tg = M.fromList (la ++ lb)
      old = wPos w1
      startOf i t = case M.lookup i old of
        Just p -> p
        Nothing -> M.findWithDefault t (take (length i - 1) i) old
  in w1 { wPos = M.mapWithKey (\i (t, _) -> lerpP 0.12 (startOf i t) t) tg }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (treePics ++ headings ++ chart ++ intro ++ table ++ hint)
  where
    k = wK w
    h = envH env
    (tableW, midX, _) = geom env w
    ((la, ea, u1), (lb, eb, u2)) = trees env w
    posOf i = M.findWithDefault (0, 0) i (wPos w)
    treePics =
      [ lineP env 1.5 (hsb 45 20 50) (posOf a) (posOf b) | (a, b) <- ea ++ eb ] ++
      [ discP env (hsb (45 + fromIntegral d * 25) 70 95) (posOf i) (clampF 6 22 (u1 * 0.6) / 2) | (i, (_, d)) <- la ] ++
      [ discP env (hsb 200 40 70) (posOf i) (clampF 6 22 (u2 * 0.6) / 2) | (i, _) <- lb ]
    headings =
      [ txtC env ((30 + midX) / 2) 130 15 (hsb 45 60 100)
          ("With marks: thinnest degree " ++ show k ++ " = " ++ show (treeSize (minimal k "S")) ++ " = F(" ++ show (k + 2) ++ ")")
      , txtC env ((midX + envW env - tableW) / 2) 130 15 (hsb 200 40 90)
          ("No rule: only " ++ show (k + 1) ++ " nodes")
      ]
    -- 棒グラフ：F(k+2) と k+1 と φ^k
    gx = 60
    gy = h - 80
    gw = envW env - tableW - 120
    gh = h * 0.26
    maxV = fromIntegral (fib (kMax + 2)) :: Float
    bw = gw / fromIntegral (kMax + 1)
    chart =
      lineP env 1 (hsb 45 15 40) (gx, gy) (gx + gw, gy) : concat
      [ [ fillBox env (if c == k then hsb 45 90 100 else hsb 45 60 60) (x + 4) (gy - h1) (bw * 0.45) h1
        , fillBox env (if c == k then hsb 200 60 100 else hsb 200 40 55) (x + 4 + bw * 0.47) (gy - h2) (bw * 0.3) h2
        , txtC env (x + bw * 0.4) (gy + 14) 14 (hsb 0 0 80) (show c)
        , txtC env (x + 4 + bw * 0.22) (gy - h1 - 12) 14 (hsb 45 60 95) (show f) ]
      | c <- [0 .. kMax]
      , let f = fib (c + 2)
            h1 = fromIntegral f / maxV * gh
            h2 = fromIntegral (c + 1) / maxV * gh
            x = gx + fromIntegral c * bw ]
      ++ [ pathP env 2 (hsba 0 0 100 0.7)
             [ (gx + t * bw + bw * 0.25, gy - phi ** t / maxV * gh) | i <- [0 .. 90 :: Int], let t = fromIntegral i * 0.1 ]
         , txt env gx (gy - gh - 30) 14 (hsb 0 0 85) "Yellow = F(k+2) (with marks)   Blue = k+1 (no rule)   White line = phi^k"
         ]
    million = floor (log 1e6 / log phi) :: Int
    intro =
      [ txt env 24 18 26 (hsb 0 0 100) "Degree bound and Fibonacci numbers"
      , txt env 24 56 14 (hsb 0 0 80)
          ("s_k = 2 + s_0 + ... + s_(k-2) = F(k+2) >= phi^k, so degree <= log_phi n (n = 1 million: degree <= " ++ show million ++ ")")
      , txt env 24 80 14 (hsb 0 0 80)
          "No rule: a degree k tree has only k+1 nodes, big-degree roots pile up, and EXTRACT-MIN gets slow"
      ]
    rows =
      [ ["Operation", "Bin", "Binom", "Fib*"], ["MAKE-HEAP", "1", "1", "1"], ["INSERT", "lg n", "lg n", "1"]
      , ["MINIMUM", "1", "lg n", "1"], ["EXTRACT-MIN", "lg n", "lg n", "lg n"], ["UNION", "n", "lg n", "1"]
      , ["DECREASE-KEY", "lg n", "lg n", "1"], ["DELETE", "lg n", "lg n", "lg n"] ]
    table
      | not (wTable w) = []
      | otherwise =
          let tx = envW env - 390
              ty = 110
          in [ fillBox env (hsb 45 15 15) (tx - 10) (ty - 10) 385 420
             , txt env tx ty 16 (hsb 0 0 100) "Heaps: worst / *amortized" ] ++
             [ txt env (tx + off) (ty + 34 + fromIntegral j * 32) 14
                 (if j == 0 then hsb 45 50 90 else if i == (3 :: Int) then hsb 45 70 100 else hsb 0 0 85) c
             | (j, r) <- zip [0 :: Int ..] rows, (i, c, off) <- zip3 [0 ..] r [0, 175, 240, 310] ] ++
             [ txt env tx (ty + 300 + fromIntegral j * 22) 14 (hsb 0 0 70) s
             | (j, s) <- zip [0 :: Int ..]
                 [ "* amortized; all are Theta or O"
                 , "Dijkstra with a binary heap:"
                 , "O((V+E) lg V)"
                 , "with a Fibonacci heap:"
                 , "O(V lg V + E)" ] ]
    hint = [ txt env 24 (h - 30) 14 (hsb 0 0 60) "Left, Right = degree k / Space = auto / H = comparison table" ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wK = min kMax (wK w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wK = max 0 (wK w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _ | c `elem` "hH" -> w { wTable = not (wTable w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. k を 1 ずつ増やして、左の木の節点の数を書き出そう。1, 2, 3, 5, 8 ... になっている？
-- 2. minimal の [0 .. k - 2] を [0 .. k - 1] に変えると、どんな数列になる？
-- 3. 比較表を見て、ダイクストラ法でフィボナッチヒープが速くなる理由を考えよう

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左上が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx (gy - size) size c s

-- 中央揃えの文字（中心が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 太さのある線分
lineP :: Env -> Float -> Color -> Point -> Point -> Picture
lineP env w c (x1, y1) (x2, y2) = Color c (thickLine w (toG env x1 y1) (toG env x2 y2))

-- 太さのある折れ線
pathP :: Env -> Float -> Color -> [Point] -> Picture
pathP env w c ps = Color c (thickPath w [toG env x y | (x, y) <- ps])

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 2 点の間を t の割合で進む（p5.js の lerp を x, y に）
lerpP :: Float -> Point -> Point -> Point
lerpP t (a, b) (c, d) = (lerpF a c t, lerpF b d t)

-- 子の中心の真ん中（子がなければ d）
midOf :: Float -> [Float] -> Float
midOf d [] = d
midOf _ xs@(x : _) = (x + last xs) / 2
