-- ============================================
-- 42-11 表現可能性：ファノ・マトロイドと非ファノ
-- 7 点のマトロイド。F7（ファノ）は 7 本の「直線」（3 点組）を持ち、そのうち 1 本は図では円で描かれる
--   F7 は GF(2) 上で表現できる（点 = GF(2)^3 のベクトル）が、実数 R 上ではできない（3 つの中点は一直線に乗らない）
--   非ファノ F7-（円の直線をなくしたもの）は逆に R で表現できて GF(2) ではできない。体によって「作れる構造」がちがう
-- 操作：点をクリック = 選ぶ（階数を両方の体で計算） / F = ファノ ⇄ 非ファノ / 三角形の頂点をドラッグ / C = 選択を消す
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, (.&.))
import Data.Bits (xor)
import Data.List (foldl', intercalate, maximumBy, subsequences)

-- GF(2)^3 のベクトル（ビット：A = 100, B = 010, C = 001）
ptBits :: [Int]
ptBits = [4, 2, 1, 6, 3, 5, 7]

ptNames :: String
ptNames = "ABCDEFG"

-- 直線：GF(2) で a + b = c になる 3 点組（= 階数 2）。ファノは 7 本、非ファノは円 {D, E, F} を除く 6 本
allLines :: [[Int]]
allLines = [[0, 1, 3], [1, 2, 4], [0, 2, 5], [0, 4, 6], [1, 5, 6], [2, 3, 6], [3, 4, 5]]

data World = World
  { wFano   :: !Bool          -- ★ True = F7、False = 非ファノ F7-
  , wCorner :: ![(Double, Double)]   -- 三角形の頂点（階数の計算のため Double）
  , wOn     :: ![Bool]        -- 7 点の選択
  , wDrag   :: !Int
  , wMoved  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "42-11 Fano and non-Fano" world0)
  { skBack  = hsb 60 25 9
  , skStart = \env -> pure (setup env)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World True [] (replicate 7 False) (-1) False

setup :: Env -> World
setup env =
  let cx = realToFrac (envW env * 0.3) :: Double
      cy = realToFrac (envH env * 0.58)
      r = realToFrac (min (envW env) (envH env) * 0.3)
      a i = 2 * pi * fromIntegral i / 3 - pi / 2 :: Double
  in world0 { wCorner = [ (cx + r * cos (a i), cy + r * sin (a i)) | i <- [0 .. 2 :: Int] ] }

-- ★ 図の座標：頂点 A, B, C、中点 D = AB, E = BC, F = AC、重心 G
layoutPoints :: World -> [(Double, Double)]
layoutPoints w = case wCorner w of
  [a@(ax, ay), b@(bx, by), c@(cx, cy)] ->
    let mid (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
    in [a, b, c, mid a b, mid b c, mid a c, ((ax + bx + cx) / 3, (ay + by + cy) / 3)]
  _ -> replicate 7 (0, 0)

activeLines :: World -> [[Int]]
activeLines w = if wFano w then allLines else take 6 allLines

-- ★ マトロイドでの独立：3 点以下で、直線をまるごと含まない
indepIn :: World -> [Int] -> Bool
indepIn w sub = length sub <= 3 && not (any (all (`elem` sub)) (activeLines w))

rankR :: Env -> World -> [Int] -> Int
rankR env w sub =
  let pts = layoutPoints w
      ox = realToFrac (envW env * 0.3)
      oy = realToFrac (envH env * 0.58)
      row i = let (x, y) = pick (0, 0) pts i in [(x - ox) / 100, (y - oy) / 100, 1]
  in rankReal (map row sub)

rank2 :: [Int] -> Int
rank2 = rankGF2 . map (pick 0 ptBits)

-- 定義から：独立な部分集合の最大の大きさ
matroidRank :: World -> [Int] -> Int
matroidRank w sub = foldl' max 0 [ length s | s <- subsequences sub, indepIn w s ]

toF :: (Double, Double) -> Point
toF (x, y) = (realToFrac x, realToFrac y)

draw :: Env -> World -> Picture
draw env w =
  let pts = map toF (layoutPoints w)
      pt = pick (0, 0) pts
      fano = wFano w
      sel = [ i | (i, True) <- zip [0 ..] (wOn w) ]
      -- 直線を描く
      linePic k l =
        let on = k < (6 :: Int) || fano
            hl = length sel >= 2 && all (`elem` sel) l
            c = hsb (if hl then 50 else 200) (if hl then 90 else 40) (if on then (if hl then 100 else 70) else 35)
            wgt | hl = 5
                | on = 3
                | otherwise = 1.5
            g = pt 6
            r = dist2 g (pt 3)
        in if k == 6
             then (if on then Color c (at (sp env g) (ring r wgt)) else dashCircle env r wgt c g)   -- 中点を通る円
             else seg env wgt c (pt (pick 0 l 0)) (pt (pick 0 l 1))   -- 直線の両端は最初の 2 点
      -- 点
      ptPic i p@(x, y) =
        let on = pick False (wOn w) i
            bits = pick 0 ptBits i
            vec = "(" ++ concatMap (\s -> show ((bits `shiftR` s) .&. 1)) [2, 1, 0] ++ ")"
        in Pictures
             [ ball env (if on then 15 else 11) (if on then hsb 50 90 100 else hsb 60 40 80) p
             , txtC env x y 14 (gray 0.1) [pick '?' ptNames i]
             , txtC env (x + (if i < 3 then 0 else 32)) (y + (if i < 3 then -26 else 20)) 14 (gray 0.8) vec ]
      -- 右：表
      px = envW env - 470
      py = 120
      triples = [ [a, b, c] | a <- [0 .. 6], b <- [a + 1 .. 6], c <- [b + 1 .. 6] ]
      want s = if indepIn w s then 3 else 2
      ok2 = all (\s -> rank2 s == want s) triples
      okR = all (\s -> rankR env w s == want s) triples
      -- 全部の 3 点組を検査して、体ごとに「マトロイドと一致するか」
      rowPic k l =
        let isLine = k < (6 :: Int) || fano
            r2 = rank2 l
            rR = rankR env w l
            wnt = if isLine then 2 else 3
            y = py + 62 + fromIntegral k * 24
            mark r = show r ++ (if r == wnt then " [ok]" else " [x]")
            okC r = hsb (if r == wnt then 130 else 5) 80 100
        in Pictures
             [ txt env px y 15 (gray 0.9) (unwords (map (\i -> [pick '?' ptNames i]) l))
             , txt env (px + 80) y 15 (if isLine then hsb 50 70 90 else gray 0.9) (if isLine then "line (dep)" else "indep")
             , txt env (px + 220) y 15 (okC r2) (mark r2)
             , txt env (px + 330) y 15 (okC rR) (mark rR) ]
      y2 = py + 62 + 7 * 24 + 4
      nameList = intercalate ", " . map (\i -> [pick '?' ptNames i])
      mr = matroidRank w sel
      r2s = rank2 sel
      rRs = rankR env w sel
      selRows
        | null sel = [ (15, white, "Choose 3 points") ]
        | otherwise =
            [ (15, white, "Chosen {" ++ nameList sel ++ "}")
            , (15, white, "Matroid rank " ++ show mr ++ (if indepIn w sel then " (independent)" else " (dependent)"))
            , (14, gray 0.8, "GF(2) rank " ++ show r2s ++ (if r2s == mr then " [ok]" else " [x]")
                             ++ "   R rank " ++ show rRs ++ (if rRs == mr then " [ok]" else " [x]")) ]
      explain
        | fano = [ "GF(2): D + E = (110) + (011)"
                 , "= (101) = F, so D, E, F are dependent."
                 , "In R, the midpoints are never collinear,"
                 , "however you move it."
                 , "F7 needs characteristic 2." ]
        | otherwise = [ "R: as drawn (moving corners is fine)."
                      , "GF(2) forces D, E, F to be dependent,"
                      , "so they cannot be independent."
                      , "F7- needs characteristic not 2." ]
  in Pictures $
       zipWith linePic [0 ..] allLines ++ zipWith ptPic [0 ..] pts ++
       [ txt env 20 40 16 white ("Click point: select  F: " ++ (if fano then "Fano F7" else "non-Fano F7-") ++ " (toggle)  Drag triangle corner  C: clear")
       , txt env 20 66 14 (gray 0.6) "( ) = vector in GF(2)^3. Line = 3 dependent points (rank 2). In Fano the circle D, E, F is a line too."
       , box env (withA 0.08 white) (px - 20) (py - 40) 480 (envH env - py + 20)
       , txt env px py 20 (hsb 50 80 100) (if fano then "Fano matroid F7 (7 lines)" else "Non-Fano F7- (6 lines)")
       , txt env px (py + 36) 15 white "Triple"
       , txt env (px + 80) (py + 36) 15 white "Matroid"
       , txt env (px + 220) (py + 36) 15 white "GF(2)"
       , txt env (px + 330) (py + 36) 15 white "R (figure)" ] ++
       zipWith rowPic [0 ..] allLines ++
       [ stack env px y2
           ([ (14, gray 0.6, "Other triples: independent (35 checked)")
            , (17, hsb (if ok2 then 130 else 5) 80 100, "GF(2): " ++ (if ok2 then "representable" else "not representable"))
            , (17, hsb (if okR then 130 else 5) 80 100, "R: " ++ (if okR then "representable" else "not representable")) ]
            ++ map (\s -> (14, gray 0.7, s)) explain ++ [ (6, white, "") ] ++ selRows)
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hits = [ i | (i, c) <- zip [0 ..] (wCorner w), dist2 m (toF c) < 18 ]
    in w { wDrag = foldl' (\_ i -> i) (-1) hits, wMoved = False }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        c = ( realToFrac (clampF 20 (envW env - 500) mx), realToFrac (clampF 90 (envH env - 20) my) )
    in w { wCorner = setAt (wDrag w) (const c) (wCorner w), wMoved = True }
  EventKey (MouseButton LeftButton) Up _ _ ->
    let m = mouseP env
        hits = [ i | (i, p) <- zip [0 ..] (layoutPoints w), dist2 m (toF p) < 18 ]
        w1 = w { wDrag = -1 }
    in case hits of
         (i : _) | not (wMoved w) -> w1 { wOn = setAt i not (wOn w) }
         _ -> w1
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> w { wFano = not (wFano w) }
    | c `elem` "cC" -> w { wOn = replicate 7 False }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ファノで D, E, F を選ぶ：GF(2) では階数 2、R では 3。これが「R で表現できない」理由そのもの
-- 2. 非ファノで頂点をドラッグして三角形をつぶしてみる。どこまでなら R の表に [ok] が並ぶ？（3 点が一直線に乗ると壊れる）
-- 3. 4 点を選ぶと必ず従属（階数 3 のマトロイド）。基は何個ある？（独立な 3 点組の数 = 35 − 直線の数）
-- 4. GF(3)（0, 1, 2 の世界）ではどちらが表現できる？ rankGF2 を mod 3 の消去法に書きかえて試す

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点の座標で）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 文字（中央を指定）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (a, b) = sp env (x, y) in labelC a b size c s

-- 上から順に文字を並べる：(大きさ, 色, 文字)。空の文字は行間だけあける
stack :: Env -> Float -> Float -> [(Float, Color, String)] -> Picture
stack env x y0 rows = Pictures (go y0 rows)
  where go _ [] = []
        go y ((sz, c, s) : r) = txt env x y sz c s : go (y + sz * 1.5) r

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 破線の円（半径 r、太さ w）
dashCircle :: Env -> Float -> Float -> Color -> Point -> Picture
dashCircle env r w c (x, y) =
  let n = max 6 (floor (tau * r / 16)) :: Int
      pt a = (x + r * cos a, y + r * sin a)
      da = tau / fromIntegral n
  in Pictures [ seg env w c (pt a) (pt (a + da / 2)) | k <- [0 .. n - 1], let a = fromIntegral k * da ]

-- 白いふちつきの丸（半径 r）
ball :: Env -> Float -> Color -> Point -> Picture
ball env r c p = at (sp env p) (Pictures [Color white (disc (r + 1)), Color c (disc r)])

-- 塗りつぶしの長方形（左上の角 (x, y)、幅 w、高さ h）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (a, b) = sp env (x + w / 2, y + h / 2) in Color c (rect a b w h)

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }

-- リストの i 番目だけ f で変える
setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i f xs = [ if k == i then f x else x | (k, x) <- zip [0 ..] xs ]

-- 実数のベクトルの階数（ガウスの消去法。小さい値は 0 とみなす）
rankReal :: [[Double]] -> Int
rankReal rows = go 0 rows
  where
    n = case rows of { (r : _) -> length r; [] -> 0 }
    val c r = case drop c r of { (v : _) -> v; [] -> 0 }
    go c rs
      | c >= n || null rs = 0
      | abs (val c p) < 1e-6 = go (c + 1) rs
      | otherwise = 1 + go (c + 1) [ zipWith (\x y -> x - f r * y) r p | r <- rest ]
      where
        ip = fst (maximumBy (\a b -> compare (abs (val c (snd a))) (abs (val c (snd b)))) (zip [0 :: Int ..] rs))
        p = case drop ip rs of { (r : _) -> r; [] -> [] }
        rest = [ r | (i, r) <- zip [0 ..] rs, i /= ip ]
        f r = val c r / val c p

-- GF(2) 上の階数：ベクトルをビット列（整数）で持ち、XOR で消去する
rankGF2 :: [Int] -> Int
rankGF2 = length . foldl' add []
  where
    add basis v0 =
      let v = foldl' (\v1 b -> min v1 (v1 `xor` b)) v0 basis
      in if v /= 0 then insertDesc v basis else basis
    insertDesc v bs = takeWhile (> v) bs ++ [v] ++ dropWhile (> v) bs
