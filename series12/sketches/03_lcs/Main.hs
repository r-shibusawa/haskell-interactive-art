-- ============================================
-- 03 最長共通部分列（LCS）：2 つの文字列に共通する、いちばん長い「とびとびの列」
-- c[i][j] = X の先頭 i 文字と Y の先頭 j 文字の LCS の長さ
-- 文字が同じなら ↖ c[i-1][j-1] + 1、違えば ↑ か ← の大きいほう。最後に矢印を逆にたどると答え
-- スペース = 再生・停止 / → = 1 マス / 1〜3 = お手本 / P = 回文モード（Y = X を逆にしたもの → 最長回文部分列）/ R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.List (foldl')
import qualified Data.Set as Set
import Text.Printf (printf)

startX, startY :: String
startX = "CADBACD"      -- ★ 文字列を変えてみよう（この例の LCS は長さ 4）
startY = "ABCDBCA"

cellsPerFrame :: Float
cellsPerFrame = 0.25    -- ★ 1 フレームに埋めるマス数

data Dir = DiagD | UpD | LeftD | NoD deriving (Eq)

data World = World
  { wX       :: !String
  , wY       :: !String
  , wTab     :: !(Array (Int, Int) (Int, Dir))
  , wPath    :: ![(Int, Int, Dir)]
  , wFilled  :: !Float
  , wPlaying :: !Bool
  , wPal     :: !Bool
  }

main :: IO ()
main = runSketch (sketch "12-03 Longest common subsequence" (restart (World startX startY (lcsTable "" "") [] 0 True False)))
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ 表を作る。Haskell の配列は遅延評価なので、漸化式をそのまま書ける
lcsTable :: String -> String -> Array (Int, Int) (Int, Dir)
lcsTable xs ys = t
  where
    m = length xs
    n = length ys
    xa = listArray (1, m) xs
    ya = listArray (1, n) ys
    t = array ((0, 0), (m, n)) [((i, j), f i j) | i <- [0 .. m], j <- [0 .. n]]
    f i j
      | i == 0 || j == 0 = (0, NoD)
      | xa ! i == ya ! j = (fst (t ! (i - 1, j - 1)) + 1, DiagD)
      | fst (t ! (i - 1, j)) >= fst (t ! (i, j - 1)) = (fst (t ! (i - 1, j)), UpD)
      | otherwise = (fst (t ! (i, j - 1)), LeftD)

-- 右下から矢印を逆にたどる
tracePath :: Array (Int, Int) (Int, Dir) -> Int -> Int -> [(Int, Int, Dir)]
tracePath t i j
  | i <= 0 || j <= 0 = []
  | otherwise =
      let d = snd (t ! (i, j))
      in (i, j, d) : case d of
           DiagD -> tracePath t (i - 1) (j - 1)
           UpD -> tracePath t (i - 1) j
           _ -> tracePath t i (j - 1)

restart :: World -> World
restart w0 =
  let w = if wPal w0 then w0 { wY = reverse (wX w0) } else w0
      t = lcsTable (wX w) (wY w)
  in w { wTab = t, wPath = tracePath t (length (wX w)) (length (wY w)), wFilled = 0 }

totals :: World -> (Int, Int)
totals w = (length (wX w) * length (wY w), length (wPath w))

step :: World -> World
step w
  | wPlaying w = w { wFilled = min (i2f (total + plen + 1)) (wFilled w + max cellsPerFrame (i2f total / 400)) }
  | otherwise = w
  where (total, plen) = totals w

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ ->
    let (total, plen) = totals w
    in w { wPlaying = False, wFilled = min (i2f (total + plen + 1)) (i2f (floor (wFilled w) + 1 :: Int)) }
  EventKey (Char '1') Down _ _ -> restart w { wPal = False, wX = startX, wY = startY }
  EventKey (Char '2') Down _ _ -> restart w { wPal = False, wX = "GATTACAGCTTAGC", wY = "TCAGGATTCAGCAT" }  -- DNA の塩基配列（A C G T）
  EventKey (Char '3') Down _ _ -> restart w { wPal = False, wX = "STRAWBERRY", wY = "RASPBERRY" }
  EventKey (Char c) Down _ _
    | c `elem` "pP" ->
        if wPal w then restart w { wPal = False, wX = startX, wY = startY }
                  else restart w { wPal = True, wX = "SEPARATE" }
    | c `elem` "rR" -> w { wFilled = 0, wPlaying = True }
  _ -> w

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 (if wPal w then "Longest palindrome = LCS(X, reversed X)" else "Longest common subsequence (LCS)")
    , txt 15 cMuted 32 60 "Space: play/pause   Right: 1 cell   1-3: samples   P: palindrome mode   R: restart   Click: pause" ]
  ++ headers ++ cells ++ panel )
  where
    xs = wX w
    ys = wY w
    m = length xs
    n = length ys
    total = m * n
    shown = floor (wFilled w) :: Int
    cell = minimum [56, (envW env - 380) / i2f (n + 2), (envH env - 200) / i2f (m + 2)]
    x0 = 60
    y0 = 110
    (ci, cj) | shown < total = (shown `div` n + 1, shown `mod` n + 1)
             | otherwise = (-1, -1)
    fs = max 14 (min 20 (cell * 0.45))
    headers =
      [ txtC fs (if j == cj then cAmber else cMuted) (x0 + i2f (j + 1) * cell + cell / 2) (y0 + cell / 2) [nth ' ' ys (j - 1)]
      | j <- [1 .. n] ]
      ++ [ txtC fs (if i == ci then cAmber else cMuted) (x0 + cell / 2) (y0 + i2f (i + 1) * cell + cell / 2) [nth ' ' xs (i - 1)]
         | i <- [1 .. m] ]
    pathShown = if shown >= total then shown - total else -1
    walked = take (max 0 pathShown) (wPath w)
    onPath = Set.fromList [(i, j) | (i, j, _) <- walked]
    cells = concat
      [ [ cellBox col (if isCur then cAmber else if dep then cCyan else cGrid) (if isCur || dep then 2.5 else 1)
                  (x + 1) (y + 1) (cell - 2) (cell - 2) ]
        ++ (if visible || isCur
              then txtC 14 (if isCur then cAmber else cFg) (x + cell * 0.66) (y + cell * 0.66) (show c)
                   : (if i > 0 && j > 0 then [dirGlyph d x y cell] else [])
              else [])
      | i <- [0 .. m], j <- [0 .. n]
      , let x = x0 + i2f (j + 1) * cell
      , let y = y0 + i2f (i + 1) * cell
      , let idx = (i - 1) * n + (j - 1)
      , let visible = i == 0 || j == 0 || idx < shown
      , let isCur = i == ci && j == cj
      , let dep = shown < total && ((i == ci - 1 && j == cj - 1) || (i == ci - 1 && j == cj) || (i == ci && j == cj - 1))
      , let (c, d) = wTab w ! (i, j)
      , let col | Set.member (i, j) onPath = if d == DiagD then rgb255 0x5f 0x7f 0x1e else rgb255 0x4a 0x3d 0x56
                | visible = cPanel
                | otherwise = cBg ]
    px = x0 + i2f (n + 2) * cell + 30
    pw = envW env - px - 30
    lettersOf ps = reverse [nth ' ' xs (i - 1) | (i, _, DiagD) <- ps]
    lcs = lettersOf (wPath w)
    partial = lettersOf walked
    usedX = [i - 1 | (i, _, DiagD) <- walked]
    usedY = [j - 1 | (_, j, DiagD) <- walked]
    row lab str used yy = txt 18 cMuted (px + 16) (y0 + yy) lab
      : [ txt 18 (if k `elem` used then cLime else rgb255 0x6a 0x5a 0x76)
              (px + 44 + i2f k * min 20 ((pw - 60) / i2f (length str))) (y0 + yy) [ch]
        | (k, ch) <- zip [0 ..] str ]
    panel =
      [ rbox cPanel px y0 pw 300
      , txt 16 cFg (px + 16) (y0 + 14) "Rule"
      , txt 14 cMuted (px + 16) (y0 + 42) "X[i] = Y[j]: diagonal, +1"
      , txt 14 cMuted (px + 16) (y0 + 62) "otherwise: larger of up / left" ]
      ++ (if shown >= total
            then [ txt 16 cFg (px + 16) (y0 + 100) ("Length " ++ show (fst (wTab w ! (m, n))))
                 , txt (max 14 (min 30 (pw / i2f (max 4 (length lcs)) * 1.0))) cLime (px + 16) (y0 + 130)
                     (if pathShown >= length (wPath w) then lcs else partial) ]
                 ++ row "X" xs usedX 200 ++ row "Y" ys usedY 240
            else [txt 14 cMuted (px + 16) (y0 + 100) ("Cells filled " ++ show shown ++ " / " ++ show total)])

-- 矢印の記号（gloss の文字には ↖ がないので線で描く）
dirGlyph :: Dir -> Float -> Float -> Float -> Picture
dirGlyph d x y cell = Color col (arrow 1.5 (cx + dx * s, negate (cy + dy * s)) (cx - dx * s, negate (cy - dy * s)))
  where
    col = if d == DiagD then cLime else cMuted
    cx = x + cell * 0.28
    cy = y + cell * 0.3
    s = cell * 0.16
    (dx, dy) = case d of
      DiagD -> (1, 1)
      UpD -> (0, 1)
      LeftD -> (1, 0)
      NoD -> (0, 0)

-- ---------- やってみよう ----------
-- 1. startX, startY を自分の名前に変えて、共通部分列を探す
-- 2. 回文モード（P）で "SEPARATE" を自分の単語に変える
-- 3. tracePath で UpD と LeftD の順を変えると、別の LCS が見つかることがある

-- ---------- 道具：色と、左上が原点の座標で描く関数（p5.js の座標のまま書ける） ----------

cBg, cPanel, cFg, cMuted, cRasp, cCyan, cAmber, cLime, cGrid :: Color
cBg    = rgb255 0x16 0x11 0x1c
cPanel = rgb255 0x23 0x1a 0x2b
cFg    = rgb255 0xf3 0xea 0xf0
cMuted = rgb255 0x9a 0x8a 0xa3
cRasp  = rgb255 0xff 0x6b 0x8b
cCyan  = rgb255 0x5e 0xe6 0xe0
cAmber = rgb255 0xff 0xc8 0x57
cLime  = rgb255 0xa3 0xe6 0x35
cGrid  = rgb255 0x3a 0x2f 0x44

-- | 画面全体を「左上が原点」の座標に移す（中の図形は y を下向きで渡す）
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Pictures

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 四角の枠線（太さ t）
outline :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline c t x y w h = case cornerPts x y w h of
  ps@(p : _) | t <= 1.2  -> Color c (Line (ps ++ [p]))
             | otherwise -> Color c (thickPath t (ps ++ [p]))
  [] -> Blank

-- | 塗りと枠線のある表のマス
cellBox :: Color -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
cellBox f s t x y w h = Pictures [rbox f x y w h, outline s t x y w h]

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | リストの i 番目（範囲外なら d）
nth :: a -> [a] -> Int -> a
nth d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
