-- ============================================
-- 10 ワン・タイル（Wang tiles）
-- 4 つの辺に色がついた正方形。となりと接する辺の色が同じになるように並べる（回転は禁止）
--   左上から順に「合うタイル」をランダムに選ぶと模様（テクスチャ）ができる。合うものがなければ 1 つ戻る
--   タイルの組によっては「くり返さない並べ方しかできない」ものがある（非周期セット）
-- 1 = 2 色 16 枚（必ず埋まる）/ 2 = 3 色の少ないセット（戻りながら埋める）/ スペース = 作り直す / D = 描き方 / ↑↓ = 大きさ / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, (.&.))
import Data.List (foldl', nub)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, randomR)

-- タイル：[北, 東, 南, 西] の色番号
data Tile = Tile { tN, tE, tS, tW :: !Int } deriving (Eq)

data World = World
  { wMode    :: !Int     -- ★ 1 = 2 色の全部入りセット、2 = 3 色の少ないセット
  , wCell    :: !Float   -- ★ タイル 1 枚の大きさ
  , wStyle   :: !Int     -- 0 = 辺の三角形、1 = 道（パイプ）
  , wTiles   :: V.Vector Tile
  , wCols, wRows :: !Int
  , wPlaced  :: !(U.Vector Int)   -- マスごとのタイル番号（−1 = まだ）
  , wStack   :: [([Int], Int)]    -- 探索の記録（候補, いまの番号）。先頭が最新
  , wPos, wBack :: !Int
  , wStuck   :: !Bool
  , wSeed    :: !Int
  , wGen     :: !StdGen
  }

setSize :: Int
setSize = 4              -- ★ モード 2：k × k のドーナツから作る（最大 k² 枚）

world0 :: World
world0 = World 1 48 1 V.empty 0 0 U.empty [] 0 0 False 1 (mkStdGen 1)

main :: IO ()
main = runSketch (sketch "32-10 Wang Tiles" world0)
  { skBack  = rgb255 16 18 26
  , skStart = \env -> pure (makeSet env world0)
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 辺の色
edgeCol :: Int -> Color
edgeCol i = case i of
  0 -> rgb255 60 70 100
  1 -> rgb255 250 190 70
  _ -> rgb255 90 200 220

makeSet :: Env -> World -> World
makeSet env w =
  let g0 = mkStdGen (wSeed w)
  in if wMode w == 1
       then restart env w { wGen = g0
              , wTiles = V.fromList [ Tile ((b `shiftR` 3) .&. 1) ((b `shiftR` 2) .&. 1) ((b `shiftR` 1) .&. 1) (b .&. 1) | b <- [0 .. 15] ] }
       else
         -- k × k のドーナツ（上下・左右がつながった盤）の辺をランダムに塗り、そこに現れるタイルだけを集める
         -- → このセットは少なくとも「くり返し」で並べられるが、ランダムに置くと詰まることがある
         let k = setSize
             (colors, g1) = randoms3 (2 * k * k) g0
             v = U.fromList (take (k * k) colors)
             hz = U.fromList (drop (k * k) colors)
             vAt x y = v U.! ((x `mod` k) * k + (y `mod` k))
             hAt x y = hz U.! ((x `mod` k) * k + (y `mod` k))
             ts = nub [ Tile (hAt x y) (vAt (x + 1) y) (hAt x (y + 1)) (vAt x y) | x <- [0 .. k - 1], y <- [0 .. k - 1] ]
         in restart env w { wGen = g1, wTiles = V.fromList ts }

randoms3 :: Int -> StdGen -> ([Int], StdGen)
randoms3 n g0 = foldr (\_ (acc, g) -> let (c, g') = randomR (0, 2) g in (c : acc, g')) ([], g0) [1 .. n]

restart :: Env -> World -> World
restart env w =
  let cols = max 1 (floor ((envW env - 40) / wCell w))
      rows = max 1 (floor ((envH env - 150) / wCell w))
  in w { wCols = cols, wRows = rows, wPlaced = U.replicate (cols * rows) (-1)
       , wStack = [], wPos = 0, wBack = 0, wStuck = False }

-- ★ 1 フレームに何マス進めるか
stepWorld :: World -> World
stepWorld w0 = foldl' (\w _ -> if wPos w < wCols w * wRows w && not (wStuck w) then stepFill w else w) w0 [1 .. 12 :: Int]

-- ★ 1 マス進める：上と左に合うタイルの中からランダムに選ぶ。なければ戻る
stepFill :: World -> World
stepFill w0 =
  let w = if length (wStack w0) == wPos w0
            then let (c, g') = shuffle (candidatesAt w0 (wPos w0)) (wGen w0)
                 in w0 { wStack = (c, -1) : wStack w0, wGen = g' }
            else w0
      pos = wPos w
  in case wStack w of
       [] -> w { wStuck = True }
       ((cands, idx) : rest) -> case drop (idx + 1) cands of
         (t : _) -> w { wStack = (cands, idx + 1) : rest
                      , wPlaced = wPlaced w U.// [(pos, t)], wPos = pos + 1 }
         [] ->
           -- 候補を使い切った → このマスを空にして 1 つ前へ
           let back = wBack w + 1
               p1 = pos - 1
               stuck = p1 < 0 || back > 20000
               p2 = max 0 p1
           in w { wStack = rest, wPlaced = wPlaced w U.// [(pos, -1), (p2, -1)]
                , wPos = p2, wBack = back, wStuck = stuck }

candidatesAt :: World -> Int -> [Int]
candidatesAt w i =
  let x = i `mod` wCols w
      y = i `div` wCols w
      tileAt j = let t = wPlaced w U.! j in if t >= 0 then wTiles w V.!? t else Nothing
      okLeft t = x == 0 || maybe True (\l -> tE l == tW t) (tileAt (i - 1))        -- 左のタイルの東 = 自分の西
      okUp t = y == 0 || maybe True (\u -> tS u == tN t) (tileAt (i - wCols w))    -- 上のタイルの南 = 自分の北
  in [ k | (k, t) <- zip [0 ..] (V.toList (wTiles w)), okLeft t, okUp t ]

drawAll :: Env -> World -> Picture
drawAll env w =
  let cols = wCols w
      rows = wRows w
      cell = wCell w
      ox = (envW env - fromIntegral cols * cell) / 2
      oy = 120
      grid = [ let x = ox + fromIntegral (i `mod` cols) * cell
                   y = oy + fromIntegral (i `div` cols) * cell
                   t = wPlaced w U.! i
               in case (if t >= 0 then wTiles w V.!? t else Nothing) of
                    Just tile -> drawTile (wStyle w) tile x y cell
                    Nothing -> Color (rgb255 28 30 40) (box x y (cell - 1) (cell - 1))
             | i <- [0 .. cols * rows - 1] ]
      -- セットの見本
      samples = [ drawTile (wStyle w) t (20 + fromIntegral k * 34) 70 30 | (k, t) <- zip [0 :: Int ..] (V.toList (wTiles w)) ]
      nTiles = V.length (wTiles w)
      title | wMode w == 1 = "2 colors, 16 tiles (always fits)  (1, 2)"
            | otherwise = "3 colors, " ++ show nTiles ++ " tiles (" ++ show setSize ++ "x" ++ show setSize ++ " torus)  (1, 2)"
      st | wStuck w = "Could not fill -> Space for a new set"
         | wPos w >= cols * rows = "Done!"
         | otherwise = "Placing..."
  in Pictures
    [ onP5 env (Pictures (grid ++ samples))
    , txt env 20 36 20 white title
    , txt env 640 36 15 (gray 0.75) (st ++ "   back " ++ show (wBack w) ++ "   D style")
    , txt env 20 (envH env - 20) 14 (gray 0.6) "1966 Berger: aperiodic set (20426 tiles). 2015 Jeandel-Rao: min 11 tiles, 4 colors. No general test for tileability."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (makeSet env w { wSeed = wSeed w + 1 })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (restart env w { wCell = min 96 (wCell w + 8) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (restart env w { wCell = max 16 (wCell w - 8) })
  EventKey (Char c) Down _ _
    | c == '1' || c == '2' -> pure (makeSet env w { wMode = fromEnum c - 48 })
    | c == 'd' || c == 'D' -> pure w { wStyle = (wStyle w + 1) `mod` 2 }
    | c == 's' || c == 'S' -> saveSvg "wang" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. モード 1 の道のスタイルは、16 枚から迷路や配管のような模様ができる。ゲームの地形づくりにも使われる
-- 2. モード 2 で setSize を 3 → 6 と変えると、戻る回数はどう変わる？ 左上の見本と模様を見比べよう
-- 3. edgeCol を変え、style 0 の三角形の色で「床のモザイク」を作ろう
-- 4. 調べてみよう：ワン・タイルで計算機（チューリング機械）を作れる → 判定不能の証明


-- ==================================================
-- ここから下は「描画の道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs g0 = go (length xs) xs g0 []
  where
    go _ [] g acc = (acc, g)
    go n l g acc =
      let (i, g1) = randomR (0, n - 1) g
      in case splitAt i l of
           (a, x : b) -> go (n - 1) (a ++ b) g1 (x : acc)
           (a, []) -> go (n - 1) (drop 1 a) g1 acc

drawTile :: Int -> Tile -> Float -> Float -> Float -> Picture
drawTile style t x y s =
  let cx = x + s / 2
      cy = y + s / 2
      es = [tN t, tE t, tS t, tW t]
  in if style == 0
       then
         let corners = [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
             tris = [ Color (edgeCol e) (Polygon [a, b, (cx, cy)])
                    | (e, a, b) <- zip3 es corners (drop 1 corners ++ take 1 corners) ]
         in Pictures (tris ++ [Color (rgb255 16 18 26) (rectLine cx cy s s)])
       else
         -- 道：色 0 以外の辺から真ん中へ太い線を引く
         let mids = [(cx, y), (x + s, cy), (cx, y + s), (x, cy)]
             arms = [ Pictures [Color (edgeCol e) (thickLine (s * 0.32) m (cx, cy))]
                    | (e, m) <- zip es mids, e /= 0 ]
             top = foldl' max 0 es
         in Pictures
              ( Color (rgb255 34 38 52) (box x y s s)
              : arms ++ [ Color (edgeCol top) (at (cx, cy) (disc (s * 0.16))) | top > 0 ] )
