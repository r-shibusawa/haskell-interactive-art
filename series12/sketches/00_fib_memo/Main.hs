-- ============================================
-- 00 フィボナッチの呼び出し木：同じ計算を何度もしていない？
-- fib(n) = fib(n-1) + fib(n-2)。そのまま再帰すると同じ fib(k) を何度も計算する（部分問題の重複）
-- 一度計算した答えを表にメモすると、呼び出しが激減する。これが動的計画法（DP）の出発点
-- 1 = 素朴な再帰 / 2 = メモ化 / 3 = ボトムアップ / ←→ = n / スペース = 再生・停止 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import Text.Printf (printf)

startN :: Int
startN = 7            -- ★ 最初の n（素朴な再帰は 11 くらいまで）

speedScale :: Float
speedScale = 1        -- ★ アニメの速さ（2 にすると倍速）

-- 呼び出し木の 1 つの節
data Node = Node
  { nK      :: !Int     -- fib(k) の k
  , nDepth  :: !Int
  , nParent :: !Int     -- 親の番号（根は -1）
  , nHit    :: !Bool    -- メモから答えたか
  , nEnter  :: !Int     -- 呼ばれた時刻
  , nExit   :: !Int     -- 答えが出た時刻
  , nX      :: !Float   -- 左から何枚目の葉の位置か
  }

data World = World
  { wN       :: !Int
  , wMode    :: !Int
  , wNodes   :: !(IM.IntMap Node)
  , wEvents  :: !Int      -- 出来事（呼び出しと戻り）の数
  , wT       :: !Float    -- いま何個目の出来事まで見せるか
  , wPlaying :: !Bool
  , wLeaves  :: !Int
  , wDepth   :: !Int
  }

main :: IO ()
main = runSketch (sketch "12-00 Fibonacci call tree" (rebuild (World startN 1 IM.empty 0 0 True 1 1)))
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 再帰を実行しながら、呼び出しの記録を取っていく
data Build = Build
  { bEv    :: !Int
  , bNext  :: !Int
  , bLeaf  :: !Int
  , bDepth :: !Int
  , bMemo  :: !(IM.IntMap Int)
  , bNodes :: !(IM.IntMap Node)
  }

-- ★ fib(k) を呼ぶ。返り値は (答え, 横の位置, 記録)
call :: Int -> Int -> Int -> Int -> Build -> (Int, Float, Build)
call mode k depth parent b0 =
  let me = bNext b0
      enter = bEv b0
      b1 = b0 { bEv = enter + 1, bNext = me + 1, bDepth = max (bDepth b0) depth }
      memoHit = if mode == 2 then IM.lookup k (bMemo b1) else Nothing
      (val, hit, kids, b2) = case memoHit of
        Just v -> (v, True, [], b1)                  -- メモに答えがある → 計算しない
        Nothing
          | k < 2 -> (k, False, [], b1)
          | otherwise ->
              let (va, xa, ba) = call mode (k - 1) (depth + 1) me b1
                  (vb, xb, bb) = call mode (k - 2) (depth + 1) me ba
              in (va + vb, False, [xa, xb], bb)
      b3 = if mode == 2 then b2 { bMemo = IM.insert k val (bMemo b2) } else b2
      (x, b4) = case kids of
        [xa, xb] -> ((xa + xb) / 2, b3)
        _ -> (i2f (bLeaf b3), b3 { bLeaf = bLeaf b3 + 1 })
      exit = bEv b4
      node = Node k depth parent hit enter exit x
  in (val, x, b4 { bEv = exit + 1, bNodes = IM.insert me node (bNodes b4) })

rebuild :: World -> World
rebuild w
  | wMode w == 3 = w { wNodes = IM.empty, wEvents = wN w + 1, wT = 0 }
  | otherwise =
      let (_, _, b) = call (wMode w) (wN w) 0 (-1) (Build 0 0 0 0 IM.empty IM.empty)
      in w { wNodes = bNodes b, wEvents = bEv b, wT = 0, wLeaves = max 1 (bLeaf b), wDepth = bDepth b }

step :: World -> World
step w
  | wPlaying w && wT w < ev = w { wT = min ev (wT w + max 0.15 (ev / 500) * speedScale) }
  | otherwise = w
  where ev = i2f (wEvents w)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" ->
        let m = fromEnum c - fromEnum '0'
        in rebuild w { wMode = m, wN = if m == 1 then min (wN w) 11 else wN w }
    | c `elem` "rR" -> w { wT = 0, wPlaying = True }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wN = min limit (wN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wN = max 1 (wN w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w
  where limit = case wMode w of { 1 -> 11; 2 -> 22; _ -> 30 }

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  [ txt 26 cFg 32 24 ("fib(" ++ show (wN w) ++ ") - " ++ name)
  , txt 15 cMuted 32 60 "1 naive / 2 memo / 3 bottom-up   Left/Right: n   Space: play/pause   R: restart   Click: pause"
  , if wMode w == 3 then drawBottomUp env w shown else drawTree env w shown
  ]
  where
    shown = floor (wT w) :: Int
    name = case wMode w of
      1 -> "naive recursion"
      2 -> "memoization (top-down)"
      _ -> "bottom-up (fill the table from the left)"

drawTree :: Env -> World -> Int -> Picture
drawTree env w shown = Pictures
  ( [seg (withA 0.2 white) 1.5 (pos nd) (pos p) | nd <- visible, Just p <- [IM.lookup (nParent nd) nodes]]
  ++ map drawNode visible
  ++ panel
  ++ legendRow )
  where
    wd = envW env
    ht = envH env
    bigW = wd * 0.68
    top = 130
    bottom = ht - 60
    nodes = wNodes w
    n = wN w
    visible = [nd | nd <- IM.elems nodes, nEnter nd <= shown]
    dy = min 80 ((bottom - top) / i2f (max 1 (wDepth w)))
    leaves = wLeaves w
    r = clampF 8 36 ((bigW - 60) / i2f leaves * 0.8)
    pos nd = ( if leaves == 1 then bigW / 2 else remap (nX nd) 0 (i2f (leaves - 1)) 40 (bigW - 20)
             , top + i2f (nDepth nd) * dy )
    drawNode nd =
      let (x, y) = pos nd
          col | nHit nd = cCyan
              | nExit nd > shown = cAmber
              | otherwise = cRasp
      in Pictures (circ col x y r : [txtC 14 cBg x y (show (nK nd)) | r > 16])
    calls = length visible
    hits = length (filter nHit visible)
    -- fib(k) を実際に計算した回数
    counts = foldl' (\m nd -> if nHit nd then m else IM.insertWith (+) (nK nd) (1 :: Int) m) IM.empty visible
    mx = max 1 (maximum (0 : IM.elems counts))
    x0 = bigW + 20
    pw = wd - x0 - 24
    rowH = min 26 ((ht - 260) / i2f (n + 1))
    panel =
      [ rbox cPanel x0 110 pw (ht - 150)
      , txt 18 cFg (x0 + 16) 126 ("Calls: " ++ show calls ++ " / " ++ show (IM.size nodes)) ]
      ++ [txt 18 cCyan (x0 + 16) 152 ("Answered by memo: " ++ show hits) | wMode w == 2]
      ++ [txt 14 cMuted (x0 + 16) 186 "Times fib(k) was computed"]
      ++ concatMap row [0 .. n]
    row k =
      let y = 212 + i2f k * rowH
          c = IM.findWithDefault 0 k counts
          bw = (pw - 170) * i2f c / i2f mx
      in [ txtR 14 cMuted (x0 + 90) (y + rowH / 2) ("fib(" ++ show k ++ ")")
         , box (if c > 1 then cRasp else cCyan) (x0 + 98) (y + 3) bw (rowH - 6)
         , txtL 14 cFg (x0 + 104 + bw) (y + rowH / 2) (show c) ]
    ly = ht - 34
    legendRow =
      [ circ cAmber 40 ly 14, txtL 14 cMuted 52 ly "computing"
      , circ cRasp 170 ly 14, txtL 14 cMuted 182 ly "answered" ]
      ++ (if wMode w == 2 then [circ cCyan 300 ly 14, txtL 14 cMuted 312 ly "memo lookup"] else [])

drawBottomUp :: Env -> World -> Int -> Picture
drawBottomUp env w shown = Pictures (concatMap cell [0 .. n] ++ bottomText)
  where
    n = wN w
    wd = envW env
    cw = min 110 ((wd - 80) / i2f (n + 1))
    tall = cw < 60                -- 細いマスは縦長にして数字を縦に書く
    ch = if tall then 96 else cw
    y = envH env * 0.45
    x0 = (wd - cw * i2f (n + 1)) / 2
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs) :: [Int]
    cell i =
      let x = x0 + i2f i * cw
          filled = i < shown
          cur = i == shown - 1
          val = show (nth 0 fibs i)
          arcs = [ pathL (if d == 1 then cCyan else cRasp) 2.5
                     (bezierPts (sx, y + ch) (sx, y + ch + h) (ex, y + ch + h) (ex, y + ch))
                 | cur, i >= 2, d <- [1, 2 :: Int]
                 , let sx = x0 + i2f (i - d) * cw + cw / 2
                 , let ex = x + cw / 2
                 , let h = 50 + i2f d * 30 ]
      in [ cellBox (if filled then (if cur then rgb255 0x4a 0x3a 0x20 else cPanel) else cBg)
                   (if cur then cAmber else cGrid) (if cur then 3 else 1.5) (x + 3) y (cw - 6) (ch - 6)
         , txtC 14 cMuted (x + cw / 2) (y - 14) (if tall then show i else "f[" ++ show i ++ "]") ]
         ++ [ if tall then vertical (x + cw / 2) (y + ch / 2 - 3) val
                      else txtC (min 22 (max 14 (cw * 0.28))) cFg (x + cw / 2) (y + ch / 2 - 3) val
            | filled ]
         ++ arcs
    vertical cx cy s = Translate cx (-cy) (Rotate 90 (txtC 14 cFg 0 0 s))
    bottomText =
      [ txtC 18 cFg (wd / 2) (y + ch + 130)
          ("Additions: " ++ show (max 0 (min shown (n + 1) - 2)) ++ " (naive recursion: " ++ show (fibCalls n) ++ " calls)")
      , txtC 15 cMuted (wd / 2) (y + ch + 160) "f[i] = f[i-1] + f[i-2] for growing i. The answers you need are always in the table." ]

-- 素朴な再帰の呼び出し回数
fibCalls :: Int -> Int
fibCalls k
  | k < 2 = 1
  | otherwise = snd (foldl' (\(a, b) _ -> (b, a + b + 1)) (1, 1) [2 .. k])

-- ---------- やってみよう ----------
-- 1. startN を 11 にして素朴な再帰を見る。呼び出しは何回？ メモ化だと何回？
-- 2. speedScale を 3 にして、木が育つ速さを変える
-- 3. call の中で depth の代わりに k で高さを決めると、同じ fib(k) が同じ段に並ぶ

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

-- | 左上 (x, y)、幅 w、高さ h の塗りつぶした四角
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Polygon [(x, -y), (x + w, -y), (x + w, -y - h), (x, -y - h)])

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

-- | 線分（太さ t）
seg :: Color -> Float -> Point -> Point -> Picture
seg c t (x1, y1) (x2, y2)
  | t <= 1.2  = Color c (Line [(x1, -y1), (x2, -y2)])
  | otherwise = Color c (thickLine t (x1, -y1) (x2, -y2))

-- | 折れ線（太さ t）
pathL :: Color -> Float -> [Point] -> Picture
pathL c t ps = Color c (thickPath t [(x, -y) | (x, y) <- ps])

-- | 円（p5.js の circle と同じく d は直径）
circ :: Color -> Float -> Float -> Float -> Picture
circ c x y d = Color c (Translate x (-y) (disc (d / 2)))

-- | 3 次ベジェ曲線の点列（p5.js の bezier の代わり）
bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  [ (f ax bx cx dx t, f ay by cy dy t) | i <- [0 .. 20 :: Int], let t = fromIntegral i / 20 ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を左端の真ん中にして書く
txtL :: Float -> Color -> Float -> Float -> String -> Picture
txtL size c x y = label x (-y - size / 2) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 文字：(x, y) を右端の真ん中にして書く
txtR :: Float -> Color -> Float -> Float -> String -> Picture
txtR size c x y s = label (x - fromIntegral (length s) * size * 0.8) (-y - size / 2) size c s

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
