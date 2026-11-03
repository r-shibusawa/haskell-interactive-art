-- ============================================
-- 12 償却解析③：動的配列（のびちぢみする表）
-- 満杯になったら 2 倍の表を作って全部コピー（拡張）。たまに高いけど、1 回あたりは平均 3 以下
-- 縮めるとき「半分以下で半分に」すると、境目で拡張と縮小を繰り返して大損！「1/4 以下で半分に」が正解
-- I = 挿入 / D = 削除 / スペース = 自動 / 1 = 挿入だけ / 2 = 1/2 で縮める（失敗例）/ 3 = 1/4 で縮める（正解）/ R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Text.Printf (printf)

autoSpeed :: Int
autoSpeed = 10          -- ★ 自動のとき何フレームごとに操作するか

data Rec = Rec { rIns :: !Bool, rCost :: !Int, rPhi :: !Float, rAmort :: !Float }

data World = World
  { wCap     :: !Int
  , wNum     :: !Int
  , wPolicy  :: !Int
  , wHist    :: !(Seq.Seq Rec)   -- 最近の操作（240 回まで）
  , wAuto    :: !Bool
  , wTimer   :: !Int
  , wPattern :: !Int
  , wAnim    :: !Float
  , wLast    :: !Int             -- 最後にコピーした個数
  , wOld     :: !Int             -- 拡張する前の容量
  }

main :: IO ()
main = runSketch (sketch "12-12 Dynamic table" (World 0 0 1 Seq.empty False 0 0 0 0 0))
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ ポテンシャル関数：使用率が半分以上なら 2·要素数 − 容量、半分未満なら 容量/2 − 要素数
phi :: Int -> Int -> Float
phi cap num
  | cap == 0 = 0
  | i2f num / i2f cap >= 0.5 = i2f (2 * num - cap)
  | otherwise = i2f cap / 2 - i2f num

record :: Bool -> Int -> Float -> World -> World
record isIns cost before w =
  let p = phi (wCap w) (wNum w)
      h = wHist w Seq.|> Rec isIns cost p (i2f cost + p - before)
  in w { wHist = if Seq.length h > 240 then Seq.drop 1 h else h }

insertItem :: World -> World
insertItem w
  | wNum w >= 512 = w
  | wNum w == wCap w =
      record True (1 + wNum w) before
        w { wOld = wCap w, wCap = max 1 (wCap w * 2), wLast = wNum w, wAnim = 1, wNum = wNum w + 1 }
  | otherwise = record True 1 before w { wNum = wNum w + 1 }
  where before = phi (wCap w) (wNum w)

deleteItem :: World -> World
deleteItem w
  | wNum w == 0 = w
  | wPolicy w /= 1 && wCap w > 1 && i2f num / i2f (wCap w) < limit =
      record False (1 + num) before
        w { wNum = num, wOld = wCap w, wCap = max 1 (wCap w `div` 2), wLast = num, wAnim = 1 }
  | otherwise = record False 1 before w { wNum = num }
  where
    before = phi (wCap w) (wNum w)
    num = wNum w - 1
    limit = if wPolicy w == 2 then 0.5 else 0.25

-- 境目 16 ↔ 17 を行ったり来たり
autoMove :: World -> World
autoMove w
  | wPolicy w == 1 = insertItem w
  | wNum w < 16 && wPattern w == 0 = insertItem w
  | otherwise =
      let moved = if wPattern w `mod` 4 `elem` [0, 3] then insertItem w else deleteItem w
      in moved { wPattern = wPattern w + 1 }

step :: World -> World
step w0 =
  let w = w0 { wAnim = max 0 (wAnim w0 - 0.04) }
  in if wAuto w
       then if wTimer w + 1 >= autoSpeed then autoMove w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
       else w

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> insertItem w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c `elem` "iI" -> insertItem w
    | c `elem` "dD" -> deleteItem w
    | c `elem` "123" -> clear w { wPolicy = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> clear w
  _ -> w
  where clear v = v { wCap = 0, wNum = 0, wHist = Seq.empty, wPattern = 0 }

policyName :: Int -> String
policyName p = case p of
  1 -> "insert only (double when full)"
  2 -> "halve below load 1/2 (bad example)"
  _ -> "halve below load 1/4 (correct)"

showPhi :: Float -> String
showPhi v | v == fromIntegral (round v :: Int) = show (round v :: Int)
          | otherwise = fixN 1 v

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 ("Dynamic table: " ++ policyName (wPolicy w))
    , txt 15 cMuted 32 60 "I: insert   D: delete   Space: auto   1-3: policy   R: restart" ]
  ++ cells
  ++ [txt 16 cFg 32 ty ("Items " ++ show num ++ "   Capacity " ++ show cap ++ "   Load " ++ (if cap > 0 then fixN 2 (i2f num / i2f cap) else "-") ++ "   Phi = " ++ showPhi (phi cap num))]
  ++ [txt 16 cAmber 720 ty ("Copied " ++ show (wLast w) ++ " items!") | wAnim w > 0]
  ++ (if gh < 60 then [] else graph) )
  where
    wd = envW env
    ht = envH env
    cap = wCap w
    num = wNum w
    cols = 32
    rows = max 1 ((cap + cols - 1) `div` cols)
    cell = minimum [34, (wd - 64) / i2f cols, ht * 0.36 / i2f rows]
    y0 = 100
    anim = wAnim w
    cells = concat
      [ [ rbox fillC (x + 2) (y + 2) (cell - 4) (cell - 4) ]
        ++ [ outline (if isNew then cAmber else cGrid) (if isNew then 2 else 1) (x + 2) (y + 2) (cell - 4) (cell - 4) | cell >= 10 || isNew ]
      | a <- [0 .. cap - 1]
      , let x = 32 + i2f (a `mod` cols) * cell
      , let y = y0 + i2f (a `div` cols) * cell
      , let isNew = anim > 0 && a >= wOld w && cap > wOld w
      , let fillC | a >= num = cPanel
                  | a < wLast w && anim > 0 = mixC cLime cAmber anim
                  | otherwise = cLime ]
    ty = y0 + i2f rows * cell + 12
    -- グラフ
    gx = 50
    gy = ty + 50
    gw = wd - 100
    gh = ht - gy - 110
    hist = toList (wHist w)
    n = length hist
    bw = gw / i2f (max 80 n)
    vmax = foldl' max 10 (concatMap (\h -> [i2f (rCost h), rPhi h]) hist)
    px a = gx + i2f a * bw + bw / 2
    py v = gy + gh - v / vmax * gh
    total = sum (map rCost hist)
    graph =
      rbox cPanel (gx - 18) (gy - 12) (gw + 36) (gh + 96)
      : [ box (if rCost h > 1 then (if rIns h then cAmber else cRasp) else rgb255 0x6b 0x5a 0x78)
              (gx + i2f a * bw) (gy + gh - bh) (max 1 (bw - 1)) bh
        | (a, h) <- zip [0 :: Int ..] hist
        , let bh = i2f (rCost h) / vmax * gh ]
      ++ [ pathL cCyan 2 [(px a, py (rPhi h)) | (a, h) <- zip [0 :: Int ..] hist]
         , pathL cLime 2 [(px a, py (max 0 (rAmort h))) | (a, h) <- zip [0 :: Int ..] hist]
         , txt 15 cFg gx (gy + gh + 12) ("Operations " ++ show n ++ "   Total actual cost " ++ show total ++ "   Per op " ++ (if n > 0 then fixN 2 (i2f total / i2f n) else "-"))
         , box cAmber gx (gy + gh + 41) 13 13
         , txt 15 cAmber (gx + 20) (gy + gh + 40) "insert + grow"
         , box cRasp (gx + 210) (gy + gh + 41) 13 13
         , txt 15 cRasp (gx + 230) (gy + gh + 40) "delete + shrink"
         , legend cCyan (gx + 440) (gy + gh + 40) "Phi (potential)"
         , legend cLime (gx + 680) (gy + gh + 40) "amortized = actual + dPhi" ]

-- ---------- やってみよう ----------
-- 1. 2 にして自動（スペース）を見る。1 回あたりのコストはどこまで増える？
-- 2. 3 に変えると、同じ操作でも安くなることを確かめる
-- 3. insertItem の 2 倍を 3 倍にすると、平均のコストはどう変わる？

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

-- | 折れ線（太さ t）
pathL :: Color -> Float -> [Point] -> Picture
pathL c t ps = Color c (thickPath t [(x, -y) | (x, y) <- ps])

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | グラフの凡例（短い線と文字）
legend :: Color -> Float -> Float -> String -> Picture
legend c x y s = Pictures [box c x (y + 7) 22 3, txt 15 c (x + 28) y s]

-- | 2 色を混ぜる（p5.js の lerpColor）
mixC :: Color -> Color -> Float -> Color
mixC a b t0 =
  let t = clampF 0 1 t0
      (r1, g1, b1, _) = rgbaOfColor a
      (r2, g2, b2, _) = rgbaOfColor b
  in makeColor (lerpF r1 r2 t) (lerpF g1 g2 t) (lerpF b1 b2 t) 1

-- | 小数 n 桁の文字列（p5.js の toFixed）
fixN :: Int -> Float -> String
fixN n = printf ("%." ++ show n ++ "f")

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
