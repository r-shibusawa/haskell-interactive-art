-- ============================================
-- 11 償却解析②：2 進カウンタ
-- INCREMENT は末尾の 1 を全部 0 にして、次の 0 を 1 にする。0111…1 → 1000…0 のときは k 回も反転！
-- でもビット i は 2^i 回に 1 回しか反転しない → n 回で合計 2n 回未満。1 回あたり O(1)
-- → = 1 回 INCREMENT / スペース = 自動 / 1〜3 = 見方（集計・出納・ポテンシャル）/ D = DECREMENT も混ぜる（償却が崩れる例）/ R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import Text.Printf (printf)

bitCount :: Int
bitCount = 12         -- ★ ビット数

autoSpeed :: Int
autoSpeed = 8         -- ★ 自動のとき何フレームごとに数えるか

data Rec = Rec { rCost :: !Int, rOnes :: !Int, rDec :: !Bool }

data World = World
  { wBits   :: ![Int]             -- A[0], A[1], ... の順
  , wFlipT  :: !(IM.IntMap Int)   -- 反転してからのフレーム数
  , wFlips  :: !(IM.IntMap Int)   -- ビットごとの反転回数
  , wHist   :: !(Seq.Seq Rec)     -- 最近の操作（300 回まで）
  , wView   :: !Int
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wCount  :: !Int
  , wUseDec :: !Bool
  , wUp     :: !Bool
  }

initial :: World
initial = reset (World [] IM.empty IM.empty Seq.empty 1 False 0 0 False True)

main :: IO ()
main = runSketch (sketch "12-11 Binary counter" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

reset :: World -> World
reset w = w
  { wBits = if wUseDec w then replicate (bitCount - 1) 1 ++ [0] else replicate bitCount 0   -- D のときは 0111…1 から始める
  , wFlipT = IM.fromList [(i, 99) | i <- [0 .. bitCount - 1]]
  , wFlips = IM.empty
  , wHist = Seq.empty
  , wCount = if wUseDec w then 2 ^ (bitCount - 1) - 1 else 0
  , wUp = True }

-- ★ INCREMENT（from = 1, to = 0）と DECREMENT（from = 0, to = 1）は、同じ形をしている
--   下から from のビットを全部ひっくり返し、最初のちがうビットもひっくり返す
rippleFlips :: Int -> [Int] -> ([Int], [Int])
rippleFlips from = go 0
  where
    go _ [] = ([], [])
    go i (b : bs)
      | b == from = let (bs', fl) = go (i + 1) bs in (1 - b : bs', i : fl)
      | otherwise = (1 - b : bs, [i])

applyOp :: Int -> Int -> Bool -> World -> World
applyOp from delta isDec w =
  let (bits, flipped) = rippleFlips from (wBits w)
      modulus = 2 ^ bitCount
      h = wHist w Seq.|> Rec (length flipped) (sum bits) isDec
  in w { wBits = bits
       , wFlipT = foldr (\i -> IM.insert i 0) (wFlipT w) flipped
       , wFlips = foldr (\i -> IM.insertWith (+) i 1) (wFlips w) flipped
       , wCount = (wCount w + delta + modulus) `mod` modulus
       , wHist = if Seq.length h > 300 then Seq.drop 1 h else h }

increment, decrement :: World -> World
increment = applyOp 1 1 False
decrement = applyOp 0 (-1) True

-- 0111…1 と 1000…0 の境目を行ったり来たり：毎回 K ビット反転する最悪の並び
tick :: World -> World
tick w
  | not (wUseDec w) = increment w
  | wCount w < boundary - 1 = increment w
  | wUp w = (increment w) { wUp = False }
  | otherwise = (decrement w) { wUp = True }
  where boundary = 2 ^ (bitCount - 1)

step :: World -> World
step w0 =
  let w = w0 { wFlipT = IM.map (\t -> min 99 (t + 1)) (wFlipT w0) }
  in if wAuto w
       then if wTimer w + 1 >= autoSpeed then tick w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
       else w

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> tick w
  EventKey (MouseButton LeftButton) Down _ _ -> tick w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wView = fromEnum c - fromEnum '0' }
    | c `elem` "dD" -> reset w { wUseDec = not (wUseDec w) }
    | c `elem` "rR" -> reset w
  _ -> w

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 ("Amortized analysis: binary counter - " ++ viewName)
    , txt 15 cMuted 32 60 "Right: count once   Space: auto   1-3: view   D: mix in DECREMENT   R: restart" ]
  ++ bitsPic
  ++ [txtC 18 cFg (wd / 2) (by + 100) ("value = " ++ show (wCount w))]
  ++ left ++ right )
  where
    wd = envW env
    ht = envH env
    k = bitCount
    viewName = case wView w of
      1 -> "1 aggregate"
      2 -> "2 accounting (1 yen on each 1)"
      _ -> "3 potential (Phi = number of 1s)"
    -- ビット
    bw = min 80 ((wd - 64) / i2f k)
    bx0 = wd / 2 - bw * i2f k / 2
    by = 110
    bitsPic = concat
      [ [ rbox (if b == 1 then cLime else cPanel) (x + 4) (by + (1 - fl) * 30) (bw - 8) (60 * fl + 0.1) ]
        ++ [txtC (max 14 (min 30 (bw * 0.5))) (if b == 1 then cBg else cMuted) (x + bw / 2) (by + 30) (show b) | fl > 0.4]
        ++ [txtC 14 cMuted (x + bw / 2) (by + 74) ("A[" ++ show i ++ "]")]
        ++ [circ cAmber (x + bw / 2) (by - 14) 20 | wView w == 2 && b == 1]
      | (i, b) <- zip [0 ..] (wBits w)
      , let x = bx0 + i2f (k - 1 - i) * bw
      , let t = IM.findWithDefault 99 i (wFlipT w)
      , let fl = if t < 6 then abs (cos (i2f t / 6 * pi)) else 1 ]
    -- 下左：1 回ごとのコストと累積
    gx = 60
    gy = 250
    gw = wd * 0.55
    gh = ht - gy - 130
    hist = toList (wHist w)
    n = length hist
    total = sum (map rCost hist)
    barW = gw / i2f (max 60 n)
    range = i2f (maximum [total, 2 * n, 2 * k]) * 1.1
    yOf v = gy + gh - v / range * gh
    bars =
      [ box (if rDec h then cRasp else if rCost h > 3 then cAmber else rgb255 0x6b 0x5a 0x78)
            (gx + i2f a * barW) (gy + gh - bh) (max 1 (barW - 1)) bh
      | (a, h) <- zip [0 :: Int ..] hist
      , let bh = i2f (rCost h) / i2f k * gh * 0.4 ]
    cums = drop 1 (scanl (+) 0 (map rCost hist))
    left =
      rbox cPanel (gx - 16) (gy - 16) (gw + 32) (gh + 120)
      : bars
      ++ [ pathL cAmber 2.5 [(gx + i2f (a + 1) * barW, yOf (i2f c)) | (a, c) <- zip [0 :: Int ..] cums]
         , seg cLime 2.5 (gx, gy + gh) (gx + i2f n * barW, yOf (2 * i2f n)) ]
      ++ [ pathL cCyan 2.5 [(gx + i2f (a + 1) * barW, yOf (i2f (rOnes h))) | (a, h) <- zip [0 :: Int ..] hist] | wView w == 3 ]
      ++ [ txt 15 cFg gx (gy + gh + 14) ("Operations " ++ show n ++ "   Total flips " ++ show total ++ "   Per op " ++ (if n > 0 then fixN 2 (i2f total / i2f n) else "-"))
         , legend cAmber gx (gy + gh + 40) "flips, cumulative"
         , legend cLime (gx + 260) (gy + gh + 40) "2n (bound)" ]
      ++ [legend cCyan (gx + 440) (gy + gh + 40) "Phi = 1s" | wView w == 3]
      ++ [txt 15 cRasp gx (gy + gh + 66) "With DECREMENT each op flips K bits and passes 2n" | wUseDec w]
    -- 右：ビットごとの反転回数（集計法）
    rx = gx + gw + 60
    rw = wd - rx - 40
    explain = case wView w of
      1 -> ["Bit i flips about n / 2^i times", "Total n(1 + 1/2 + 1/4 + ...) < 2n"]
      2 -> ["0->1 costs 2 yen: 1 flip, 1 saved", "1->0 is paid by the savings"]
      _ -> ["Amortized = actual + dPhi", "t trailing 1s: (t+1) + (1-t) = 2"]
    flipsOf i = IM.findWithDefault 0 i (wFlips w)
    mx = maximum (1 : IM.elems (wFlips w))
    rowH = min 22 ((gh + 20) / i2f k)
    right =
      rbox cPanel (rx - 16) (gy - 16) (rw + 32) (gh + 120)
      : [txt 15 cFg rx (gy - 4 + i2f l * 22) s | (l, s) <- zip [0 :: Int ..] explain]
      ++ concat
         [ [ txtR 14 cMuted (rx + 50) (y + rowH / 2) ("A[" ++ show i ++ "]")
           , rbox (if i == 0 then cLime else cCyan) (rx + 56) (y + 2) len (rowH - 4)
           , txtL 14 cFg (rx + 62 + len) (y + rowH / 2) (show (flipsOf i)) ]
         | i <- [0 .. k - 1]
         , let y = gy + 60 + i2f i * rowH
         , let len = (rw - 130) * i2f (flipsOf i) / i2f mx ]

-- ---------- やってみよう ----------
-- 1. bitCount を 16 にして、桁が増えても 1 回あたりが 2 未満のままか確かめる
-- 2. D を押して、累積の線が 2n を超えるのを見る
-- 3. 1 回の INCREMENT で反転する個数を数列にすると、どんな模様になる？

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

-- | グラフの凡例（短い線と文字）
legend :: Color -> Float -> Float -> String -> Picture
legend c x y s = Pictures [box c x (y + 7) 22 3, txt 15 c (x + 28) y s]

-- | 小数 n 桁の文字列（p5.js の toFixed）
fixN :: Int -> Float -> String
fixN n = printf ("%." ++ show n ++ "f")

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
