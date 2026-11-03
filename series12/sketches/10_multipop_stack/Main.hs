-- ============================================
-- 10 償却解析①：MULTIPOP つきスタック
-- MULTIPOP(k) は一度に k 枚取るので 1 回は高くつく。でも「積んだ皿より多くは取れない」ので平均すると 1 回 O(1)
-- 3 つの見方：①集計法（合計 ÷ 回数） ②出納法（積むときに 1 円の貯金を皿に乗せる） ③ポテンシャル法（Φ = 皿の枚数）
-- P またはクリック = PUSH / O = POP / M = MULTIPOP(k) / ↑↓ = k / スペース = ランダムに自動 / 1〜3 = 見方 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Text.Printf (printf)

startK :: Int
startK = 4              -- ★ MULTIPOP で取る枚数

autoSpeed :: Int
autoSpeed = 12          -- ★ 自動のとき、何フレームごとに操作するか

data OpKind = PushOp | PopOp | MultiOp deriving (Eq)

-- 操作の記録
data Rec = Rec { rKind :: !OpKind, rCost :: !Int, rSize :: !Int }

-- 飛んでいく皿
data Fly = Fly { fHue :: !Float, fY :: !Int, fT :: !Int, fDx :: !Float }

data World = World
  { wK     :: !Int
  , wStack :: ![Float]            -- 皿の色相。先頭がいちばん上
  , wHist  :: !(Seq.Seq Rec)      -- 最近の操作（400 回まで）
  , wFly   :: ![Fly]
  , wView  :: !Int
  , wAuto  :: !Bool
  , wTimer :: !Int
  }

main :: IO ()
main = runSketch (sketch "12-10 MULTIPOP stack" (World startK [] Seq.empty [] 1 False 0))
  { skBack  = cBg
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> onEvent ev w
  }

record :: OpKind -> Int -> World -> World
record kind cost w =
  let h = wHist w Seq.|> Rec kind cost (length (wStack w))
  in w { wHist = if Seq.length h > 400 then Seq.drop 1 h else h }

pushPlate :: World -> IO World
pushPlate w
  | length (wStack w) >= 30 = pure w
  | otherwise = do
      hue <- randomRIO (0, 360)
      pure (record PushOp 1 w { wStack = hue : wStack w })

-- ★ cnt 枚取る。実際に取れるのは積んである枚数まで
popPlates :: Int -> OpKind -> World -> IO World
popPlates cnt kind w = do
  let real = min cnt (length (wStack w))
      size = length (wStack w)
  dxs <- mapM (const (randomRIO (0, 4))) [1 .. real]
  let flying = [ Fly hue (size - a) 0 dx | (a, hue, dx) <- zip3 [0 ..] (wStack w) dxs ]
  pure (record kind real w { wStack = drop real (wStack w), wFly = wFly w ++ flying })

step :: World -> IO World
step w0 = do
  let w = w0 { wFly = [f { fT = fT f + 1 } | f <- wFly w0, fT f + 1 < 40] }
  if wAuto w
    then if wTimer w + 1 >= autoSpeed
      then do
        r <- randomRIO (0, 1 :: Float)
        let w1 = w { wTimer = 0 }
        if r < 0.62 then pushPlate w1
          else if r < 0.82 then popPlates 1 PopOp w1
          else popPlates (wK w1) MultiOp w1
      else pure w { wTimer = wTimer w + 1 }
    else pure w

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pushPlate w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wK = min 20 (wK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wK = max 1 (wK w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> pushPlate w
    | c `elem` "oO" -> popPlates 1 PopOp w
    | c `elem` "mM" -> popPlates (wK w) MultiOp w
    | c `elem` "123" -> pure w { wView = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> pure w { wStack = [], wHist = Seq.empty, wFly = [] }
  _ -> pure w

-- hsl(h, 60%, 65%) を hsb に直した色
plateColor :: Float -> Float -> Color
plateColor hue a = hsba hue 48.8 86 a

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 ("Amortized analysis: MULTIPOP stack - " ++ viewName)
    , txt 15 cMuted 32 60 ("P: push   O: pop 1   M: pop " ++ show (wK w) ++ "   Up/Down: k   Space: auto   1-3: view   R: restart")
    , box cGrid (sx - 20) base 200 8 ]
  ++ plates ++ viewStack ++ flying ++ graph ++ notes )
  where
    wd = envW env
    ht = envH env
    viewName = case wView w of
      1 -> "1 aggregate"
      2 -> "2 accounting (savings)"
      _ -> "3 potential"
    stack = reverse (wStack w)
    size = length stack
    sx = 60
    base = ht - 70
    ph = min 18 ((ht - 220) / 30)
    plates = concat
      [ rbox (plateColor hue 1) sx (y + 1) 160 (ph - 2)
        : [circ cAmber (sx + 180) (y + ph / 2) (ph * 0.8) | wView w == 2]
      | (a, hue) <- zip [0 :: Int ..] stack
      , let y = base - i2f (a + 1) * ph ]
    viewStack
      | wView w == 3 =
          [ outline cCyan 2 (sx - 10) (base - i2f size * ph) 180 (i2f size * ph)
          , txtL 14 cCyan (sx + 172) (base - i2f size * ph - 12) ("Phi=" ++ show size) ]
      | wView w == 2 = [txt 14 cAmber 40 100 "coin = 1 yen saved"]
      | otherwise = []
    flying =
      [ rbox (plateColor (fHue f) (max 0 (1 - t / 40))) (sx - t * (2 + fDx f * 0.3))
             (base - i2f (fY f) * ph - t * 6 + t * t * 0.3) 160 (ph - 2)
      | f <- wFly w, let t = i2f (fT f) ]
    -- グラフ
    gx = 320
    gy = 110
    gw = wd - gx - 40
    gh = ht - 260
    hist = toList (wHist w)
    ops = length hist
    actual = sum (map rCost hist)
    maxBar = foldl' max 4 (map rCost hist)
    bw = gw / i2f (max 40 ops)
    cmax = i2f (max (2 * ops) 10)
    bars =
      [ box (case rKind h of
               PushOp -> rgb255 0x6b 0x5a 0x78
               PopOp -> cCyan
               MultiOp -> cRasp)
            (gx + i2f a * bw) (gy + gh - bh) (max 1 (bw - 1)) bh
      | (a, h) <- zip [0 :: Int ..] hist
      , let bh = i2f (rCost h) / i2f maxBar * gh * 0.35 ]
    cums = drop 1 (scanl (+) 0 (map rCost hist))
    yOf v = gy + gh - v / cmax * gh
    graph =
      rbox cPanel (gx - 16) (gy - 10) (gw + 32) (gh + 150)
      : bars
      ++ [ pathL cAmber 2.5 [(gx + i2f (a + 1) * bw, yOf (i2f c)) | (a, c) <- zip [0 :: Int ..] cums]
         , seg cLime 2.5 (gx, gy + gh) (gx + i2f ops * bw, yOf (2 * i2f ops)) ]
      ++ [ pathL cCyan 2.5 [(gx + i2f (a + 1) * bw, yOf (i2f (rSize h))) | (a, h) <- zip [0 :: Int ..] hist] | wView w == 3 ]
    ty = gy + gh + 16
    explain = case wView w of
      1 -> "Each plate moves twice at most: pushed once, popped once. n operations cost at most 2n in total, so O(1) each."
      2 -> "A push pays 2 yen: 1 for the work, 1 saved on the plate. POP and MULTIPOP are paid by the savings, which never go negative."
      _ -> "Amortized = actual + dPhi. PUSH 1 + 1 = 2, POP 1 - 1 = 0, MULTIPOP(k) k - k = 0. Phi >= 0, so the total is at most 2n."
    notes =
      [ txt 15 cFg gx ty ("Operations " ++ show ops ++ "   Total actual cost " ++ show actual ++ "   Per operation " ++ (if ops > 0 then fixN 2 (i2f actual / i2f ops) else "-"))
      , legend cAmber gx (ty + 28) "actual cost, cumulative"
      , legend cLime (gx + 330) (ty + 28) "2 x operations (bound)" ]
      ++ [legend cCyan (gx + 650) (ty + 28) "Phi = plates" | wView w == 3]
      ++ [txt 14 cMuted gx (ty + 58 + i2f l * 20) s | (l, s) <- zip [0 :: Int ..] (wrapText 80 explain)]

-- ---------- やってみよう ----------
-- 1. step の確率を変えて、PUSH を減らすとグラフはどうなる？
-- 2. startK を 20 にしても、1 回あたりのコストが 2 を超えないことを確かめる
-- 3. スタックが空のとき POP はコスト 0。コスト 1 に変えると上限はどうなる？

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

-- | グラフの凡例（短い線と文字）
legend :: Color -> Float -> Float -> String -> Picture
legend c x y s = Pictures [box c x (y + 7) 22 3, txt 15 c (x + 28) y s]

-- | 小数 n 桁の文字列（p5.js の toFixed）
fixN :: Int -> Float -> String
fixN n = printf ("%." ++ show n ++ "f")

-- | 長い文を 1 行 n 文字くらいで折り返す
wrapText :: Int -> String -> [String]
wrapText n = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= n = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
