-- ============================================
-- 10 楕円曲線暗号
-- 左：実数の楕円曲線 y² = x³ + ax + b。2 点 P, Q を通る直線が曲線と交わる 3 つ目の点を上下反転 → P + Q
-- 右：同じ「足し算」を mod p の世界（有限体）でやると点がバラバラに散らばる。
--     k·G（G を k 回足す）は速く計算できるが、k·G から k を求めるのは難しい → 鍵共有（ECDH）に使える
-- クリック = 点 P を置く（マウスが Q）/ ↑↓ = a / ←→ = b / 1〜3 = 素数 p / スペース = k·G のアニメ / N = 秘密を選び直す
-- ※ 教育用（実際は 256 ビットの p を使う）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Control.Monad (join)
import Text.Printf (printf)

primesP :: [Int]
primesP = [31, 97, 251]            -- ★ 有限体の素数 p

fieldA, fieldB :: Int
fieldA = 2                         -- 有限体の曲線 y² = x³ + 2x + 3 (mod p)
fieldB = 3

-- 有限体の点。Nothing が無限遠点 O
type FP = Maybe (Int, Int)

data World = World
  { ca       :: !Float             -- ★ 実数の曲線の係数
  , cb       :: !Float
  , pxReal   :: !Float             -- 点 P（x 座標と上下）
  , pSign    :: !Float
  , pIdx     :: !Int
  , fpPoints :: ![(Int, Int)]
  , genG     :: !(Int, Int)
  , orderG   :: !Int
  , multiples :: !(V.Vector FP)    -- 1·G, 2·G, …
  , kShow    :: !Int
  , anim     :: !Bool
  , alice    :: !Int
  , bob      :: !Int
  }

main :: IO ()
main = do
  w0 <- buildField 1 (World (-1) 1 (-1.2) 1 1 [] (0, 0) 1 V.empty 1 True 2 2)
  runSketch (sketch "34-10 Elliptic Curve Cryptography" w0)
    { skBack  = rgb255 14 16 26
    , skStep  = \env _ w -> pure
        (if anim w && envFrame env `mod` 20 == 0 then w { kShow = kShow w `mod` max 1 (orderG w) + 1 } else w)
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

primeOf :: World -> Int
primeOf w = case drop (pIdx w) primesP of
  (p : _) -> p
  [] -> 97

-- ---- 有限体
buildField :: Int -> World -> IO World
buildField i w0 = newSecrets w0
  { pIdx = i, fpPoints = pts, genG = g, orderG = ord', multiples = V.fromList mult, kShow = 1 }
  where
    p = primeOf w0 { pIdx = i }
    pts = [ (x, y) | x <- [0 .. p - 1], y <- [0 .. p - 1]
                   , (y * y - (x * x * x + fieldA * x + fieldB)) `mod` p == 0 ]
    -- いちばん位数の大きい点を G に（同じ位数なら先に見つかった点）
    orderOf q = go 1 (Just q)
      where go :: Int -> FP -> Int
            go !k (Just c) | k < 1000 = go (k + 1) (addFp p (Just c) (Just q))
            go !k _ = k
    (ord', g) = foldl (\(bo, bg) q -> let o = orderOf q in if o > bo then (o, q) else (bo, bg)) (0, (0, 0)) pts
    mult = take ord' (iterate (\c -> addFp p c (Just g)) (Just g))

newSecrets :: World -> IO World
newSecrets w = do
  let top = max 0 (orderG w - 4)
  a <- (2 +) <$> randomRIO (0, top)
  b <- (2 +) <$> randomRIO (0, top)
  pure w { alice = a, bob = b }

-- 実数の曲線の値
fx :: World -> Float -> Float
fx w x = x * x * x + ca w * x + cb w

-- 実数の曲線上の点（x で y² < 0 なら Nothing）
curvePoint :: World -> Float -> Float -> Maybe Point
curvePoint w x sgn = let v = fx w x in if v < 0 then Nothing else Just (x, sgn * sqrt v)

-- ★ 実数の点の足し算（P = Q なら接線）。(x3, y3, 傾き) を返す
addReal :: World -> Point -> Point -> Maybe (Float, Float, Float)
addReal w (px, py) (qx, qy)
  | abs (px - qx) < 1e-6 && abs (py + qy) < 1e-6 = Nothing
  | isNaN lam || isInfinite lam || abs lam > 1e4 = Nothing
  | otherwise = let x3 = lam * lam - px - qx in Just (x3, lam * (px - x3) - py, lam)
  where
    lam | abs (px - qx) < 1e-6 = (3 * px * px + ca w) / (2 * py)
        | otherwise = (qy - py) / (qx - px)

-- 左のパネルの位置 (x, y, 幅, 高さ)
leftPanel :: Env -> (Float, Float, Float, Float)
leftPanel env = (20, 80, envW env * 0.46, envH env - 110)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ left ++ right)
  where
    sw = envW env
    sh = envH env
    amber = rgb255 255 200 90
    pink = rgb255 255 120 200
    green' = rgb255 120 230 150
    header =
      [ txt env 24 34 16 (gray 0.94)
          ("Elliptic curve  click P, mouse Q  Up/Down a = " ++ f2 (ca w) ++ "  Left/Right b = " ++ f2 (cb w)
            ++ "  1-3 p = " ++ show (primeOf w) ++ "  Space k*G  N secrets")
      , txt env 24 56 14 (gray 0.6) "Educational: with a small p, k is found from k*G by brute force right away"
      ]

    -- ================= 左：実数 =================
    (lx, ly, lw, lh) = leftPanel env
    toPanel (x, y) = (lx + lw / 2 + x * lw / 7, ly + lh / 2 - y * lh / 9)
    inPanel (x, y) = x >= lx && x <= lx + lw && y >= ly && y <= ly + lh
    (ox, oy) = toPanel (0, 0)
    seg t c a b = Color c (thickLine t (toS env a) (toS env b))
    -- 曲線（上と下の枝）。パネルの外に出る部分は描かない
    curve = concatMap branch [1, -1]
    branch sgn =
      let samples = [ if v < 0 then Nothing else Just (toPanel (x, sgn * sqrt v))
                    | i <- [0 .. 700 :: Int]
                    , let x = -3.5 + 7 * fromIntegral i / 700
                          v = fx w x ]
      in [ seg 2.5 (rgb255 120 200 255) a b
         | (Just a, Just b) <- zip samples (drop 1 samples)
         , abs (snd b - snd a) < 200, inPanel a, inPanel b ]
    -- P と Q
    pP = curvePoint w (pxReal w) (pSign w)
    (mx0, my0) = envMouse env
    mx = mx0 + sw / 2
    my = sh / 2 - my0
    qP = curvePoint w ((mx - lx - lw / 2) / (lw / 7)) (if my < oy then 1 else -1)
    sumPart = case (pP, qP) of
      (Just p@(ppx, ppy), Just q) -> case addReal w p q of
        Just (x3, y3, lam) ->
          let r1 = toPanel (x3, -y3)
              r2 = toPanel (x3, y3)
              -- 直線をパネルいっぱいに伸ばす（上下はパネルの縁で切る）
              lineAt x = ppy + lam * (x - ppx)
              tA = clipX lam ppx ppy (-3.5) 3.5
              e1 = toPanel (fst tA, lineAt (fst tA))
              e2 = toPanel (snd tA, lineAt (snd tA))
          in [ seg 1.5 (rgba255 255 200 90 160) e1 e2 ]
             ++ (if inPanel r1 && inPanel r2 then dashed env (rgba255 255 120 200 200) r1 r2 else [])
             ++ (if inPanel r1 then [ dot env r1 5 amber, txt env (fst r1 + 10) (snd r1 + 18) 14 (gray 0.67) "3rd point" ] else [])
             ++ (if inPanel r2 then [ dot env r2 8 pink, txt env (fst r2 + 10) (snd r2 - 8) 16 pink "P + Q" ] else [])
        Nothing -> []
      _ -> []
    pointMark name col rad mp = case fmap toPanel mp of
      Just s | inPanel s -> [ dot env s rad col, txt env (fst s + 10) (snd s - 8) 16 col name ]
      _ -> []
    signed v = (if v < 0 then "- " else "+ ") ++ f2 (abs v)
    left =
      [ box env lx ly lw lh (rgb255 22 26 40)
      , seg 1 (rgb255 60 70 100) (lx, oy) (lx + lw, oy)
      , seg 1 (rgb255 60 70 100) (ox, ly) (ox, ly + lh)
      ]
      ++ curve ++ sumPart
      ++ pointMark "P" green' 7 pP
      ++ pointMark "Q" white 6 qP
      ++ [ txt env (lx + 14) (ly + 24) 15 (gray 0.78) ("y^2 = x^3 " ++ signed (ca w) ++ "x " ++ signed (cb w)) ]
      ++ [ txt env (lx + 14) (ly + 46) 15 (rgb255 255 110 110) "Singular (sharp point): unusable for crypto"
         | 4 * ca w * ca w * ca w + 27 * cb w * cb w == 0 ]

    -- ================= 右：mod p =================
    p = primeOf w
    rx = sw * 0.5
    ry = 80
    rw = sw * 0.48
    rh = sh * 0.62
    fp = fromIntegral p :: Float
    cs = min ((rw - 30) / fp) ((rh - 30) / fp)
    gx v = rx + 15 + fromIntegral v * cs + cs / 2
    gy v = ry + rh - 15 - fromIntegral v * cs - cs / 2
    gridP (a, b) = (gx (a :: Int), gy (b :: Int))
    mult k = join (multiples w V.!? (k - 1))   -- k·G（範囲外や O なら Nothing）
    cur = mult (kShow w)
    pA = mult (alice w)
    pB = mult (bob w)
    pS = mult (alice w * bob w `mod` max 1 (orderG w))
    trail =
      [ seg 1 (rgba255 255 200 90 120) (gridP a) (gridP b)
      | k <- [1 .. min (kShow w - 1) (V.length (multiples w) - 1)]
      , Just a <- [mult k], Just b <- [mult (k + 1)] ]
    marks col rad = maybe [] (\q -> [dot env (gridP q) rad col])
    ty = ry + rh + 26
    (gX, gY) = genG w
    right =
      box env rx ry rw rh (rgb255 22 26 40)
      : [ dot env (gridP q) (max 1.5 (cs * 0.3)) (rgb255 110 130 180) | q <- fpPoints w ]
      ++ trail
      ++ marks amber (max 4 (cs * 0.7)) cur
      ++ [ dot env (gridP (genG w)) (max 4 (cs * 0.6)) green' ]
      ++ marks (rgb255 255 140 160) (max 5 (cs * 0.75)) pA
      ++ marks (rgb255 130 180 255) (max 5 (cs * 0.75)) pB
      ++ maybe [] (\q -> [ at (toS env (gridP q)) (Color white (ring (max 8 (cs * 1.2)) 2)) ]) pS
      ++ [ txt env rx ty 14 (gray 0.78)
             ("y^2 = x^3 + 2x + 3 (mod " ++ show p ++ ")   " ++ show (length (fpPoints w) + 1) ++ " points (incl. infinity O)")
         , txt env rx (ty + 22) 14 (gray 0.78)
             ("G = (" ++ show gX ++ ", " ++ show gY ++ ")   order " ++ show (orderG w))
         , txt env rx (ty + 46) 14 amber
             (show (kShow w) ++ "*G = " ++ maybe "infinity O" showPt cur ++ "  <- regular steps, yet it jumps around")
         , txt env rx (ty + 72) 14 (rgb255 255 140 160)
             ("Alice secret a = " ++ show (alice w) ++ "  public A = a*G = " ++ ptStr pA)
         , txt env rx (ty + 96) 14 (rgb255 130 180 255)
             ("Bob   secret b = " ++ show (bob w) ++ "  public B = b*G = " ++ ptStr pB)
         , txt env rx (ty + 122) 14 (gray 0.94)
             ("Shared: a*B = b*A = (ab)*G = " ++ ptStr pS ++ " (white ring)")
         ]

ptStr :: FP -> String
ptStr = maybe "O" showPt

showPt :: (Int, Int) -> String
showPt (a, b) = "(" ++ show a ++ ", " ++ show b ++ ")"

-- 直線がパネルの上下（y = ±4.5）からはみ出さない x の範囲を求める
clipX :: Float -> Float -> Float -> Float -> Float -> (Float, Float)
clipX lam x0 y0 a b
  | abs lam < 1e-6 = (a, b)
  | otherwise =
      let x1 = x0 + (4.5 - y0) / lam
          x2 = x0 + (-4.5 - y0) / lam
      in (max a (min x1 x2), min b (max x1 x2))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure place
  EventKey (SpecialKey KeyUp) Down _ _    -> pure (fixP w { ca = min 2 (ca w + 0.25) })
  EventKey (SpecialKey KeyDown) Down _ _  -> pure (fixP w { ca = max (-3) (ca w - 0.25) })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (fixP w { cb = min 3 (cb w + 0.25) })
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure (fixP w { cb = max (-3) (cb w - 0.25) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { anim = not (anim w) }
  EventKey (Char '1') Down _ _ -> buildField 0 w
  EventKey (Char '2') Down _ _ -> buildField 1 w
  EventKey (Char '3') Down _ _ -> buildField 2 w
  EventKey (Char c) Down _ _ | c `elem` "nN" -> newSecrets w
  _ -> pure w
  where
    (lx, ly, lw, lh) = leftPanel env
    (mx0, my0) = envMouse env
    mx = mx0 + envW env / 2
    my = envH env / 2 - my0
    place
      | mx > lx && mx < lx + lw && fx w x >= 0 = w { pxReal = x, pSign = if my < ly + lh / 2 then 1 else -1 }
      | otherwise = w
      where x = (mx - lx - lw / 2) / (lw / 7)

-- 係数を変えて P が曲線から外れたら、左から順に探して置き直す
fixP :: World -> World
fixP w
  | fx w (pxReal w) >= 0 = w
  | otherwise = case [x | i <- [0 .. 139 :: Int], let x = -3.5 + 0.05 * fromIntegral i, fx w x >= 0] of
      (x : _) -> w { pxReal = x }
      [] -> w

-- ---------- やってみよう ----------
-- 1. Q を P に近づけると直線は接線になる（P + P = 2P）。真上・真下に並べると？（無限遠点 O）
-- 2. a = −3, b = 2 にすると曲線にとがった点ができる（特異）。4a³ + 27b² = 0 を確かめよう
-- 3. p = 251 で k·G の軌跡を見ると、次の点が予測できない。だから k を逆算するのは難しい
-- 4. 同じ安全さに必要な鍵の長さ：RSA 3072 ビット ≒ 楕円曲線 256 ビット。スマホや IC カードで使われる理由

-- ==================================================
-- ここから下は「道具」（mod p の点の足し算と、描画の部品）
-- ==================================================
invMod :: Int -> Int -> Int
invMod v p = go 1 (v `mod` p) (p - 2)
  where
    go :: Int -> Int -> Int -> Int
    go !r !_ 0 = r
    go !r !x e = go (if odd e then r * x `mod` p else r) (x * x `mod` p) (e `div` 2)

addFp :: Int -> FP -> FP -> FP      -- Nothing = 無限遠点 O
addFp _ Nothing q = q
addFp _ a Nothing = a
addFp p (Just (ax, ay)) (Just (bx, by))
  | ax == bx && (ay + by) `mod` p == 0 = Nothing
  | otherwise =
      let lam | ax == bx = (3 * ax * ax + fieldA) * invMod (2 * ay) p `mod` p
              | otherwise = (by - ay) * invMod (bx - ax) p `mod` p
          x3 = (lam * lam - ax - bx) `mod` p
      in Just (x3, (lam * (ax - x3) - ay) `mod` p)

-- 点線
dashed :: Env -> Color -> Point -> Point -> [Picture]
dashed env c (ax, ay) (bx, by) =
  let len = sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
      n = min 120 (floor (len / 12)) :: Int
      pt t = (ax + (bx - ax) * t, ay + (by - ay) * t)
      stepT = if len < 1 then 1 else 12 / len
  in [ Color c (thickLine 1.5 (toS env (pt t)) (toS env (pt (t + stepT / 2))))
     | i <- [0 .. n - 1], let t = fromIntegral i * stepT ]

dot :: Env -> Point -> Float -> Color -> Picture
dot env p r c = at (toS env p) (Color c (disc r))

f2 :: Float -> String
f2 = printf "%.2f"

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
