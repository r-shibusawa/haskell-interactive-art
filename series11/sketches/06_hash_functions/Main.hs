-- ============================================
-- 06 よいハッシュ関数とは
-- よいハッシュ関数 = キーをマスに「均等に」ばらまく。キーにかたよった規則があっても崩れないことが大切
-- 割り算法 h(k) = k mod m：m が 2 のべき乗だと、下のビットしか見ないので規則的なキーに弱い
-- かけ算法 h(k) = ⌊m (kA mod 1)⌋：m は何でもよい
-- ユニバーサル・ハッシュ h(k) = ((ak + b) mod p) mod m：a, b を毎回ランダムに選ぶ → どんな悪いキーの並びにも平均では強い
-- 操作：1 = 割り算法 / 2 = かけ算法 / 3 = ユニバーサル（R で a, b を引き直す）
--       4 = ランダムなキー / 5 = 16 の倍数のキー / 6 = 連番のキー / ↑↓ = m / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Bits ((.&.))
import Data.Char (digitToInt, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Numeric (showFFloat)

firstM :: Int
firstM = 16        -- ★ マスの数（16 と 17 で割り算法をくらべよう）

nKeys :: Int
nKeys = 240        -- ★ キーの個数

primeP :: Int
primeP = 1000003   -- ユニバーサル・ハッシュで使う素数（キーより大きい）

constA :: Double
constA = (sqrt 5 - 1) / 2

-- | 落ちてくるキー：行き先のマス、進み具合 t、出発の x
data Drop = Drop { dSlot :: !Int, dT :: !Float, dX0 :: !Float }

data World = World
  { m      :: !Int
  , a      :: !Int
  , b      :: !Int
  , method :: !Int
  , keySet :: !Int
  , keys   :: [Int]               -- まだ落としていないキー
  , placed :: !Int
  , counts :: !(IM.IntMap Int)
  , drops  :: [Drop]
  , paused :: !Bool
  }

initialWorld :: World
initialWorld = World firstM 1 0 1 5 [] 0 IM.empty [] False

-- | a, b を引き直す
drawAB :: World -> IO World
drawAB w = do
  a' <- randomRIO (1, primeP - 1)
  b' <- randomRIO (0, primeP - 1)
  pure w { a = a', b = b' }

hashOf :: World -> Int -> Int
hashOf w k = case method w of
  1 -> k `mod` m w
  2 -> let v = fromIntegral k * constA
       in floor (fromIntegral (m w) * (v - fromIntegral (floor v :: Int)))
  _ -> ((a w * k + b w) `mod` primeP) `mod` m w   -- Int は 64 ビットなので、この大きさのかけ算は正確

makeKeys :: Int -> IO [Int]
makeKeys set = case set of
  4 -> replicateM nKeys (randomRIO (1, 99999))
  5 -> pure [16 * (i + 1) | i <- [0 .. nKeys - 1]]
  _ -> pure [1000 + i | i <- [0 .. nKeys - 1]]

restart :: World -> IO World
restart w = do
  ks <- makeKeys (keySet w)
  pure w { keys = ks, placed = 0, counts = IM.empty, drops = [] }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  -- キーを 1 フレームに 2 個ずつ落とす
  w1 <- if paused w0 then pure w0 else foldM (\w _ -> dropOne env w) w0 [1, 2 :: Int]
  let moved = [d { dT = dT d + 0.04 } | d <- drops w1]
      (done, flying) = (filter ((>= 1) . dT) moved, filter ((< 1) . dT) moved)
      counts' = foldl' (\acc d -> IM.insertWith (+) (dSlot d) 1 acc) (counts w1) done
  pure w1 { drops = flying, counts = counts' }

dropOne :: Env -> World -> IO World
dropOne env w = case keys w of
  [] -> pure w
  (k : ks) -> do
    x0 <- randomRIO (envW env * 0.1, envW env * 0.9)
    pure w { keys = ks, placed = placed w + 1, drops = Drop (hashOf w k) 0 x0 : drops w }

methodName :: Int -> String
methodName i = case i of
  1 -> "division  k mod m"
  2 -> "multiplication  floor(m(kA mod 1))"
  _ -> "universal  ((ak+b) mod p) mod m"

setName :: Int -> String
setName i = case i of
  4 -> "random"
  5 -> "multiples of 16 (16, 32, 48, ...)"
  _ -> "consecutive (1000, 1001, ...)"

draw :: Env -> World -> Picture
draw env w =
  let bx = 60
      bw = (envW env - 120) / fi (m w)
      base = envH env - 150
      cnt i = IM.findWithDefault 0 i (counts w)
      vals = map cnt [0 .. m w - 1]
      maxC = maximum (0 : vals)
      ideal = fi (placed w) / fi (m w) :: Float
      mx = maximum [1, fi maxC, ideal * 2]
      bar i =
        let c = cnt i
            hgt = (fi c / mx) * (base - 180)
        in Pictures
             [ box env (bx + fi i * bw + 2) (base - hgt) (bw - 4) hgt 3 (if fi c > ideal * 1.8 then rgb255 255 110 110 else rgb255 120 170 255)
             , txtC env (bx + (fi i + 0.5) * bw) (base + 14) 14 (gray 0.6) (show i)
             ]
      dropPic d =
        let x = lerpF (dX0 d) (bx + (fi (dSlot d) + 0.5) * bw) (dT d)
            y = lerpF 140 base (dT d * dT d)
        in circ env x y 3.5 (rgba255 255 200 120 200)
      iy = base - (ideal / mx) * (base - 180)
      -- 衝突の数：同じマスのペアの数 Σ C(c, 2)
      coll = sum [c * (c - 1) `div` 2 | c <- vals]
      expect = fi (placed w * (placed w - 1)) / fi (2 * m w) :: Float
      empties = length (filter (== 0) vals)
      power2 = m w .&. (m w - 1) == 0
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "What Makes a Good Hash Function"
         , txt env 30 60 14 (rgb255 170 170 200) "1 division  2 multiplication  3 universal (R redraws a, b)  4-6 key sets  Up/Down m  Space pause"
         , txt env 30 82 16 (gray 1) ("Hash: " ++ methodName (method w) ++ "   m = " ++ show (m w) ++ (if power2 then " (a power of 2)" else ""))
         , txt env 30 104 16 (gray 1) ("Keys: " ++ setName (keySet w))
         , if method w == 3 then txt env 30 126 14 (gray 0.67) ("a = " ++ show (a w) ++ ", b = " ++ show (b w) ++ ", p = " ++ show primeP) else Blank
         ]
         ++ map dropPic (drops w)
         ++ map bar [0 .. m w - 1]
         ++ [ seg env 2 (rgb255 255 225 140) bx iy (envW env - 60) iy
            , txtR env (envW env - 60) (iy - 22) 14 (rgb255 255 225 140) ("Ideal (even) = " ++ fmt 1 ideal ++ " keys")
            , txt env 30 (envH env - 100) 16 (gray 0.86)
                ("Colliding pairs " ++ show coll ++ " (random: about " ++ show (round expect :: Int) ++ ")   Empty slots "
                 ++ show empties ++ " / " ++ show (m w) ++ "   Fullest slot " ++ show maxC)
            , txt env 30 (envH env - 66) 18 (rgb255 255 225 140) (verdict w coll expect empties)
            ] )

verdict :: World -> Int -> Float -> Int -> String
verdict w coll expect empties
  | placed w < 20 = "Inserting keys..."
  | fi coll > expect * 2 || fi empties > fi (m w) / 2 = "X  Skewed! Keys pile up in some slots (searching gets slow)"
  | otherwise = "O  Almost evenly spread"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onKey :: Key -> World -> IO World
onKey key w = case key of
  Char c
    | c >= '1' && c <= '3' -> do
        let w1 = w { method = digitToInt c }
        w2 <- if method w1 == 3 then drawAB w1 else pure w1
        restart w2
    | c >= '4' && c <= '6' -> restart w { keySet = digitToInt c }
    | c == 'r' -> drawAB w >>= restart
  SpecialKey KeyUp -> restart w { m = min 40 (m w + 1) }
  SpecialKey KeyDown -> restart w { m = max 2 (m w - 1) }
  SpecialKey KeySpace -> pure w { paused = not (paused w) }
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-06 Good Hash Functions" initialWorld)
  { skBack  = rgb255 16 16 24
  , skStart = \_ -> drawAB initialWorld >>= restart
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. 1（割り算法）+ 5（16 の倍数）+ m = 16 → 全部 0 番へ！ ↑ で m = 17（素数）にすると？
-- 2. 同じ 16 の倍数でも 2（かけ算法）や 3（ユニバーサル）なら均等になる
-- 3. 実際のソフトでは、パスワードなどは暗号学的ハッシュ（SHA-256）、表には高速なハッシュ（xxHash や SipHash など）を使い分ける


-- ==================================================
-- ここから下は「画面の道具」です
-- p5.js と同じ「左上が原点、y は下向き」の座標で考えて、ここで gloss の座標に直します
-- ==================================================
fi :: Int -> Float
fi = fromIntegral

scr :: Env -> Float -> Float -> Point
scr env x y = (x - envW env / 2, envH env / 2 - y)

-- | 左上をそろえた文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y sz c s = let (px, py) = scr env x (y + sz) in label px py sz c s

-- | 中央をそろえた文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y sz c s = let (px, py) = scr env x y in labelC px py sz c s

-- | 右上をそろえた文字
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y sz c s = txt env (x - fi (length s) * sz * 0.72) y sz c s

-- | 塗りつぶした円（中心と半径）
circ :: Env -> Float -> Float -> Float -> Color -> Picture
circ env x y r c = at (scr env x y) (Color c (disc r))

-- | 太さのある線分
seg :: Env -> Float -> Color -> Float -> Float -> Float -> Float -> Picture
seg env sw c x1 y1 x2 y2 = Color c (thickLine sw (scr env x1 y1) (scr env x2 y2))

-- | 角の丸い長方形のかど（左上の x, y、幅、高さ、丸みの半径）
roundCorners :: Float -> Float -> Float -> Float -> Float -> [(Float, Float)]
roundCorners x y w h r0 =
  let r = max 0 (minimum [r0, w / 2, h / 2])
      arcAt cx cy a0 = [(cx + r * cos a, cy + r * sin a) | i <- [0 .. 3 :: Int], let a = a0 + fi i * pi / 6]
  in arcAt (x + w - r) (y + r) (-pi / 2) ++ arcAt (x + w - r) (y + h - r) 0
     ++ arcAt (x + r) (y + h - r) (pi / 2) ++ arcAt (x + r) (y + r) pi

-- | 塗りつぶした角丸の長方形（p5.js の rect と同じく左上で指定）
box :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h r c = Color c (Polygon [scr env px py | (px, py) <- roundCorners x y w h r])

-- | 小数 n 桁の文字列（p5.js の nf に相当）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

-- | キーをそろえる：英字は小文字に。Enter・Backspace・スペースは環境によって来かたが違うので 1 つにまとめる
normKey :: Key -> Key
normKey k = case k of
  Char ' ' -> SpecialKey KeySpace
  Char '\r' -> SpecialKey KeyEnter
  Char '\n' -> SpecialKey KeyEnter
  Char '\b' -> SpecialKey KeyBackspace
  Char '\DEL' -> SpecialKey KeyBackspace
  SpecialKey KeyDelete -> SpecialKey KeyBackspace
  SpecialKey KeyPadEnter -> SpecialKey KeyEnter
  Char c -> Char (toLower c)
  _ -> k
