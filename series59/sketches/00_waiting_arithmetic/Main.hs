-- ============================================
-- 59-00 待ち合わせの算数 ─ 「2 人とも揃ったら出発」は max、「移動に t 分」は +
-- A さん（a 分）と B さん（b 分）が駅 1 に着く。揃ったら出発 = max(a, b)、t1 分で駅 2 へ。そこで C さん（c 分）と合流して t2 分でゴール
--   a ⊕ b = max(a, b)、a ⊗ t = a + t と書くと、ゴールの時刻 = ((a ⊕ b) ⊗ t1 ⊕ c) ⊗ t2 = a⊗t1⊗t2 ⊕ b⊗t1⊗t2 ⊕ c⊗t2（1 次式！）
--   ε = −∞ は「待たなくていい人（来ない人）」、e = 0 は「所要時間 0」
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps と書く
-- 操作：ドラッグ 到着時刻を動かす / ↑↓ t1 / ← → t2 / B・C その人を ε（来ない）にする / R ランダム
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

tMax :: Double
tMax = 100                 -- タイムラインの長さ（分）

data Who = WA | WB | WC deriving (Eq, Show)

data World = World
  { wA, wB, wC   :: !Double   -- ★ 3 人の到着時刻（分）。negInf にすると「来ない人」
  , wT1, wT2     :: !Double   -- ★ 駅 1 → 駅 2、駅 2 → ゴールの所要時間（分）
  , wDrag        :: !(Maybe Who)
  , wSavedB, wSavedC :: !Double
  , wTest        :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-00 Waiting arithmetic" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 12 20 45 18 10 Nothing 20 45 []

-- ★ 待ち合わせの計算（ふつうの書き方）：(駅 1 の出発, 駅 2 に着く, 駅 2 の出発, ゴール)
plainTrip :: Double -> Double -> Double -> Double -> Double -> (Double, Double, Double, Double)
plainTrip a b c t1 t2 =
  let dep1 = mpAdd a b                 -- 揃ったら出発（max）
      arr2 = mpMul dep1 t1             -- 移動（+）
      dep2 = mpAdd arr2 c              -- 駅 2 で C を待つ
  in (dep1, arr2, dep2, mpMul dep2 t2)

goalOf :: (Double, Double, Double, Double) -> Double
goalOf (_, _, _, g) = g

-- ★ 展開した 1 次式：a⊗t1⊗t2 ⊕ b⊗t1⊗t2 ⊕ c⊗t2（どの項がいちばん大きい？ = だれが全体を決める？）
expandedTerms :: Double -> Double -> Double -> Double -> Double -> [(String, Double)]
expandedTerms a b c t1 t2 = [("A", mpMul (mpMul a t1) t2), ("B", mpMul (mpMul b t1) t2), ("C", mpMul c t2)]

-- タイムラインの枠（p5 の tl）
tlG :: Env -> G
tlG env = G 140 120 (envW env - 200) 300 0 tMax 0 1

arrOf :: World -> Who -> Double
arrOf w k = case k of { WA -> wA w; WB -> wB w; WC -> wC w }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- タイムライン
  [ Color (grayC 60) (Line [(gX tl m, pY tl), (gX tl m, pY tl + pH tl)]) | m <- ticks ]
  ++ [ sayC (show (round m :: Int) ++ " min") (gX tl m) (pY tl + pH tl + 20) 14 (grayC 150) | m <- ticks ]
  ++ [ sayR nm (pX tl - 16) (r + 6) 16 (grayC 210) | (nm, r) <- zip ["Station 1", "Station 2", "Goal"] rows ]
  ++ [ Color (grayC 80) (Line [(pX tl, r), (pX tl + pW tl, r)]) | r <- rows ]
  -- 待ち時間（早く着いた人が待つ）と移動
  ++ [ wait (wA w) dep1 (row0 - 14) (rgbaC 255 170 70 120), wait (wB w) dep1 (row0 + 14) (rgbaC 110 190 255 120)
     , wait arr2 dep2 (row1 - 14) (rgbaC 255 220 120 120), wait (wC w) dep2 (row1 + 14) (rgbaC 150 230 160 120) ]
  ++ [ Color (grayC 240) (lineW 3 (gX tl dep1, row0) (gX tl arr2, row1)) | not (isEps dep1) ]
  ++ [ Color (grayC 240) (lineW 3 (gX tl dep2, row1) (gX tl goal, row2)) | not (isEps dep2) ]
  -- 人のマーカー（ε の人は左はしに薄く）
  ++ [ marker WA (row0 - 14) (255, 170, 70) "A", marker WB (row0 + 14) (110, 190, 255) "B", marker WC (row1 + 14) (150, 230, 160) "C" ]
  ++ concat [ [ Color (rgbC 255 230 120) (circleAt (gX tl v) y 12), say (s ++ " " ++ f v) (gX tl v + 10) (y + 27) 14 (rgbC 255 230 120) ]
            | (v, y, s) <- [(dep1, row0, "depart"), (dep2, row1, "depart"), (goal, row2, "arrive")], not (isEps v) ]
  -- ---- 式
  ++ [ say ("Depart station 1 = a (+) b = " ++ f (wA w) ++ " (+) " ++ f (wB w) ++ " = " ++ f dep1 ++ "   (max)") 40 ey 20 (grayC 240)
     , say ("Goal = ((a (+) b) (x) t1 (+) c) (x) t2 = ((" ++ f dep1 ++ " (x) " ++ f (wT1 w) ++ ") (+) " ++ f (wC w) ++ ") (x) " ++ f (wT2 w) ++ " = " ++ f goal) 40 (ey + 34) 20 (grayC 240)
     , say ("Expanded, a linear form: a(x)t1(x)t2 (+) b(x)t1(x)t2 (+) c(x)t2 = " ++ joinWith " (+) " (map (f . snd) terms) ++ " = " ++ f expanded) 40 (ey + 68) 18 (rgbC 255 220 120)
     , sayWrap (if null winWho then "Nobody comes -> the goal is eps too"
                else "The largest term is " ++ winWho ++ ": if " ++ winWho ++ " is 1 min late, the goal is 1 min late. Small delays of the others change nothing (coefficients are 0 or 1).")
               40 (ey + 86) (envW env - 80) 16 (grayC 200)
     , sayWrap "Ordinary -> max-plus:  plus + -> max (+)   times x -> plus (x)   0 -> eps = -inf (unit of (+))   1 -> e = 0 (unit of (x))"
               40 (ey + 136) (envW env - 80) 15 (grayC 160)
     , say ("Waiting arithmetic   t1 = " ++ f (wT1 w) ++ " min, t2 = " ++ f (wT2 w) ++ " min") 40 40 20 (grayC 240)
     , say "Drag arrival times   Up/Down t1   <- -> t2   B / C: that person is eps (absent)   R random" 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    tl = tlG env
    ticks = [0, 10 .. tMax]
    rows@[row0, row1, row2] = [pY tl + 40, pY tl + 150, pY tl + 260]
    res@(dep1, arr2, dep2, _) = plainTrip (wA w) (wB w) (wC w) (wT1 w) (wT2 w)
    goal = goalOf res
    terms = expandedTerms (wA w) (wB w) (wC w) (wT1 w) (wT2 w)
    expanded = foldl' mpAdd negInf (map snd terms)
    -- 最初に見つかったいちばん大きい項（同点なら先のもの）
    winWho = fst (foldl' (\(nm, v) (nm', v') -> if v' > v then (nm', v') else (nm, v)) ("", negInf) terms)
    f = mpFmt 0
    ey = pY tl + pH tl + 60
    wait from to y col
      | isEps from || isEps to || to <= from = Blank
      | otherwise = Color col (lineW 6 (gX tl from, y) (gX tl to, y))
    marker k y (r, g, b) lab =
      let v = arrOf w k
          x = markerX env w k
      in Pictures [ Color (if isEps v then rgbaC r g b 70 else rgbC r g b) (circleAt x y 26)
                  , sayM lab x y 14 (grayC 20)
                  , sayR (if isEps v then "eps" else f v ++ " min") (x - 18) (y + 5) 14 (grayC 220) ]

markerX :: Env -> World -> Who -> Float
markerX env w k = let tl = tlG env; v = arrOf w k in if isEps v then pX tl - 70 else gX tl v

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

-- マウスの近くの人
personAt :: Env -> World -> (Float, Float) -> Maybe Who
personAt env w (mx, my) = headOr Nothing [ Just k | (k, y) <- zip [WA, WB, WC] [ys + 26, ys + 54, ys + 164]
                                                  , let x = markerX env w k, sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int)) < 16 ]
  where ys = pY (tlG env)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wDrag = personAt env w (toP5 env p) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion p | Just k <- wDrag w ->
    let (mx, _) = toP5 env p
        v = clampv 0 (tMax - 10) (jsRound (realToFrac ((mx - 140) / (envW env - 200)) * tMax))
    in pure (case k of { WA -> w { wA = v }; WB -> w { wB = v }; WC -> w { wC = v } })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wT1 = min 40 (wT1 w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wT1 = max 0 (wT1 w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wT2 = min 40 (wT2 w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wT2 = max 0 (wT2 w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> pure (if isEps (wB w) then w { wB = wSavedB w } else w { wSavedB = wB w, wB = negInf })
    | c `elem` "cC" -> pure (if isEps (wC w) then w { wC = wSavedC w } else w { wSavedC = wC w, wC = negInf })
    | c `elem` "rR" -> do
        a <- randomRIO (0, 39 :: Int)
        b <- randomRIO (0, 39 :: Int)
        cc <- randomRIO (0, 69 :: Int)
        pure w { wA = fromIntegral a, wB = fromIntegral b, wC = fromIntegral cc }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 59・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("nested ((a(+)b)(x)t1 (+) c)(x)t2 = expanded linear form (400 cases, with eps)", all (\(s, _, _, _) -> s) runs)
  , ("eps is the unit of (+) (a (+) eps = a), absorbing for (x) (a (x) eps = eps), e = 0 is the unit of (x)"
    , all (\a -> mpAdd a negInf == a && isEps (mpMul a negInf) && mpMul a 0 == a) [-3, 0, 7] && isEps (mpMul negInf negInf))
  , ("no NaN ever (also (x) and (+) of eps with eps)", all (\(_, n, _, _) -> n) runs && noNaN [mpAdd negInf negInf, mpMul negInf 5])
  , ("the goal time always equals one of the terms (someone decides it)", all (\(_, _, c, _) -> c) runs)
  , ("if A is 1 min late, the goal is 0 to 1 min late (never more)", all (\(_, _, _, l) -> l) runs)
  , ("eps is shown as \"eps\" (never -Infinity)", mpFmt 1 negInf == "eps" && mpFmt 1 (-0.04) == "0") ]
  where
    runs = withSeed 59 (mapM (const one) [1 .. 400 :: Int])
    vals = do u <- rnd
              if u < 0.2 then pure negInf else fromIntegral <$> rndInt (-20) 60
    one = do
      a <- vals; b <- vals; c <- vals
      s <- fromIntegral <$> rndInt 0 30
      u <- fromIntegral <$> rndInt 0 30
      let g1 = goalOf (plainTrip a b c s u)
          ts = map snd (expandedTerms a b c s u)
          g2 = foldl' mpAdd negInf ts
          same = g1 == g2
          noN = noNaN [g1, g2]
          crit = isEps g1 || any (== g1) ts
          lip = isEps a || (let g3 = goalOf (plainTrip (a + 1) b c s u) in g3 >= g1 && g3 <= g1 + 1)
      pure (same, noN, crit, lip)

-- ---------- やってみよう ----------
-- 1. A さんをドラッグして、B さんより遅くしたり早くしたりしよう。駅 1 の出発時刻はどう変わる？
-- 2. C さんがとても遅いとき、A や B が遅れてもゴールは変わらない。なぜ？（max の中で負けている項）
-- 3. B キーで B さんを ε（来ない）にすると、式のどこが消える？ ε ⊕ a = a を確かめよう
-- 4. 3 つ目の駅と D さんを足して（World に wD、plainTrip に引数を 1 つ）、式を 1 次式に展開してみよう
-- 5. 06・07 では、この「待ち合わせ」を行列でまとめて、列車や工場のダイヤを計算する

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine
-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
