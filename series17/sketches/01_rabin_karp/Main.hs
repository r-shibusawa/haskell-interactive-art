-- ============================================
-- 01 ラビン–カープ法
-- 窓の中の文字列を 1 つの数（ハッシュ値）にして比べる。窓を 1 つずらすときは
-- 「左の 1 文字を引いて、右の 1 文字を足す」だけ（ローリングハッシュ）
-- ハッシュが同じでも中身が違う「にせの一致（spurious hit）」は 1 文字ずつ確かめる
-- 操作：キーで入力 / Tab = テキスト⇄パターン / Backspace / スペース = 再生・停止
--       → = 1 つずらす / ← = お手本を切りかえ / ↑↓ = 法 q を変える / Enter = はじめから
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit, digitToInt, ord)
import Numeric (showFFloat)

primes :: [Int]
primes = [7, 11, 13, 31, 101, 997]   -- ★ 法 q の候補（小さいほどにせの一致が増える）

speed :: Int
speed = 40              -- ★ 何フレームに 1 つずらすか

data Kind = Miss | Hit | Spurious deriving Eq

data World = World
  { wT        :: !String
  , wP        :: !String
  , wEditT    :: !Bool
  , wQi       :: !Int
  , wAuto     :: !Bool
  , wPreset   :: !Int
  , wD        :: !Int
  , wQ        :: !Int
  , wH        :: !Int                -- h = d^(m−1) mod q
  , wPh       :: !Int                -- パターンのハッシュ p
  , wTh       :: !Int                -- 窓のハッシュ t
  , wS        :: !Int                -- 窓の位置
  , wHistory  :: ![(Int, Int, Kind)] -- (s, t, 結果)。古い順
  , wFinished :: !Bool
  , wHits     :: !Int
  , wSpurious :: !Int
  , wChars    :: !Int                -- 文字の比較回数
  }

main :: IO ()
main = runSketch (sketch "17-01 Rabin-Karp" initialWorld)
  { skBack  = cBg
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` speed == 0 then slide w else w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = restart (World "6142718281828459045" "27182" False 2 True 1 10 13 1 0 0 0 [] False 0 0 0)

-- 数字だけなら 10 進数（手計算しやすい）、それ以外は文字コード（256 進数）
allDigits :: World -> Bool
allDigits w = all isDigit (wT w ++ wP w)

val :: World -> Char -> Int
val w c = if allDigits w then (if isDigit c then digitToInt c else 0) else ord c

valAt :: World -> String -> Int -> Int
valAt w s i = maybe 0 (val w) (charAt s i)

qOf :: Int -> Int
qOf i = case drop i primes of
  (q : _) -> q
  [] -> 13

restart :: World -> World
restart w0 =
  let d = if allDigits w0 then 10 else 256
      q = qOf (wQi w0)
      m = length (wP w0)
      n = length (wT w0)
      finished = m == 0 || m > n
      w = w0 { wD = d, wQ = q, wHistory = [], wHits = 0, wSpurious = 0, wChars = 0, wFinished = finished }
      h = foldl (\a _ -> a * d `mod` q) 1 [1 .. m - 1]    -- h = d^(m−1) mod q
      hash s = foldl (\a c -> (d * a + val w c) `mod` q) 0 (take m s)   -- 前処理
  in if finished then w else check w { wH = h, wPh = hash (wP w), wTh = hash (wT w), wS = 0 }

-- いまの窓 s を調べる
check :: World -> World
check w
  | wTh w == wPh w =
      let m = length (wP w)
          same = takeWhile id (zipWith (==) (drop (wS w) (wT w)) (wP w))
          k = length same
          ok = k == m
      in w { wChars = wChars w + (if ok then m else k + 1)
           , wHits = wHits w + (if ok then 1 else 0)
           , wSpurious = wSpurious w + (if ok then 0 else 1)
           , wHistory = wHistory w ++ [(wS w, wTh w, if ok then Hit else Spurious)] }
  | otherwise = w { wHistory = wHistory w ++ [(wS w, wTh w, Miss)] }

-- ★ 窓を 1 つずらす（ローリングハッシュ）
slide :: World -> World
slide w
  | wFinished w = w
  | wS w >= n - m = w { wFinished = True }
  | otherwise =
      let t' = (wD w * (wTh w - valAt w (wT w) (wS w) * wH w) + valAt w (wT w) (wS w + m)) `mod` wQ w   -- 左を引いて、右を足す
      in check w { wTh = t', wS = wS w + 1 }
  where n = length (wT w)
        m = length (wP w)

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      t = wT w
      p = wP w
      n = length t
      m = length p
      cw = clampF 16 52 ((ww - 80) / fromIntegral (max n 1))
      x0 = (ww - cw * fromIntegral n) / 2
      yT = hh * 0.26
      xAt i = x0 + fromIntegral (i :: Int) * cw
      hist = wHistory w
      textCell i =
        let hit = any (\(s, _, k) -> k == Hit && i >= s && i < s + m) hist
        in Pictures
             [ cell env (if hit then cGreen else cPanel) (if wEditT w then cOrchid else cEdge) (xAt i) yT (cw - 2) (cw - 2)
             , txtC env (xAt i + cw / 2 - 1) (yT + cw / 2) (max 14 (cw * 0.55)) cFg (chr1 t i) ]
      windowPart = case reverse hist of
        ((_, _, kind) : _) | m > 0 && m <= n ->
          let col = case kind of { Hit -> cMint; Spurious -> cBad; Miss -> cGold }
              s = wS w
              verdict = case kind of
                Hit -> "Match! (contents equal too)"
                Spurious -> "Spurious hit! (text differs)"
                Miss -> "differs from p: skip"
          in Pictures
               [ outline env col 3 (xAt s - 3) (yT - 3) (cw * fromIntegral m + 4) (cw + 4)
               , txt env 30 (yT + cw + 30) 20 cOrchid ("P = \"" ++ p ++ "\"   ->   p = " ++ show (wPh w))
               , txt env 30 (yT + cw + 62) 18 col ("Window T[" ++ show s ++ ".." ++ show (s + m - 1) ++ "] = \""
                   ++ take m (drop s t) ++ "\"  ->  t = " ++ show (wTh w) ++ "   " ++ verdict)
               , if s < n - m
                   then txt env 30 (yT + cw + 98) 15 cDim ("Next: t' = (d*(t - T[s]*h) + T[s+m]) mod q = (" ++ show (wD w) ++ "*("
                          ++ show (wTh w) ++ " - " ++ show (valAt w t s) ++ "*" ++ show (wH w) ++ ") + "
                          ++ show (valAt w t (s + m)) ++ ") mod " ++ show (wQ w))
                   else Blank ]
        _ -> Blank
      -- ハッシュ値の列
      yH = hh * 0.62
      histCell (s, v, kind) = Pictures
        [ box env (case kind of { Hit -> cGreen; Spurious -> cRed; Miss -> cPanel }) (xAt s) yH (cw - 2) (cw - 2)
        , txtC env (xAt s + cw / 2 - 1) (yH + cw / 2) (max 14 (min (cw * 0.42) 16)) (if kind == Miss then cDim else cFg) (show v) ]
      yB = hh * 0.82
  in Pictures
       [ txt env 30 24 22 cFg "Rabin-Karp: turn the window into a number"
       , txt env 30 56 14 cDim ("Editing: " ++ (if wEditT w then "text T" else "pattern P") ++ " (Tab)   Up/Down q = " ++ show (qOf (wQi w))
           ++ "   d = " ++ show (wD w) ++ "   Space play/stop   Right slide   Left examples")
       , Pictures (map textCell [0 .. n - 1])
       , windowPart
       , txt env x0 (yH - 26) 14 cDim "Hash t for each shift s (green = match, red = spurious)"
       , Pictures (map histCell hist)
       , txt env 30 yB 18 cFg ("Matches " ++ show (wHits w) ++ "   Spurious " ++ show (wSpurious w) ++ "   Char compares " ++ show (wChars w)
           ++ " (naive: up to " ++ show (max 0 ((n - m + 1) * m)) ++ ")")
       , txt env 30 (yB + 30) 14 cDim "A small q makes hashes collide, so more spurious hits. With a large prime they are rare."
       , if m == 0 then txt env 30 (yB + 56) 14 cBad "The pattern is empty." else Blank
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey k Down _ _) w = pure (onKey k w)
onEvent _ _ w = pure w

onKey :: Key -> World -> World
onKey k w
  | k == SpecialKey KeySpace = w { wAuto = not (wAuto w) }
  | k == SpecialKey KeyRight = slide w
  | k == SpecialKey KeyLeft = preset (wPreset w `mod` 3 + 1) w
  | k == SpecialKey KeyUp = restart w { wQi = min (wQi w + 1) (length primes - 1) }
  | k == SpecialKey KeyDown = restart w { wQi = max (wQi w - 1) 0 }
  | isEnter k = restart w
  | isTab k = w { wEditT = not (wEditT w) }
  | isBack k = restart (if wEditT w then w { wT = dropLast (wT w) } else w { wP = dropLast (wP w) })
-- 数字も文字として入力するので、お手本の切りかえは ← キー
onKey (Char c) w
  | printable c = restart (typed c)
  where
    typed ch
      | wEditT w = if length (wT w) < 32 then w { wT = wT w ++ [ch] } else w
      | otherwise = if length (wP w) < 10 then w { wP = wP w ++ [ch] } else w
onKey _ w = w

preset :: Int -> World -> World
preset k w = restart $ case k of
  1 -> w' { wT = "6142718281828459045", wP = "27182", wQi = 2 }   -- q = 13 だとにせの一致が出る
  2 -> w' { wT = "ABRACADABRA_ABRACADABRA", wP = "CAD", wQi = 0 }
  _ -> w' { wT = "TOBEORNOTTOBETHATIS", wP = "TOBE", wQi = 1 }
  where w' = w { wPreset = k }

-- ---------- やってみよう ----------
-- 1. ↓ で q を 7 にして、にせの一致がいくつ増えるか数える
-- 2. primes に 3 や 5 を足してみる。文字の比較回数はどうなる？
-- 3. check の中で中身を確かめるのをやめると、どんなまちがいが起きるか考えよう

-- ---------- 道具：キーと文字列 ----------
isTab, isEnter, isBack :: Key -> Bool
isTab k = k == SpecialKey KeyTab || k == Char '\t'
isEnter k = k == SpecialKey KeyEnter || k == Char '\r' || k == Char '\n'
isBack k = k `elem` [SpecialKey KeyBackspace, SpecialKey KeyDelete, Char '\b', Char '\DEL']

-- 文字として入力できるキーか
printable :: Char -> Bool
printable c = c > ' ' && c <= '~'

-- i 文字目（範囲外は Nothing）
charAt :: String -> Int -> Maybe Char
charAt s i
  | i < 0 = Nothing
  | otherwise = case drop i s of
      (c : _) -> Just c
      [] -> Nothing

-- 1 文字の文字列（範囲外は空）
chr1 :: String -> Int -> String
chr1 s i = maybe "" (: []) (charAt s i)

dropLast :: String -> String
dropLast s = take (length s - 1) s

-- マス（枠つきの四角）
cell :: Env -> Color -> Color -> Float -> Float -> Float -> Float -> Picture
cell env fc sc x y w h = Pictures [box env fc x y w h, Color sc (rectLine (gx env (x + w / 2)) (gy env (y + h / 2)) w h)]

-- ---------- 道具：色 ----------
cBg, cPanel, cFg, cDim, cOrchid, cMint, cGold, cBad, cEdge, cGreen, cRed, cAmber, cBlue :: Color
cBg     = rgb255 0x14 0x10 0x1f
cPanel  = rgb255 0x22 0x1a 0x33
cFg     = rgb255 0xe9 0xe4 0xf5
cDim    = rgb255 0x8c 0x84 0xa3
cOrchid = rgb255 0xf0 0xa3 0xff
cMint   = rgb255 0x6f 0xe3 0xd0
cGold   = rgb255 0xff 0xcf 0x6b
cBad    = rgb255 0xff 0x6b 0x81
cEdge   = rgb255 0x3a 0x2f 0x52
cGreen  = rgb255 0x2f 0x5e 0x57
cRed    = rgb255 0x5e 0x2a 0x3a
cAmber  = rgb255 0x5a 0x4a 0x1f
cBlue   = rgb255 0x2f 0x4a 0x5e

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置を p5.js の座標で
mouse :: Env -> Point
mouse env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の枠
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (pt env) [(x - t / 2, y), (x + w + t / 2, y), (x + w, y), (x + w, y + h), (x + w + t / 2, y + h), (x - t / 2, y + h), (x, y + h), (x, y)]))

-- 太さ t の線分（p5.js の line）と折れ線
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

path :: Env -> Color -> Float -> [Point] -> Picture
path env c t ps = Color c (thickPath t (map (pt env) ps))

-- 点線
dashed :: Env -> Color -> Float -> Float -> Point -> Point -> Picture
dashed env c t d a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      k = floor (l / (2 * d)) :: Int
      f s = (x1 + (x2 - x1) * s / max l 1e-6, y1 + (y2 - y1) * s / max l 1e-6)
  in Pictures [seg env c t (f (2 * d * fromIntegral i)) (f (min l (2 * d * fromIntegral i + d))) | i <- [0 .. k]]

-- 直径 d の円（p5.js の circle）と、太さ t の輪
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

hoop :: Env -> Color -> Float -> Point -> Float -> Picture
hoop env c t p d = Color c (at (pt env p) (ring (d / 2) t))

-- 塗りつぶしの多角形（凸形のみ）
fillPoly :: Env -> Color -> [Point] -> Picture
fillPoly env c ps = Color c (Polygon (map (pt env) ps))

-- 文字：txt は左上が (x, y)、txtM は左端で上下中央、txtC は中心が (x, y)
txt, txtM, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env (y + s)) s c str
txtM env x y s c str = label (gx env x) (gy env (y + s / 2)) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 小数点以下 d 桁（JavaScript の toFixed）
fix :: Int -> Float -> String
fix d v = showFFloat (Just d) v ""

