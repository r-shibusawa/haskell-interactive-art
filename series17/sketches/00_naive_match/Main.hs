-- ============================================
-- 00 素朴な文字列照合
-- パターン P を 1 文字ずつずらしながら、テキスト T と左から 1 文字ずつ比べる
-- 比べた回数を数えると、「いちばん悪い場合」は (n − m + 1) × m 回になる
-- 操作：キーで文字を入力 / Tab = テキスト⇄パターンの切りかえ / Backspace = 1 文字消す
--       スペース = 自動再生・停止 / → = 1 回だけ比べる / Enter = はじめから / 1〜4 = お手本
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Numeric (showFFloat)

speed :: Int
speed = 8               -- ★ 何フレームに 1 回比べるか（小さいほど速い）

data World = World
  { wT        :: !String
  , wP        :: !String
  , wEditT    :: !Bool                  -- いま入力している方（True = T、False = P）
  , wAuto     :: !Bool
  , wS        :: !Int                   -- ずらし量
  , wJ        :: !Int                   -- パターンの何文字目を比べているか
  , wComps    :: !Int                   -- 比べた回数
  , wFound    :: ![Int]                 -- 見つかった位置
  , wPerShift :: !(IM.IntMap Int)       -- ずらし量ごとの比較回数
  , wLast     :: !(Maybe (Int, Int, Bool))  -- 直前に比べた場所 (i, j, 一致したか)
  , wFinished :: !Bool
  }

main :: IO ()
main = runSketch (sketch "17-00 Naive string matching" initialWorld)
  { skBack  = cBg
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` speed == 0 then compareOnce w else w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = restart (World "ABRACADABRA_ABRACADABRA" "ABRA" False True 0 0 0 [] IM.empty Nothing False)

restart :: World -> World
restart w = w
  { wS = 0, wJ = 0, wComps = 0, wFound = [], wPerShift = IM.empty, wLast = Nothing
  , wFinished = null (wP w) || length (wP w) > length (wT w) }

-- ★ 比較 1 回ぶん
compareOnce :: World -> World
compareOnce w
  | wFinished w = w
  | otherwise =
      let n = length (wT w)
          m = length (wP w)
          s = wS w
          j = wJ w
          ok = charAt (wT w) (s + j) == charAt (wP w) j
          (s', j', found)
            | ok && j + 1 == m = (s + 1, 0, wFound w ++ [s])    -- 全部一致 → 見つけた！次のずらしへ
            | ok = (s, j + 1, wFound w)
            | otherwise = (s + 1, 0, wFound w)                  -- 1 文字でも違えば、1 つずらしてやり直し
      in w { wS = s', wJ = j', wFound = found, wComps = wComps w + 1
           , wPerShift = IM.insertWith (+) s 1 (wPerShift w)
           , wLast = Just (s + j, j, ok)
           , wFinished = s' > n - m }

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      t = wT w
      p = wP w
      n = length t
      m = length p
      cw = clampF 14 48 ((ww - 80) / fromIntegral (max n 1))
      x0 = (ww - cw * fromIntegral n) / 2
      yT = hh * 0.3
      yP = yT + cw * 1.4
      xAt i = x0 + fromIntegral i * cw
      fs = max 14 (cw * 0.6)
      found = wFound w
      finished = wFinished w
      textCell i =
        let hit = any (\f -> i >= f && i < f + m) found
        in Pictures
             [ cell env (if hit then cGreen else cPanel) (if wEditT w then cOrchid else cEdge) (xAt i) yT (cw - 2) (cw - 2)
             , txtC env (xAt i + cw / 2 - 1) (yT + cw / 2) fs cFg (chr1 t i)
             , if i `mod` 5 == 0 then txtC env (xAt i + cw / 2) (yT - 12) 14 cDim (show i) else Blank ]
      -- パターン（いまのずらし量 s の位置）
      ss = min (wS w) (n - m)
      patCell k =
        let c | not finished, Just (li, lj, False) <- wLast w, li - lj == ss, k == lj = cRed
              | not finished && k < wJ w = cGreen
              | otherwise = cPanel
        in Pictures
             [ cell env c (if wEditT w then cEdge else cOrchid) (xAt (ss + k)) yP (cw - 2) (cw - 2)
             , txtC env (xAt (ss + k) + cw / 2 - 1) (yP + cw / 2) fs cOrchid (chr1 p k) ]
      pattern'
        | m > 0 && m <= n =
            Pictures (map patCell [0 .. m - 1] ++
              [ -- いま比べている文字を線で示す
                if finished then Blank
                else let cx = xAt (wS w + wJ w) + cw / 2 in seg env cGold 2 (cx, yT + cw) (cx, yP - 2) ])
        | otherwise = Blank
      -- ずらし量ごとの比較回数（棒グラフ）
      statsY = yP + cw + 40
      gyy = hh - 50
      gh = max 40 (gyy - statsY - 110)
      bar (k, v) =
        let h = fromIntegral v / fromIntegral (max m 1) * gh
        in box env (if k `elem` found then cMint else cOrchid) (xAt k + cw * 0.2) (gyy - h) (cw * 0.6) h
      worst = max 0 ((n - m + 1) * m)
  in Pictures
       [ txt env 30 24 22 cFg "Naive string matching: find P inside T"
       , txt env 30 56 14 cDim ("Editing: " ++ (if wEditT w then "text T" else "pattern P")
           ++ " (Tab)   Space play/stop   Right 1 compare   Enter restart   1-4 examples")
       , Pictures (map textCell [0 .. n - 1])
       , pattern'
       , seg env cEdge 1 (x0, gyy) (x0 + cw * fromIntegral n, gyy)
       , Pictures (map bar (IM.toList (wPerShift w)))
       , txt env x0 (gyy + 8) 14 cDim "Comparisons per shift s (green = found)"
       , txt env 30 statsY 18 cFg ("n = " ++ show n ++ "   m = " ++ show m ++ "   comparisons = " ++ show (wComps w)
           ++ "   worst case (n-m+1)*m = " ++ show worst)
       , txt env 30 (statsY + 30) 18 cMint ("Found at: " ++ (if null found then "-" else intercalate ", " (map show found))
           ++ (if finished then "   (done)" else ""))
       , if m == 0 then txt env 30 (statsY + 60) 18 cBad "The pattern is empty. Type some characters." else Blank
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey k Down _ _) w = pure (onKey k w)
onEvent _ _ w = pure w

onKey :: Key -> World -> World
onKey k w
  | k == SpecialKey KeySpace = w { wAuto = not (wAuto w) }
  | k == SpecialKey KeyRight = compareOnce w
  | isEnter k = restart w
  | isTab k = w { wEditT = not (wEditT w) }
  | isBack k = restart (if wEditT w then w { wT = dropLast (wT w) } else w { wP = dropLast (wP w) })
onKey (Char c) w = case c of
  '1' -> example "ABRACADABRA_ABRACADABRA" "ABRA"
  '2' -> example "AAAAAAAAAAAAAAAAAAAAAAAB" "AAAAB"   -- いちばん悪い場合
  '3' -> example "ACGTTACGACGTACGGACGTAC" "ACGT"      -- DNA
  '4' -> example "sumomomomomomomonouchi" "momo"      -- 早口ことば（重なって見つかる）
  _ | printable c -> restart (typed c)
    | otherwise -> w
  where
    example t p = restart w { wT = t, wP = p }
    typed ch
      | wEditT w = if length (wT w) < 40 then w { wT = wT w ++ [ch] } else w
      | otherwise = if length (wP w) < 12 then w { wP = wP w ++ [ch] } else w
onKey _ w = w

-- ---------- やってみよう ----------
-- 1. お手本 2 で、比較回数が「いちばん悪い場合」にどこまで近づくか見る
-- 2. speed を 1 にして、棒グラフができる様子を一気に見る
-- 3. 自分の名前を T に入力して、1 文字のパターンで探してみよう

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

