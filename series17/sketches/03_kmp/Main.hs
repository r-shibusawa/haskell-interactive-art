-- ============================================
-- 03 KMP 法（クヌース–モリス–プラット）
-- ① 前処理：接頭辞関数 π[q] =「P の先頭 q 文字の、末尾と先頭が一致する最長の長さ」
-- ② 照合：くい違ったら、π を使って「もう一致しているとわかっている分」を飛ばしてずらす
-- テキストを後戻りせずに読むので Θ(n + m)。素朴な方法との比較回数の差を見てみよう
-- 操作：キーで入力 / Tab = テキスト⇄パターン / Backspace / スペース = 再生・停止
--       → = 1 ステップ / Enter = はじめから / 1〜3 = お手本
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

speed :: Int
speed = 20              -- ★ 何フレームに 1 ステップ進むか

-- いまの様子（p5.js 版の snap）
data Snap
  = NoSnap
  | PiSnap !Int !Int !Bool String       -- q, k, 一致したか, 説明
  | MatchSnap !Int !Int !Bool String    -- i, q, 一致したか, 説明
  | DoneSnap

data S = S
  { sPi    :: !(U.Vector Int)   -- 接頭辞関数（0 始まり。-1 = まだ）
  , sSnap  :: !Snap
  , sComps :: !Int
  , sFound :: ![Int]
  , sPhase :: String
  }

data World = World
  { wT     :: !String
  , wP     :: !String
  , wEditT :: !Bool
  , wAuto  :: !Bool
  , wS     :: !S
  , wRest  :: [S]               -- 処理の続き（ジェネレーターの残り）
  , wNaive :: !Int              -- 素朴な方法の比較回数
  }

main :: IO ()
main = runSketch (sketch "17-03 KMP" initialWorld)
  { skBack  = cBg
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` speed == 0 then advance w else w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = restart (World "caabaabbaabaabaacaabaaba" "aabaaba" False True (S U.empty NoSnap 0 [] "") [] 0)

restart :: World -> World
restart w =
  let m = length (wP w)
      ok = m > 0 && m <= length (wT w)
      s0 = S (U.replicate m (-1)) NoSnap 0 []
             (if ok then "Step 1: build the prefix function pi" else "The pattern is empty or longer than the text")
  in w { wS = s0, wRest = if ok then steps (kmp (wP w) (wT w)) s0 else [], wNaive = countNaive (wT w) (wP w) }

setPi :: Int -> Int -> Gen S ()
setPi q v = modify (\s -> s { sPi = sPi s U.// [(q, v)] })

piAt :: Int -> Gen S Int
piAt q = gets (\s -> sPi s U.! q)

snapIt :: Snap -> Gen S ()
snapIt v = modify (\s -> s { sSnap = v })

-- ★ KMP 本体（yield のたびに 1 ステップ止まる）
kmp :: String -> String -> Gen S ()
kmp ps ts = do
  let p = U.fromList ps
      t = U.fromList ts
      m = U.length p
      n = U.length t
      -- ① 接頭辞関数 π をつくる
      backK q k
        | k > 0 && p U.! k /= p U.! q = do
            pk <- piAt (k - 1)
            snapIt (PiSnap q k False ("P[" ++ show k ++ "] != P[" ++ show q ++ "] -> k = pi[" ++ show (k - 1) ++ "] = " ++ show pk))
            yield
            backK q pk
        | otherwise = pure k
      buildPi q k
        | q >= m = pure ()
        | otherwise = do
            k1 <- backK q k
            let ok = p U.! k1 == p U.! q
                k2 = if ok then k1 + 1 else k1
            setPi q k2
            snapIt (PiSnap q k2 ok (if ok then "Equal -> pi[" ++ show q ++ "] = " ++ show k2 else "Not equal -> pi[" ++ show q ++ "] = 0"))
            yield
            buildPi (q + 1) k2
      -- ② π を使ってテキストを読む
      backQ i q
        | q > 0 && p U.! q /= t U.! i = do
            pq <- piAt (q - 1)
            modify (\s -> s { sComps = sComps s + 1 })
            snapIt (MatchSnap i q False ("T[" ++ show i ++ "] != P[" ++ show q ++ "] -> shift by " ++ show (q - pq)
                      ++ " (q = pi[" ++ show (q - 1) ++ "] = " ++ show pq ++ ")"))
            yield
            backQ i pq
        | otherwise = pure q
      match i q
        | i >= n = pure ()
        | otherwise = do
            q1 <- backQ i q
            modify (\s -> s { sComps = sComps s + 1 })
            let ok = p U.! q1 == t U.! i
                q2 = if ok then q1 + 1 else q1
            snapIt (MatchSnap i (if ok then q2 - 1 else q2) ok
                      (if ok then "T[" ++ show i ++ "] = P[" ++ show (q2 - 1) ++ "]  " ++ show q2 ++ " chars match"
                             else "T[" ++ show i ++ "] != P[0]  next character"))
            yield
            if q2 == m
              then do
                pm <- piAt (m - 1)
                modify (\s -> s { sFound = sFound s ++ [i - m + 1] })
                snapIt (MatchSnap i (m - 1) True ("Found at " ++ show (i - m + 1) ++ "! Continue from q = pi["
                          ++ show (m - 1) ++ "] = " ++ show pm))
                yield
                match (i + 1) pm
              else match (i + 1) q2
  setPi 0 0
  buildPi 1 0
  modify (\s -> s { sPhase = "Step 2: match using pi" })
  match 0 0
  modify (\s -> s { sPhase = "Done", sSnap = DoneSnap })
  yield

countNaive :: String -> String -> Int
countNaive t p
  | m == 0 = 0
  | otherwise = sum [ cost (drop s t) | s <- [0 .. n - m] ]
  where
    n = length t
    m = length p
    cost u = let k = length (takeWhile id (zipWith (==) u p)) in min m (k + 1)

advance :: World -> World
advance w = case wRest w of
  [] -> w
  (s : r) -> w { wS = s, wRest = r }

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      s = wS w
      t = wT w
      p = wP w
      n = length t
      m = length p
      snap = sSnap s
      piOf q = if q < U.length (sPi s) then sPi s U.! q else -1
      cw = clampF 16 46 ((ww - 80) / fromIntegral (maximum [n, m, 1]))
      fs = max 14 (cw * 0.55)
      -- --- π の表 ---
      xp = 30
      yp = 130
      px = xp + 30
      piCol q =
        let x = px + fromIntegral q * cw
            v = piOf q
            hl = case snap of
              PiSnap sq sk _ _
                | q == sq -> cAmber
                | q == sk -> cBlue
              _ -> cPanel
            x1 = x + cw / 2
            x2 = px + fromIntegral (v - 1) * cw + cw / 2
            arcPts = [ ((x1 + x2) / 2 + (x1 - x2) / 2 * cos a, yp + cw * 3 + (x1 - x2) * 0.25 * sin a)
                     | i <- [0 .. 16 :: Int], let a = pi * fromIntegral i / 16 ]
        in Pictures
             [ txtC env (x + cw / 2) (yp + cw * 0.5) 14 cDim (show q)
             , cell env hl (if wEditT w then cEdge else cOrchid) x (yp + cw) (cw - 2) (cw - 2)
             , txtC env (x + cw / 2 - 1) (yp + cw * 1.5) fs cOrchid (chr1 p q)
             , box env cPanel x (yp + cw * 2) (cw - 2) (cw - 2)
             , txtC env (x + cw / 2 - 1) (yp + cw * 2.5) fs (if v < 0 then cDim else cMint) (if v < 0 then "." else show v)
             -- π[q] > 0 なら「先頭の何文字と同じか」を弧で示す
             , if v > 0 then path env cMint 1.2 arcPts else Blank ]
      -- 接頭辞と接尾辞の重なりの説明
      piNote = case snap of
        PiSnap _ _ ok note ->
          let nx = px + fromIntegral m * cw + 30
          in Pictures
               [ txt env nx (yp + cw) 14 cDim "First k chars (blue) vs just before q (gold)"
               , txt env nx (yp + cw * 1.8) 16 (if ok then cMint else cBad) note ]
        _ -> Blank
      -- --- テキストとパターン ---
      yT = hh * 0.5
      x0 = (ww - cw * fromIntegral n) / 2
      found = sFound s
      textCell i =
        let hit = any (\f -> i >= f && i < f + m) found
            (fc, tc) = case snap of
              MatchSnap si _ ok _
                | si == i -> (if ok then cGreen else cRed, cFg)
                | i > si -> (if hit then cGreen else cPanel, cDim)
              _ -> (if hit then cGreen else cPanel, cFg)
            x = x0 + fromIntegral i * cw
        in Pictures
             [ cell env fc (if wEditT w then cOrchid else cEdge) x yT (cw - 2) (cw - 2)
             , txtC env (x + cw / 2 - 1) (yT + cw / 2) fs tc (chr1 t i) ]
      patternRow = case snap of
        MatchSnap si sq ok note ->
          let shift' = si - sq
              patCell k
                | shift' + k < 0 || shift' + k >= n = Blank
                | otherwise =
                    let x = x0 + fromIntegral (shift' + k) * cw
                        fc | k < sq = cBlue
                           | k == sq = if ok then cGreen else cRed
                           | otherwise = cPanel
                    in Pictures
                         [ cell env fc cEdge x (yT + cw * 1.3) (cw - 2) (cw - 2)
                         , txtC env (x + cw / 2 - 1) (yT + cw * 1.8) fs cOrchid (chr1 p k) ]
          in Pictures (map patCell [0 .. m - 1] ++ [txt env 30 (yT + cw * 2.8) 16 (if ok then cMint else cBad) note])
        _ -> Blank
      -- --- 比較回数 ---
      yB = hh * 0.82
      comps = sComps s
      naive = wNaive w
      bw = (ww - 60) / fromIntegral (maximum [naive, comps, 1])
  in Pictures
       [ txt env 30 24 22 cFg "KMP: never check what you already know"
       , txt env 30 56 14 cDim ("Editing: " ++ (if wEditT w then "text T" else "pattern P")
           ++ " (Tab)   Space play/stop   Right 1 step   Enter restart   1-3 examples")
       , txt env 30 86 18 cGold (sPhase s)
       , txtM env xp (yp + cw * 0.5) 14 cDim "q"
       , txtM env xp (yp + cw * 1.5) 14 cDim "P"
       , txtM env xp (yp + cw * 2.5) 14 cDim "pi"
       , Pictures (map piCol [0 .. m - 1])
       , piNote
       , Pictures (map textCell [0 .. n - 1])
       , patternRow
       , txt env 30 yB 18 cFg ("KMP comparisons " ++ show comps ++ "   naive " ++ show naive ++ "   found at: "
           ++ (if null found then "-" else intercalate ", " (map show found)))
       , box env cOrchid 30 (yB + 34) (fromIntegral comps * bw) 12
       , box env (rgb255 0x6b 0x5a 0x8f) 30 (yB + 52) (fromIntegral naive * bw) 12
       , txt env 30 (yB + 72) 14 cDim "Top: KMP   Bottom: naive (its total when finished)"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey k Down _ _) w = pure (onKey k w)
onEvent _ _ w = pure w

onKey :: Key -> World -> World
onKey k w
  | k == SpecialKey KeySpace = w { wAuto = not (wAuto w) }
  | k == SpecialKey KeyRight = advance w
  | isEnter k = restart w
  | isTab k = w { wEditT = not (wEditT w) }
  | isBack k = restart (if wEditT w then w { wT = dropLast (wT w) } else w { wP = dropLast (wP w) })
onKey (Char c) w = case c of
  '1' -> restart w { wT = "caabaabbaabaabaacaabaaba", wP = "aabaaba" }
  '2' -> restart w { wT = "aaaaaaaaaaaaaaaaaaaaaaab", wP = "aaaab" }   -- 素朴な方法が苦手な例
  '3' -> restart w { wT = "abcabcabdabcabcabcabdab", wP = "abcabd" }
  _ | not (printable c) -> w
    | wEditT w -> restart (if length (wT w) < 34 then w { wT = wT w ++ [c] } else w)
    | otherwise -> restart (if length (wP w) < 12 then w { wP = wP w ++ [c] } else w)
onKey _ w = w

-- ---------- やってみよう ----------
-- 1. お手本 2 で、KMP と素朴な方法の比較回数の差を確かめる
-- 2. パターン abcabc の π を手で計算してから、画面で答え合わせをする
-- 3. π がすべて 0 になるパターンでは、ずれ方はどうなる？

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

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])

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

