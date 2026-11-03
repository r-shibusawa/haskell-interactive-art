-- ============================================
-- 02 有限オートマトンで文字列照合
-- 「いま P の先頭何文字ぶん一致しているか」を状態 0〜m として、1 文字読むごとに表 δ で次の状態へ
-- テキストの各文字を 1 回しか見ない。表をつくる（前処理）のがこの方法の仕事
-- 操作：キーで入力 / Tab = テキスト⇄パターン / Backspace / スペース = 再生・停止
--       → = 1 文字読む / Enter = はじめから / 1〜3 = お手本
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate, isSuffixOf, nub, sort)
import qualified Data.Map.Strict as M
import Numeric (showFFloat)

speed :: Int
speed = 24              -- ★ 何フレームに 1 文字読むか

data World = World
  { wT     :: !String
  , wP     :: !String
  , wEditT :: !Bool
  , wAuto  :: !Bool
  , wSigma :: !String                   -- アルファベット（P に出てくる文字）
  , wDelta :: !(M.Map (Int, Char) Int)  -- δ(q, a) = 次の状態
  , wBuilt :: !Int                      -- 表をつくるアニメーション：何マスできたか
  , wState :: !Int
  , wPos   :: !Int
  , wFound :: ![Int]
  , wLast  :: !(Maybe (Int, Int, Char)) -- 直前の遷移 (from, to, 文字)
  }

main :: IO ()
main = runSketch (sketch "17-02 Finite automaton" initialWorld)
  { skBack  = cBg
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = rebuild (World "abaabaabacabaabaabacab" "abaabac" False True "" M.empty 0 0 0 [] Nothing)

-- ★ 遷移関数をつくる
-- δ(q, a) = 「P[0..q) + a」の末尾と一致する P の最長の先頭部分の長さ
rebuild :: World -> World
rebuild w =
  let p = wP w
      m = length p
      sigma = sort (nub p)
      deltaOf q a =
        let u = take q p ++ [a]
            go k | k > 0 && not (take k p `isSuffixOf` u) = go (k - 1)
                 | otherwise = k
        in go (min m (q + 1))
      table = M.fromList [((q, a), deltaOf q a) | q <- [0 .. m], a <- sigma]
  in restartRun w { wSigma = sigma, wDelta = table, wBuilt = 0 }

restartRun :: World -> World
restartRun w = w { wState = 0, wPos = 0, wFound = [], wLast = Nothing }

readOne :: World -> World
readOne w =
  let m = length (wP w)
  in case charAt (wT w) (wPos w) of
       Just c | m > 0 ->
         let nxt = M.findWithDefault 0 (wState w, c) (wDelta w)   -- P に出てこない文字は状態 0 へ
         in w { wLast = Just (wState w, nxt, c), wState = nxt
              , wFound = if nxt == m then wFound w ++ [wPos w - m + 1] else wFound w
              , wPos = wPos w + 1 }
       _ -> w

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wBuilt w < cells = pure w { wBuilt = wBuilt w + 1 }   -- 表を 1 マスずつ埋める
  | wAuto w && envFrame env `mod` speed == 0 = pure (readOne w)
  | otherwise = pure w
  where cells = (length (wP w) + 1) * length (wSigma w)

drawW :: Env -> World -> Picture
drawW env w =
  let header =
        [ txt env 30 24 22 cFg "Finite automaton: each character moves the state"
        , txt env 30 56 14 cDim ("Editing: " ++ (if wEditT w then "text T" else "pattern P")
            ++ " (Tab)   Space play/stop   Right read 1 char   Enter restart   1-3 examples") ]
      m = length (wP w)
  in if m == 0
       then Pictures (header ++ [txt env 30 100 18 cBad "The pattern is empty. Type some characters."])
       else Pictures (header ++ body env w)

body :: Env -> World -> [Picture]
body env w =
  let ww = envW env
      hh = envH env
      p = wP w
      t = wT w
      m = length p
      n = length t
      sigma = wSigma w
      delta q a = M.findWithDefault 0 (q, a) (wDelta w)
      lastE = wLast w
      state = wState w
      -- --- 状態図 ---
      gap = min 120 ((ww - 120) / fromIntegral (max m 1))
      x0 = (ww - gap * fromIntegral m) / 2
      yS = hh * 0.36
      xOf q = x0 + fromIntegral (q :: Int) * gap
      -- 後戻りの矢印（0 以外）をまとめる
      back = M.fromListWith (flip (++))
               [((q, k), [a]) | q <- [0 .. m], a <- sigma, let k = delta q a, k > 0, k /= q + 1]
      on q k = case lastE of
        Just (f, e, _) -> f == q && e == k
        Nothing -> False
      backEdge ((q, k), as) =
        let lit = on q k
            col = if lit then cGold else rgb255 0x6b 0x5a 0x8f
            hgt = 30 + fromIntegral (abs (q - k)) * 20
            labelY = if q == k then yS - 56 else yS - 22 - hgt * 0.75 - 8
            curve
              | q == k = hoop env col (if lit then 3 else 1.5) (xOf q, yS - 34) 28
              | otherwise = Pictures
                  [ path env col (if lit then 3 else 1.5)
                      (bezier (xOf q, yS - 22) (xOf q, yS - 22 - hgt) (xOf k, yS - 22 - hgt) (xOf k, yS - 22))
                  , fillPoly env col [(xOf k, yS - 20), (xOf k - 5, yS - 30), (xOf k + 5, yS - 30)] ]
        in Pictures [curve, txtC env ((xOf q + xOf k) / 2) labelY 14 (if lit then cGold else cDim) (intercalate "," (map (: []) as))]
      -- 前に進む矢印
      forward q =
        let lit = on q (q + 1)
            col = if lit then cGold else cOrchid
        in Pictures
             [ seg env col (if lit then 3 else 2) (xOf q + 22, yS) (xOf (q + 1) - 24, yS)
             , fillPoly env col [(xOf (q + 1) - 22, yS), (xOf (q + 1) - 32, yS - 5), (xOf (q + 1) - 32, yS + 5)]
             , txtC env ((xOf q + xOf (q + 1)) / 2) (yS + 16) 14 col (chr1 p q) ]
      -- 状態の丸
      stateDot q =
        let fc | q == state = if state == m then cMint else cGold
               | otherwise = cPanel
        in Pictures
             [ dot env fc (xOf q, yS) 40
             , hoop env (if q == m then cMint else rgb255 0x6b 0x5a 0x8f) (if q == m then 4 else 2) (xOf q, yS) 40
             , txtC env (xOf q) yS 16 (if q == state then cBg else cFg) (show q) ]
      -- --- 遷移表 δ ---
      cs = 30
      tx = 30
      ty = hh * 0.52
      ns = length sigma
      tableHead =
        txtC env (tx + cs / 2) (ty + cs / 2) 14 cDim "d"
          : [ txtC env (tx + fromIntegral (c + 1) * cs + cs / 2) (ty + cs / 2) 14 cOrchid [a] | (c, a) <- zip [0 :: Int ..] sigma ]
      tableRow q =
        txtC env (tx + cs / 2) (ty + fromIntegral (q + 1) * cs + cs / 2) 14 (if q == state then cGold else cDim) (show q)
          : [ tableCell q c a | (c, a) <- zip [0 :: Int ..] sigma ]
      tableCell q c a =
        let shown = q * ns + c < wBuilt w
            lit = case lastE of
              Just (f, _, ch) -> f == q && ch == a
              Nothing -> False
            x = tx + fromIntegral (c + 1) * cs
            y = ty + fromIntegral (q + 1) * cs
            v = delta q a
        in Pictures
             [ cell env (if lit then cAmber else cPanel) cEdge x y (cs - 2) (cs - 2)
             , if shown then txtC env (x + cs / 2 - 1) (y + cs / 2) 14 (if v == q + 1 then cOrchid else cFg) (show v) else Blank ]
      -- --- テキスト ---
      cw = clampF 14 40 ((ww - tx - fromIntegral (ns + 2) * cs - 80) / fromIntegral (max n 1))
      x1 = tx + fromIntegral (ns + 2) * cs + 30
      yT = ty + 10
      found = wFound w
      pos = wPos w
      textCell i =
        let hit = any (\f -> i >= f && i < f + m) found
            x = x1 + fromIntegral i * cw
        in Pictures
             [ cell env (if hit then cGreen else if i == pos - 1 then cAmber else cPanel)
                 (if wEditT w then cOrchid else cEdge) x yT (cw - 2) (cw - 2)
             , txtC env (x + cw / 2 - 1) (yT + cw / 2) (max 14 (cw * 0.55)) (if i < pos then cFg else cDim) (chr1 t i) ]
      cells = (m + 1) * ns
  in [ Pictures (map backEdge (M.toList back))
     , Pictures (map forward [0 .. m - 1])
     , Pictures (map stateDot [0 .. m])
     , txt env 30 (yS + 40) 14 cDim "Arrows not drawn all go back to state 0"
     , Pictures tableHead
     , Pictures (concatMap tableRow [0 .. m])
     , Pictures (map textCell [0 .. n - 1])
     , txt env x1 (yT + cw + 20) 17 cFg ("Read " ++ show pos ++ " / " ++ show n ++ "   state " ++ show state
         ++ "   found at: " ++ (if null found then "-" else intercalate ", " (map show found)))
     , txt env x1 (yT + cw + 50) 14 cDim
         (if wBuilt w < cells
            then "Building the table... (" ++ show (wBuilt w) ++ "/" ++ show cells ++ ")  preprocessing O(m^3 |Sigma|)"
            else "Matching looks up the table once per character: Theta(n)")
     ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey k Down _ _) w = pure (onKey k w)
onEvent _ _ w = pure w

onKey :: Key -> World -> World
onKey k w
  | k == SpecialKey KeySpace = w { wAuto = not (wAuto w) }
  | k == SpecialKey KeyRight = readOne w { wBuilt = 1000000000 }
  | isEnter k = restartRun w
  | isTab k = w { wEditT = not (wEditT w) }
  | isBack k = if wEditT w then restartRun w { wT = dropLast (wT w) } else rebuild w { wP = dropLast (wP w) }
onKey (Char c) w = case c of
  '1' -> rebuild w { wT = "abaabaabacabaabaabacab", wP = "abaabac" }
  '2' -> rebuild w { wT = "aabaaabaaaab", wP = "aaab" }
  '3' -> rebuild w { wT = "GCATCGCAGAGAGTATACAGTACG", wP = "GCAGAGAG" }
  _ | not (printable c) -> w
    | wEditT w -> if length (wT w) < 30 then restartRun w { wT = wT w ++ [c] } else w
    | otherwise -> if length (wP w) < 9 then rebuild w { wP = wP w ++ [c] } else w
onKey _ w = w

-- 3 次ベジェ曲線の上の点
bezier :: Point -> Point -> Point -> Point -> [Point]
bezier (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  [ (b x0 x1 x2 x3 t, b y0 y1 y2 y3 t) | i <- [0 .. 20 :: Int], let t = fromIntegral i / 20 ]
  where b a c d e t = let u = 1 - t in u * u * u * a + 3 * u * u * t * c + 3 * u * t * t * d + t * t * t * e

-- ---------- やってみよう ----------
-- 1. お手本 2 のパターン aaab で、どの状態にどんな後戻りの矢印があるか確かめる
-- 2. パターンに同じ文字をくり返し入れると、状態図はどう変わる？
-- 3. 表 δ の大きさは (m+1)×|Σ|。文字の種類が増えるとどうなるか試そう

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

