-- ============================================
-- 04 接尾辞配列と LCP 配列
-- 文字列の「接尾辞（i 文字目から最後まで）」を辞書順に並べた番号の列 = 接尾辞配列 SA
-- ① 先頭 1, 2, 4, 8 … 文字の順位を「2 つ組」で並べ直す倍々法（プレフィックス・ダブリング）
-- ② 隣どうしの共通接頭辞の長さ LCP → いちばん長い「くり返し部分」がわかる
-- ③ 並んでいるので、どんな部分文字列も二分探索で見つかる
-- 操作：キーで入力 / Tab = 文字列⇄検索語 / Backspace / スペース = 再生・停止
--       → = 1 ステップ / Enter = はじめから / 1〜3 = お手本
-- ============================================
module Main (main) where

import Art
import Data.Char (ord)
import Data.List (intercalate, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

speed :: Int
speed = 45              -- ★ 何フレームに 1 ステップ進むか

-- 接尾辞配列をつくる途中の状態
data S = S
  { sOrder :: ![Int]
  , sRank  :: !(U.Vector Int)
  , sRank2 :: !(U.Vector Int)
  , sLcp   :: !(U.Vector Int)      -- -1 = まだ
  , sKNow  :: !Int
  , sPhase :: String
  , sLrs   :: !(Maybe (Int, Int))  -- いちばん長いくり返し（行, 長さ）
  }

-- 二分探索の途中の状態
data Srch = Srch
  { srLo    :: !Int
  , srHi    :: !Int
  , srMid   :: !Int
  , srDone  :: !Bool
  , srPhase :: String
  }

data World = World
  { wStr    :: !String
  , wQ      :: !String             -- 検索語
  , wEditS  :: !Bool
  , wAuto   :: !Bool
  , wS      :: !S
  , wGen    :: [S]                 -- 接尾辞配列づくりの残り
  , wSearch :: !(Maybe Srch)
  , wIt     :: [Srch]              -- 二分探索の残り
  , wPhase  :: String
  , wRowY   :: !(U.Vector Float)   -- 表示用：各接尾辞の行（なめらかに動かす）
  }

main :: IO ()
main = runSketch (sketch "17-04 Suffix array" initialWorld)
  { skBack  = cBg
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = restart (World "mississippi" "ssi" True True (emptyS "") [] Nothing [] "" U.empty)

emptyS :: String -> S
emptyS str =
  let n = length str
  in S [0 .. n - 1] (U.fromList (map ord str)) (U.replicate n (-1)) (U.replicate n (-1)) 0 "" Nothing

restart :: World -> World
restart w =
  let str = wStr w
      n = length str
      s0 = emptyS str
  in w { wS = s0
       , wGen = if n > 0 then steps (build str) s0 else []
       , wSearch = Nothing, wIt = []
       , wPhase = if n > 0 then "Start: suffixes in order of i" else "The string is empty"
       , wRowY = U.generate n fromIntegral }

setPhase :: String -> Gen S ()
setPhase p = modify (\s -> s { sPhase = p })

-- ★ 接尾辞配列をつくる（倍々法）→ LCP（笠井のアルゴリズム）
build :: String -> Gen S ()
build str = do
  let n = length str
      sv = U.fromList str
      doubling k = do
        modify (\s ->
          let rk = sRank s
              r2 = U.generate n (\i -> if i + k < n then rk U.! (i + k) else -1)    -- 2 つ目の順位（はみ出したら −1）
          in s { sKNow = k, sRank2 = r2
               , sOrder = sortBy (comparing (\a -> (rk U.! a, r2 U.! a))) (sOrder s) })
        setPhase ("Sort by first " ++ show (2 * k) ++ " chars: pairs (rank[i], rank[i+" ++ show k ++ "])")
        yield
        modify (\s ->
          let ord' = sOrder s
              rk = sRank s
              r2 = sRank2 s
              differ a b = rk U.! a /= rk U.! b || r2 U.! a /= r2 U.! b
              rs = scanl (\r (a, b) -> r + (if differ a b then 1 else 0)) 0 (zip ord' (drop 1 ord'))
          in s { sRank = U.replicate n 0 U.// zip ord' rs })
        setPhase "Assign new ranks (equal pairs get equal ranks)"
        yield
        fin <- gets (\s -> maximum (0 : U.toList (sRank s)) == n - 1)    -- 順位が全部ちがえば完成
        if fin || k >= n then pure () else doubling (2 * k)
      -- LCP を求める（笠井らの方法）：h は 1 つ前の答えより 1 しか減らない
      lcpLoop inv ov i h
        | i >= n = pure ()
        | inv U.! i > 0 = do
            let r = inv U.! i
                j = ov U.! (r - 1)
                ext a | i + a < n && j + a < n && sv U.! (i + a) == sv U.! (j + a) = ext (a + 1)
                      | otherwise = a
                h' = ext h
            modify (\s -> s { sLcp = sLcp s U.// [(r, h')] })
            setPhase ("LCP: \"" ++ drop i str ++ "\" and the one above \"" ++ drop j str ++ "\" share " ++ show h' ++ " chars")
            yield
            lcpLoop inv ov (i + 1) (if h' > 0 then h' - 1 else h')
        | otherwise = lcpLoop inv ov (i + 1) 0
  yield
  doubling 1
  setPhase "Suffix array complete! Next: LCP (common part with the row above)"
  yield
  ov <- gets (U.fromList . sOrder)
  let inv = U.replicate n 0 U.// zip (U.toList ov) [0 ..]
  modify (\s -> s { sLcp = sLcp s U.// [(0, 0)] })
  lcpLoop inv ov 0 0
  lcp <- gets sLcp
  let best = foldl (\b r -> if lcp U.! r > lcp U.! b then r else b) 0 [1 .. n - 1]
      len = lcp U.! best
  modify (\s -> s { sLrs = if len > 0 then Just (best, len) else Nothing })
  setPhase (if len > 0
              then "Longest repeated part = \"" ++ take len (drop (ov U.! best) str) ++ "\" (largest LCP)"
              else "No repeated part")
  yield
  yield   -- 最後の 1 歩で二分探索を始める（advance を見よ）

-- ★ 二分探索：Q で始まる接尾辞の範囲を探す
startSearch :: World -> World
startSearch w
  | not (null (wGen w)) && U.elem (-1) (sLcp (wS w)) = w
  | null (wQ w) = w { wSearch = Nothing, wIt = [] }
  | otherwise =
      let n = length (wStr w)
          s0 = Srch 0 n (-1) False (wPhase w)
      in w { wSearch = Just s0, wIt = steps (searchGen (wStr w) (wQ w) (sOrder (wS w))) s0 }

searchGen :: String -> String -> [Int] -> Gen Srch ()
searchGen str q order = do
  let n = length str
      m = length q
      ov = U.fromList order
      suf r = drop (ov U.! r) str
      piece r = take m (suf r)
      loop lo hi
        | lo < hi = do
            let mid = (lo + hi) `div` 2
                before = piece mid < q
            modify (\s -> s { srLo = lo, srHi = hi, srMid = mid
                            , srPhase = "Binary search: \"" ++ suf mid ++ "\" is "
                                ++ (if before then "before \"" ++ q ++ "\" -> lower half"
                                              else "after or equal to \"" ++ q ++ "\" -> upper half") })
            yield
            if before then loop (mid + 1) hi else loop lo mid
        | otherwise = do
            let end = lo + length (takeWhile (\r -> piece r == q) [lo .. n - 1])
                places = sort (take (end - lo) (drop lo order))
            modify (\s -> s { srLo = lo, srHi = end, srMid = -1, srDone = True
                            , srPhase = if end > lo
                                then "\"" ++ q ++ "\" occurs " ++ show (end - lo) ++ " times at: " ++ intercalate ", " (map show places)
                                       ++ " (about log2 n compares)"
                                else "\"" ++ q ++ "\" not found" })
            yield
  loop 0 n

advance :: World -> World
advance w = case wGen w of
  (s : r) ->
    let w1 = w { wS = s, wGen = r, wPhase = sPhase s }
    in if null r then startSearch w1 else w1
  [] -> case wSearch w of
    Just sr | not (srDone sr) -> case wIt w of
      (x : r) -> w { wSearch = Just x, wIt = r, wPhase = srPhase x }
      [] -> w { wSearch = Just sr { srDone = True } }
    _ -> w

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 =
  let w = if wAuto w0 && envFrame env `mod` speed == 0 then advance w0 else w0
      rows = U.fromList (rowsOf (sOrder (wS w)) (U.length (wRowY w)))
      y' = U.zipWith (\y r -> lerpF y (fromIntegral r) 0.15) (wRowY w) rows
  in pure w { wRowY = y' }

-- 各接尾辞 p が表の何行目にあるか
rowsOf :: [Int] -> Int -> [Int]
rowsOf order n = U.toList (U.replicate n 0 U.// zip order [0 ..])

drawW :: Env -> World -> Picture
drawW env w =
  let header =
        [ txt env 30 24 22 cFg "Suffix array: sort all suffixes"
        , txt env 30 56 14 cDim ("Editing: " ++ (if wEditS w then "string" else "search word")
            ++ " (Tab)   Space play/stop   Right 1 step   Enter restart   1-3 examples")
        , txt env 30 84 17 cGold (wPhase w)
        , txt env 30 112 17 (if wEditS w then cDim else cOrchid) ("Search: \"" ++ wQ w ++ "\"") ]
  in if null (wStr w) then Pictures header else Pictures (header ++ table env w)

table :: Env -> World -> [Picture]
table env w =
  let hh = envH env
      str = wStr w
      n = length str
      s = wS w
      rh = clampF 18 40 ((hh - 240) / fromIntegral n)
      y0 = 180
      cw = rh * 0.62
      fs = max 14 (rh * 0.42)
      cxR = 40
      cxI = 90
      cxSuf = 150
      cxRk = 170 + cw * fromIntegral n
      cxLcp = 290 + cw * fromIntegral n
      kNow = sKNow s
      building = U.elem (-1) (sLcp s)
      rowOf = U.fromList (rowsOf (sOrder s) n)
      search = wSearch w
      row p =
        let r = rowOf U.! p
            y = y0 + (wRowY w U.! p) * rh
            lrsLen = maybe 0 snd (sLrs s)
            inLrs = case sLrs s of
              Just (lr, _) -> r == lr || r == lr - 1
              Nothing -> False
            inSearch = maybe False (\sr -> r >= srLo sr && r < srHi sr) search
            isMid = maybe False (\sr -> r == srMid sr) search
            finished = maybe False srDone search
            rowCol | isMid = cAmber
                   | finished && inSearch = cGreen
                   | inSearch = rgb255 0x2a 0x23 0x42
                   | otherwise = cPanel
            letter k =
              let hl = (inLrs && k < lrsLen) || (finished && inSearch && k < length (wQ w))
                  c | kNow > 0 && k >= 2 * kNow && building = cDim
                    | hl = cMint
                    | otherwise = cFg
              in txtM env (cxSuf + fromIntegral k * cw) (y + rh / 2) fs c (chr1 str (p + k))
            lcpV = sLcp s U.! r
            rankStr = "(" ++ show (sRank s U.! p)
                        ++ (if building then ", " ++ show (sRank2 s U.! p) else "") ++ ")"
        in Pictures
             [ box env rowCol (cxR - 10) y (cxLcp + 60 - cxR) (rh - 3)
             , txtM env cxR (y + rh / 2) fs cDim (show r)
             , txtM env cxI (y + rh / 2) fs cOrchid (show p)
             , Pictures (map letter [0 .. n - p - 1])
             , if kNow > 0 then txtM env cxRk (y + rh / 2) fs cGold rankStr else Blank
             , if lcpV >= 0
                 then Pictures [ txtM env cxLcp (y + rh / 2) fs cMint (show lcpV)
                               , box env cMint (cxLcp + 30) (y + rh * 0.3) (fromIntegral lcpV * 10) (rh * 0.4) ]
                 else Blank ]
  in [ txtM env cxR (y0 - 12) 14 cDim "#"
     , txtM env cxI (y0 - 12) 14 cDim "i"
     , txtM env cxSuf (y0 - 12) 14 cDim "suffix"
     , if kNow > 0 then txtM env cxRk (y0 - 12) 14 cDim "rank pair" else Blank
     , txtM env cxLcp (y0 - 12) 14 cDim "LCP"
     , Pictures (map row [0 .. n - 1])
     , txt env 30 (y0 + fromIntegral n * rh + 12) 14 cDim
         ("SA = [" ++ (if null (wGen w) then intercalate ", " (map show (sOrder s)) else "...") ++ "]")
     ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey k Down _ _) w = pure (onKey k w)
onEvent _ _ w = pure w

onKey :: Key -> World -> World
onKey k w
  | k == SpecialKey KeySpace = w { wAuto = not (wAuto w) }
  | k == SpecialKey KeyRight = advance w
  | isEnter k = restart w
  | isTab k = w { wEditS = not (wEditS w) }
  | isBack k = if wEditS w then restart w { wStr = dropLast (wStr w) } else startSearch w { wQ = dropLast (wQ w) }
onKey (Char c) w = case c of
  '1' -> restart w { wStr = "mississippi", wQ = "ssi" }
  '2' -> restart w { wStr = "banana", wQ = "ana" }
  '3' -> restart w { wStr = "abracadabra", wQ = "abra" }
  _ | not (printable c) -> w
    | wEditS w -> if length (wStr w) < 18 then restart w { wStr = wStr w ++ [c] } else w
    | otherwise -> if length (wQ w) < 8 then startSearch w { wQ = wQ w ++ [c] } else w
onKey _ w = w

-- ---------- やってみよう ----------
-- 1. banana の接尾辞配列を手で書いてから、画面で確かめる
-- 2. aaaaaaaa を入力すると、倍々法の回数はいくつになる？ abcdefgh とも比べよう
-- 3. 検索語を変えて、見つかる範囲（緑の行）がどう変わるか見る

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

