-- ============================================
-- 15 素数判定と素因数分解：ミラー–ラビンとポラードのロー
-- 1〜4：ミラー–ラビン法。a^(n−1) ≢ 1 なら n は合成数（フェルマー）。
--       それをすり抜ける「うそつき」もいる（カーマイケル数 561 など）ので、途中に「1 の自明でない平方根」がないかも調べる
--       マス = 底 a。赤 = 合成数だと見破った（証人）、緑 = だまされた（うそつき）。マウスを乗せると計算の中身
-- 5・6：ポラードのロー法。xᵢ₊₁ = xᵢ² + 1 mod n の数列は、隠れた素因数 p で見ると ρ の形でループする
-- 1：561（カーマイケル数）2：341（2 にだまされる）3：97（素数）4：ランダム ←→：n を 2 ずつ T：フェルマー／MR の表示切りかえ
-- 5：n = 4307 6：n = 247021　スペース：ロー法を 1 歩 / A：自動
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Numeric (showFFloat)

speed :: Int
speed = 25 -- ★ ロー法の自動再生の間隔

-- ミラー–ラビンの 1 つの底 a の計算
data Wit = Wit
  { wtA   :: !Int
  , wtT   :: !Int
  , wtU   :: !Int
  , wtSeq :: ![Int]
  , wtWhy :: !(Maybe String)
  }

data RhoRow = RhoRow !Int !Int !Int !Int  -- i, x, y, d

data World = World
  { wRho   :: !Bool
  , wNn    :: !Int
  , wFerm  :: !Bool       -- True ならフェルマーの表示
  , wCache :: [Wit]       -- n が変わったときだけ計算する
  , wRn    :: !Int
  , wX     :: !Int        -- いまの x
  , wCur   :: !Int        -- いまの番号（xs.length - 1）
  , wI     :: !Int
  , wKk    :: !Int
  , wY     :: !Int
  , wFound :: !(Maybe Int)
  , wLog   :: ![RhoRow]   -- 表に見せる分だけ（最大 14 行）
  , wAuto  :: !Bool
  , wTimer :: !Int
  }

main :: IO ()
main = runSketch (sketch "16-15 Primality and Rho" (setN 561 (initRho 4307 (World False 561 False [] 4307 2 0 1 2 2 Nothing [] False 0))))
  { skBack  = bgC
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

modPow :: Int -> Int -> Int -> Int
modPow a0 b0 m = go (a0 `mod` m) b0 1
  where
    go _ 0 r = r
    go a b r = go (a * a `mod` m) (b `div` 2) (if odd b then r * a `mod` m else r)

isPrime :: Int -> Bool
isPrime x = x >= 2 && all (\k -> x `mod` k /= 0) (takeWhile (\k -> k * k <= x) [2 ..])

-- 証人かどうか調べる：n−1 = 2ᵗu と分けて、a^u から 2 乗を t 回くり返す
witness :: Int -> Int -> Wit
witness m a =
  let split u t = if even u then split (u `div` 2) (t + 1) else (u, t)
      (uu, tt) = split (m - 1) (0 :: Int)
      sq = take (tt + 1) (iterate (\x -> x * x `mod` m) (modPow a uu m))
      badRoot = [ show p ++ "^2 = 1 but " ++ show p ++ " is not +-1: a nontrivial square root of 1, so composite"
                | (p, x) <- zip sq (drop 1 sq), x == 1, p /= 1, p /= m - 1 ]
      lastV = foldl (\_ x -> x) 1 sq
      why = case badRoot of
        (s : _) -> Just s
        [] | lastV /= 1 -> Just ("a^(n-1) = " ++ show lastV ++ " is not 1: breaks Fermat's little theorem, so composite")
           | otherwise -> Nothing
  in Wit a tt uu sq why

setN :: Int -> World -> World
setN n w = w { wNn = n, wCache = map (witness n) [1 .. n - 1] }

initRho :: Int -> World -> World
initRho rn w = w { wRn = rn, wX = 2, wCur = 0, wI = 1, wKk = 2, wY = 2, wFound = Nothing, wLog = [] }

-- ロー法の 1 歩：xᵢ = xᵢ₋₁² + 1、i が 2 の累乗のとき y を保存、gcd(y − xᵢ, n) を調べる
rhoStep :: World -> World
rhoStep w = case wFound w of
  Just _ -> w
  Nothing ->
    let rn = wRn w
        i = wI w + 1
        x = (wX w * wX w + 1) `mod` rn
        d = gcd (abs (wY w - x)) rn
        logN = wLog w ++ [RhoRow i x (wY w) d]
        w1 = w { wI = i, wX = x, wCur = wCur w + 1
               , wLog = drop (length logN - 14) logN
               , wFound = if d /= 1 && d /= rn then Just d else Nothing }
    in if i == wKk w then w1 { wY = x, wKk = wKk w * 2 } else w1

stepW :: World -> World
stepW w
  | wRho w && wAuto w && wFound w == Nothing =
      let t = wTimer w + 1
          w1 = w { wTimer = t }
      in if t `mod` speed == 0 then rhoStep w1 else w1
  | otherwise = w

drawW :: Env -> World -> Picture
drawW env w = Pictures
  [ txt env 30 50 26 (g8 235)
      (if wRho w then "Pollard's rho: factoring " ++ show (wRn w) else "Miller-Rabin: is " ++ show (wNn w) ++ " prime?")
  , txt env 30 80 15 (g8 150) ("1: 561  2: 341  3: 97  4: random  L/R: n+-2  T: Fermat/MR | 5: 4307  6: 247021  Space: step  A: auto"
                                ++ (if wAuto w then " (on)" else ""))
  , if wRho w then drawRho env w else drawMR env w
  ]

-- いまの表示でマスの配置
layoutMR :: Env -> Int -> (Int, Float)
layoutMR env n =
  let cnt = n - 1
      cols = ceiling (sqrt (fromIntegral cnt * 1.4 :: Float)) :: Int
      rows = (cnt + cols - 1) `div` cols
      cs = min (envW env * 0.56 / fromIntegral cols) ((envH env - 150) / fromIntegral rows)
  in (cols, cs)

drawMR :: Env -> World -> Picture
drawMR env w =
  let n = wNn w
      (cols, cs) = layoutMR env n
      ox = 30
      oy = 110
      (mx, my) = mouseP env
      posOf a = let i = a - 1 in (ox + fromIntegral (i `mod` cols) * cs, oy + fromIntegral (i `div` cols) * cs)
      isFermatLiar wt = foldl (\_ x -> x) 1 (wtSeq wt) == 1
      isMrLiar wt = maybe True (const False) (wtWhy wt)
      fl = length (filter isFermatLiar (wCache w))
      ml = length (filter isMrLiar (wCache w))
      cell wt =
        let (x, y) = posOf (wtA wt)
            liar = if wFerm w then isFermatLiar wt else isMrLiar wt
        in box env (if liar then mint else rgba255 255 95 109 200) x y (cs - 1) (cs - 1)
      hovered = [wt | wt <- wCache w, let (x, y) = posOf (wtA wt), mx >= x, mx < x + cs, my >= y, my < y + cs]
      prime = isPrime n
      px = ox + fromIntegral cols * cs + 30
      pw = envW env - px - 30
      pct v = showFFloat (Just 1) (100 * fromIntegral v / fromIntegral (n - 1) :: Double) ""
      header =
        [ (18, if prime then mint else coral, 12, show n ++ " is actually " ++ (if prime then "prime" else "composite (" ++ factorStr n ++ ")"))
        , (16, g8 235, 8, "View: " ++ (if wFerm w then "Fermat" else "Miller-Rabin") ++ " (T toggles)")
        , (16, g8 200, 6, "Fermat liars: " ++ show fl ++ " / " ++ show (n - 1) ++ " (" ++ pct fl ++ "%)")
        , (16, g8 200, 12, "Miller-Rabin liars: " ++ show ml ++ " / " ++ show (n - 1) ++ " (" ++ pct ml ++ "%)")
        , (14, g8 150, 14,
            if prime then "For a prime every a is a liar (all green): no witness exists."
            else "With Miller-Rabin a composite has at most half liars (really <= 1/4). s random tries: fooled with chance <= 2^-s.")
        ]
      detail = case hovered of
        (wt : _) ->
          (16, g8 235, 8, "a = " ++ show (wtA wt) ++ ": n - 1 = " ++ show (n - 1) ++ " = 2^" ++ show (wtT wt) ++ " x " ++ show (wtU wt))
            : [ ( 15
                , if v == 1 then mint else if v == n - 1 then amber else g8 200
                , 2
                , "a^(" ++ show (wtU wt) ++ "*2^" ++ show i ++ ") mod " ++ show n ++ " = " ++ show v ++ (if v == n - 1 then " (= -1)" else "") )
              | (i, v) <- zip [0 :: Int ..] (wtSeq wt) ]
            ++ [ case wtWhy wt of
                   Just why -> (15, coral, 0, why)
                   Nothing -> (15, mint, 0, "Last is 1 and the one before is +-1: no contradiction (it passed as a prime)") ]
        [] -> [(15, g8 150, 0, "Move the mouse onto a cell to see the computation for that a")]
      hoverFrame = case hovered of
        (wt : _) -> let (x, y) = posOf (wtA wt) in frame env white 2 x y (cs - 1) (cs - 1)
        [] -> Blank
  in Pictures
       [ Pictures (map cell (wCache w))
       , hoverFrame
       , stack env px 112 pw (header ++ detail)
       ]

factorStr :: Int -> String
factorStr = intercalate " x " . map show . go 2
  where
    go k m
      | m <= 1 = []
      | m `mod` k == 0 = k : go k (m `div` k)
      | otherwise = go (k + 1) m

smallestFactor :: Int -> Int
smallestFactor m = case [k | k <- takeWhile (\k -> k * k <= m) [2 ..], m `mod` k == 0] of
  (k : _) -> k
  [] -> m

-- xᵢ mod p で ρ の形を作る：しっぽ（最初の重複まで）とループ
rhoShape :: Int -> Int -> ([Int], Int)
rhoShape rn p = go (0 :: Int) 2 IM.empty []
  where
    go i x seen acc
      | i >= 200 = (reverse acc, 0)
      | otherwise =
          let v = x `mod` p
          in case IM.lookup v seen of
               Just s -> (reverse acc, s)
               Nothing -> go (i + 1) ((x * x + 1) `mod` rn) (IM.insert v i seen) (v : acc)

drawRho :: Env -> World -> Picture
drawRho env w =
  let rn = wRn w
      p = smallestFactor rn
      (mods, tl) = rhoShape rn p
      len = length mods
      loopLen = max 1 (len - tl)
      lf = fromIntegral loopLen :: Float
      cx = envW env * 0.3
      cy = envH env * 0.38
      r = min (envH env * 0.22) (22 * lf / pi + 30)
      gap = min 50 ((envH env - 110 - cy - r) / fromIntegral (max 1 tl))
      posOf :: Int -> (Float, Float)
      posOf i
        | i < tl = (cx, cy + r + gap * fromIntegral (tl - i))
        | otherwise = let ang = pi / 2 + tau * fromIntegral (i - tl) / lf in (cx + cos ang * r, cy + sin ang * r)
      lineC = rgb255 70 60 95
      links = [seg env lineC 2 (posOf (i - 1)) (posOf i) | i <- [1 .. len - 1]]
      closing = if len > 0 then seg env lineC 2 (posOf (len - 1)) (posOf tl) else Blank
      cur = wCur w
      yIdx = wKk w `div` 2 - 1
      d0 = min 30 (min (gap * 0.9 + 6) (tau * r / lf * 0.85))
      nodes = [ let (x, y) = posOf i
                    visited = i <= cur || cur >= len
                in Pictures [ dot env (if visited then amber else rgb255 60 52 80) x y d0
                            , txtC env x y 14 (if visited then bgC else g8 200) (show v) ]
              | (i, v) <- zip [0 ..] mods ]
      wrapI i = if i < len then i else tl + ((i - tl) `mod` loopLen)
      (cix, ciy) = posOf (wrapI cur)
      (yix, yiy) = posOf (wrapI yIdx)
      px = envW env * 0.55
      pw = envW env - px - 30
      colsX = [px, px + 50, px + 140, px + 230]
      y0 = 142
      rowsP = [ let col = if d /= 1 then amber else g8 200
                    yy = y0 + 22 + fromIntegral (k :: Int) * 20
                in Pictures [txtT env xx yy 14 col (show v) | (xx, v) <- zip colsX [i, x, y, d]]
              | (k, RhoRow i x y d) <- zip [0 ..] (wLog w) ]
      nRows = length (wLog w)
      yMsg = y0 + 22 + fromIntegral nRows * 20 + 14
      result = case wFound w of
        Just f -> (17, mint, 14, "Found! " ++ show rn ++ " = " ++ show f ++ " x " ++ show (rn `div` f) ++ " (" ++ show (wI w) ++ " steps)")
        Nothing -> (17, g8 235, 14, "Space: one step (A: auto)")
  in Pictures
       [ Pictures links
       , closing
       , Pictures nodes
       , hoop env coral 4 cix ciy 40
       , hoop env mint 4 yix yiy 48
       , txtC env cx (envH env - 62) 14 (g8 200) ("x_i mod " ++ show p ++ " (seen through hidden factor p): tail " ++ show tl ++ ", loop " ++ show loopLen)
       , txtC env (cx - 110) (envH env - 38) 14 coral "red = current x_i"
       , txtC env (cx + 110) (envH env - 38) 14 mint "green = saved y"
       , txtT env px 112 15 (g8 235) ("n = " ++ show rn ++ ", x1 = 2, x_i = x_(i-1)^2 + 1 mod n")
       , Pictures [txtT env xx y0 14 (g8 150) s | (xx, s) <- zip colsX ["i", "x_i", "y", "gcd(y - x_i, n)"]]
       , Pictures rowsP
       , stack env px yMsg pw
           [ result
           , (14, g8 150, 0, "Mod p there are only p values, so by the birthday paradox the sequence returns after about sqrt(p) steps (the rho shape).\nWhen red and green sit on the same circle, y = x_i (mod p), so y - x_i is a multiple of p and gcd reveals p. Without knowing p!\nTrial division takes about sqrt(n) steps; rho about sqrt(p) <= n^(1/4).")
           ]
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ | not (wRho w) -> pure (setN (min 1201 (wNn w + 2)) w)
  EventKey (SpecialKey KeyLeft) Down _ _ | not (wRho w) -> pure (setN (max 5 (wNn w - 2)) w)
  EventKey (SpecialKey KeySpace) Down _ _ | wRho w -> pure (rhoStep w)
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    '1' -> pure (setN 561 w { wRho = False })
    '2' -> pure (setN 341 w { wRho = False })
    '3' -> pure (setN 97 w { wRho = False })
    '4' -> do
      k <- randomRIO (50, 499)
      pure (setN (k * 2 + 1) w { wRho = False })
    't' -> pure w { wFerm = not (wFerm w) }
    '5' -> pure (initRho 4307 w { wRho = True })
    '6' -> pure (initRho 247021 w { wRho = True })
    'a' -> pure w { wAuto = not (wAuto w) }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 561 = 3 × 11 × 17 は、フェルマーでは互いに素な a 全部がうそつき。ミラー–ラビンでは？
-- 2. ←→ で n を動かして、緑が多い合成数を探す（強い擬素数）
-- 3. ロー法の x²+1 を x²+3 に変えると、ρ の形や歩数はどう変わる？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v
