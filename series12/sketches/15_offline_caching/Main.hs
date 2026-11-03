-- ============================================
-- 15 オフライン・キャッシング：未来がわかるなら、どれを捨てる？
-- キャッシュ（小さくて速い置き場）は k 個しか入らない。いっぱいのとき何を追い出すかで「ミス」の回数が変わる
-- 未来の要求が全部わかっているなら「いちばん先まで使わないもの」を捨てる貪欲法が最適（Belady のアルゴリズム）
-- スペース = 再生・停止 / → = 1 手 / ↑↓ = キャッシュの大きさ k / 1〜3 = お手本 / R = ランダムな要求列
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (elemIndex, foldl', findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

sample1 :: String
sample1 = "adbcaebdfacbgdaecbad"  -- ★ 要求の並び

startK :: Int
startK = 3                        -- ★ キャッシュに入る個数

framesPerMove :: Int
framesPerMove = 28                -- ★ 1 手の間のフレーム数

-- ある時点のキャッシュの様子
data Col = Col
  { cCache   :: ![Char]
  , cHit     :: !Bool
  , cEvicted :: !(Maybe Char)
  , cSlot    :: !Int
  , cMisses  :: !Int
  }

data World = World
  { wReq     :: !String
  , wK       :: !Int
  , wT       :: !Int
  , wPlaying :: !Bool
  , wTimer   :: !Int
  , wSims    :: ![[Col]]     -- FIFO, LRU, Belady の順に、最後まで計算した結果
  }

initial :: World
initial = simulateAll (World sample1 startK 0 True 0 [])

main :: IO ()
main = runSketch (sketch "12-15 Offline caching" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> onEvent ev w
  }

policyName :: Int -> String
policyName p = case p of
  0 -> "FIFO (oldest in)"
  1 -> "LRU (least recent)"
  _ -> "Belady (farthest use)"

policyColor :: Int -> Color
policyColor p = case p of
  0 -> rgb255 0xc7 0x92 0xff
  1 -> cCyan
  _ -> cLime

-- 時刻 i より後で、次に b を使う時刻（もう使わないなら Nothing）
nextUse :: String -> Char -> Int -> Maybe Int
nextUse req b i = (\j -> i + 1 + j) <$> elemIndex b (drop (i + 1) req)

-- 比べやすいように、もう使わないものは「とても先」にする
nextUseKey :: String -> Char -> Int -> Int
nextUseKey req b i = fromMaybe maxBound (nextUse req b i)

-- 1 つの方針で最後まで計算する
data SimState = SimState
  { sCache :: ![Char]
  , sIn    :: !(M.Map Char Int)
  , sLast  :: !(M.Map Char Int)
  , sMiss  :: !Int
  , sCols  :: ![Col]
  }

-- ★ 追い出す相手を選ぶ。ここが 3 つの方針のちがい
chooseVictim :: String -> Int -> SimState -> Int -> Char
chooseVictim req p st i = foldl1' pick (sCache st)
  where
    foldl1' f xs = case xs of
      (x : rest) -> foldl' f x rest
      [] -> ' '
    inT c = M.findWithDefault 0 c (sIn st)
    lastT c = M.findWithDefault 0 c (sLast st)
    pick a b = case p of
      0 -> if inT a <= inT b then a else b
      1 -> if lastT a <= lastT b then a else b
      _ -> if nextUseKey req a i >= nextUseKey req b i then a else b

simulateOne :: String -> Int -> Int -> [Col]
simulateOne req k p = reverse (sCols (foldl' visit (SimState [] M.empty M.empty 0 []) (zip [0 ..] req)))
  where
    visit st (i, r) =
      let hit = r `elem` sCache st
          full = length (sCache st) >= k
          victim = chooseVictim req p st i
          cache1
            | hit = sCache st
            | full = [if c == victim then r else c | c <- sCache st]
            | otherwise = sCache st ++ [r]
          evicted = if not hit && full then Just victim else Nothing
          miss = if hit then sMiss st else sMiss st + 1
          inT = if hit then sIn st else M.insert r i (sIn st)
          col = Col cache1 hit evicted (fromMaybe (-1) (elemIndex r cache1)) miss
      in st { sCache = cache1, sIn = inT, sLast = M.insert r i (sLast st), sMiss = miss, sCols = col : sCols st }

simulateAll :: World -> World
simulateAll w = w { wSims = [simulateOne (wReq w) (wK w) p | p <- [0, 1, 2]], wT = 0 }

step :: World -> World
step w
  | wPlaying w && wT w < length (wReq w) =
      if wTimer w + 1 >= framesPerMove then w { wTimer = 0, wT = wT w + 1 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> toggle
  EventKey (MouseButton LeftButton) Down _ _ -> toggle
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wT = min (length (wReq w)) (wT w + 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (simulateAll w { wK = min 6 (wK w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (simulateAll w { wK = max 1 (wK w - 1) })
  EventKey (Char '1') Down _ _ -> preset sample1
  EventKey (Char '2') Down _ _ -> preset "abcdabcdabcdabcd"   -- LRU と FIFO が毎回ミスする意地悪な列（k = 3）
  EventKey (Char '3') Down _ _ -> preset "aabacadaeafagaha"   -- 「a」だけよく使う
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    req <- replicateM 22 $ do
      u <- randomRIO (0, 1 :: Float)
      nth 'a' "abcdefg" <$> randomRIO (0, if u < 0.5 then 2 else 6)
    preset req
  _ -> pure w
  where
    toggle = pure w { wPlaying = not (wPlaying w) }
    preset req = pure (simulateAll w { wReq = req }) { wPlaying = True }

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 ("Which item to evict? cache size k = " ++ show k)
    , txt 15 cMuted 32 60 "Space: play/pause   Right: 1 step   Up/Down: k   1-3: samples   R: random   Click: pause"
    , txtR 14 cMuted (x0 - 10) 108 "requests ->" ]
  ++ [ txtC (max 14 (min 20 (cw * 0.55))) (if i == t - 1 then cAmber else if i < t then cFg else cMuted) (x0 + i2f i * cw + cw / 2) 108 [r]
     | (i, r) <- zip [0 ..] req ]
  ++ concat (zipWith3 policy [0 ..] tops (wSims w))
  ++ reason )
  where
    wd = envW env
    ht = envH env
    k = wK w
    t = wT w
    req = wReq w
    n = length req
    cw = min 46 ((wd - 380) / i2f (max 1 n))
    x0 = 340
    blockH = min 34 ((ht - 220) / (3 * (i2f k + 1.6)))
    tops = iterate (+ ((i2f k + 1.6) * blockH + 10)) 140
    emptyCol = Col [] True Nothing (-1) 0
    bestMiss = cMisses (nth emptyCol (nth [] (wSims w) 2) (n - 1))
    policy :: Int -> Float -> [Col] -> [Picture]
    policy p y cols =
      let miss = if t > 0 then cMisses (nth emptyCol cols (t - 1)) else 0
          finalMiss = cMisses (nth emptyCol cols (n - 1))
      in [ txt 15 (policyColor p) 32 (y + 4) (policyName p)
         , txt 22 cFg 32 (y + 30) ("misses " ++ show miss) ]
         ++ [ txt 14 cMuted 32 (y + 60) (if p == 2 then "<- the minimum" else "minimum +" ++ show (finalMiss - bestMiss)) | t >= n ]
         ++ concat
            [ rbox cPanel (x + 1) (yy + 1) (cw - 2) (blockH - 2)
              : concat
                [ [ rbox (if isReq then (if cHit c then policyColor p else cRasp) else rgb255 0x3b 0x2f 0x47)
                         (x + 2) (yy + 2) (cw - 4) (blockH - 4)
                  , txtC 14 (if isReq then cBg else cFg) (x + cw / 2) (yy + blockH / 2) [item] ]
                | i < t
                , let c = nth emptyCol cols i
                , Just item <- [slotOf c s]
                , let isReq = cSlot c == s ]
            | s <- [0 .. k - 1], i <- [0 .. n - 1]
            , let x = x0 + i2f i * cw
            , let yy = y + i2f s * blockH ]
         -- ミスの印
         ++ concat
            [ [circ cRasp cx (y + i2f k * blockH + 8) 7 | not (cHit c)]
              ++ [txtC 14 cMuted cx (y + i2f k * blockH + 22) ['x', e] | Just e <- [cEvicted c]]
            | (i, c) <- zip [0 .. t - 1] cols
            , let cx = x0 + i2f i * cw + cw / 2 ]
    slotOf c s = case drop s (cCache c) of
      (x : _) -> Just x
      [] -> Nothing
    -- Belady の判断理由
    belady = nth [] (wSims w) 2
    reason = case (t > 0 && t <= n, cEvicted (nth emptyCol belady (t - 1))) of
      (True, Just e) ->
        let i = t - 1
            prev = if i > 0 then cCache (nth emptyCol belady (i - 1)) else []
            part b = [b] ++ ": " ++ case nextUse req b i of
              Nothing -> "never"
              Just j -> "in " ++ show (j - i)
        in [txt 14 cLime 32 (ht - 36) ("Belady dropped " ++ [e] ++ ".  Next use  " ++ concatMap ((++ "   ") . part) prev)]
      _ -> []

-- ---------- やってみよう ----------
-- 1. sample1 を書きかえて、LRU が Belady と同じ回数になる列を探す
-- 2. 2 のお手本で k を 4 にすると、ミスはどう減る？
-- 3. chooseVictim に「いちばん最近使ったものを捨てる」方針を足してみる

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

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 円（p5.js の circle と同じく d は直径）
circ :: Color -> Float -> Float -> Float -> Picture
circ c x y d = Color c (Translate x (-y) (disc (d / 2)))

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 文字：(x, y) を右端の真ん中にして書く
txtR :: Float -> Color -> Float -> Float -> String -> Picture
txtR size c x y s = label (x - fromIntegral (length s) * size * 0.8) (-y - size / 2) size c s

-- | リストの i 番目（範囲外なら d）
nth :: a -> [a] -> Int -> a
nth d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
