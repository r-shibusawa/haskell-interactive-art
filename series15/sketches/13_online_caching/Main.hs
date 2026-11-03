-- ============================================
-- 13 オンライン・キャッシング（音つき）
-- 手もとの棚（キャッシュ）には k 冊しか置けない。棚にない本を頼まれたら（ミス）、どれかを捨てて取りに行く
-- どれを捨てる？ LRU = いちばん長く使っていない本 / FIFO = いちばん古く入れた本 / LFU = 使った回数が少ない本 / マーキング = ランダム
-- OPT（ベラディ）= 未来を見て「いちばん先まで使わない本」を捨てる。決定的な方法は最悪 k 倍、マーキングは約 2 ln k 倍
-- スペース = 再生・停止 / 1：ランダム 2：局所性 3：k+1 冊を順番に（LRU・FIFO への意地悪）/ 上下 = 棚の大きさ k / 左右 = 音を聞く戦略 / R
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (delete)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

pages :: String
pages = "ABCDEFGHIJ"

numPages :: Int
numPages = length pages

names :: [String]
names = ["LRU", "FIFO", "LFU", "Marking (rand)", "OPT (sees future)"]

hues :: [Float]
hues = [45, 200, 300, 140, 0]

freqs :: [Float]
freqs = [262, 294, 330, 392, 440, 523, 587, 659, 784, 880]  -- ★ 本ごとの音

data Meta = Meta { mUsed :: !Int, mFreq :: !Int, mInAt :: !Int }

data Lane = Lane
  { lCache :: [Int]
  , lMisses :: !Int
  , lLast :: Maybe Bool       -- True = ヒット
  , lMeta :: !(IM.IntMap Meta)
  , lMarked :: !IS.IntSet
  }

data World = World
  { wK :: !Int
  , wSeq :: !(U.Vector Int)
  , wPos :: !Int
  , wGen :: !Int
  , wLanes :: [Lane]
  , wPlaying :: !Bool
  , wTimer :: !Int
  , wFocus :: !Int
  }

main :: IO ()
main = runSketch (sketch "15-13 Online Caching" (World 4 U.empty 0 2 (map (const lane0) names) False 0 0))
  { skBack  = hsb 15 20 9
  , skStart = \_ -> reset (World 4 U.empty 0 2 [] False 0 0)   -- ★ 最初の棚の大きさ k
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

lane0 :: Lane
lane0 = Lane [] 0 Nothing IM.empty IS.empty

makeSeq :: Int -> Int -> IO (U.Vector Int)
makeSeq gen k = case gen of
  1 -> U.fromList <$> mapM (const (randomRIO (0, numPages - 1))) [1 .. 600 :: Int]
  3 -> pure (U.generate 600 (`mod` (k + 1)))
  _ -> do
    (_, xs) <- foldM local (0, []) [0 .. 599 :: Int]
    pure (U.fromList (reverse xs))
  where
    local (s, acc) i = do
      s' <- if i `mod` 40 == 0 then randomRIO (0, numPages - 1) else pure s
      r <- randomRIO (0, 1 :: Float)
      x <- if r < 0.85
             then (\d -> (s' + d) `mod` numPages) <$> randomRIO (0, k - 1)
             else randomRIO (0, numPages - 1)
      pure (s', x : acc)

reset :: World -> IO World
reset w = do
  s <- makeSeq (wGen w) (wK w)
  pure w { wSeq = s, wPos = 0, wLanes = map (const lane0) names }

-- いちばん小さい / 大きいものを選ぶ（同点なら先のもの）
minBy, maxBy :: Int -> (Int -> Int) -> [Int] -> Int
minBy d f xs = case xs of
  [] -> d
  (x : r) -> foldl (\b y -> if f y < f b then y else b) x r
maxBy d f xs = case xs of
  [] -> d
  (x : r) -> foldl (\b y -> if f y > f b then y else b) x r

-- 戦略 idx で本 x を出す。戻り値はヒットかどうか
serve :: World -> Int -> Lane -> Int -> IO (Lane, Bool)
serve w idx l x
  | x `elem` c =
      let old = IM.findWithDefault (Meta 0 0 0) x (lMeta l)
          meta' = IM.insert x old { mUsed = pos, mFreq = mFreq old + 1 } (lMeta l)
      in pure (l { lMeta = meta', lMarked = if idx == 3 then IS.insert x (lMarked l) else lMarked l }, True)
  | otherwise = do
      (c1, marked1) <-
        if length c >= wK w
          then do
            (victim, marked') <- chooseVictim
            pure (delete victim c, marked')
          else pure (c, lMarked l)
      pure ( l { lCache = c1 ++ [x]
               , lMisses = lMisses l + 1
               , lMeta = IM.insert x (Meta pos 1 pos) (lMeta l)
               , lMarked = if idx == 3 then IS.insert x marked1 else marked1 }
           , False )
  where
    c = lCache l
    pos = wPos w
    meta p = IM.findWithDefault (Meta 0 0 0) p (lMeta l)
    chooseVictim = case idx of
      0 -> pure (minBy x (mUsed . meta) c, lMarked l)
      1 -> pure (minBy x (mInAt . meta) c, lMarked l)
      2 -> pure (minBy x (\p -> mFreq (meta p) * 1000000 + mUsed (meta p)) c, lMarked l)
      3 -> do
        -- マーキング：全部マーク済みなら新しい「フェーズ」。マークのない本からランダムに捨てる
        let un = filter (\p -> not (IS.member p (lMarked l))) c
            (pool, marked') = if null un then (c, IS.empty) else (un, lMarked l)
        q <- randomRIO (0, length pool - 1)
        pure (pool !! q, marked')
      -- ベラディ：次に使われるのがいちばん先の本（もう使われないなら最優先）
      _ -> pure (maxBy x nextUse c, lMarked l)
    nextUse p = case U.elemIndex p (U.drop (pos + 1) (wSeq w)) of
      Just d -> d
      Nothing -> maxBound

tickRequest :: World -> IO World
tickRequest w
  | wPos w >= U.length (wSeq w) = pure w { wPlaying = False }
  | otherwise = do
      let x = wSeq w U.! wPos w
      results <- mapM (\(i, l) -> serve w i l x) (zip [0 ..] (wLanes w))
      let lanes = [l { lLast = Just hit } | (l, hit) <- results]
          focusHit = case drop (wFocus w) results of
            ((_, hit) : _) -> hit
            [] -> False
      -- 音：選んだ戦略がヒットなら本の音、ミスなら低い「ドン」
      if focusHit
        then playTone (freqs !! x) 0.25 0.25 Triangle
        else playTone 80 0.4 0.25 Square
      pure w { wLanes = lanes, wPos = wPos w + 1 }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wPlaying w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in if t `mod` 8 == 0 then tickRequest w' else pure w'
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let cw = 26
      pos = wPos w
      sq = wSeq w
      k = wK w
      fk = fromIntegral k :: Float
      strip q =
        let i = pos + q
            x = 40 + fromIntegral (q + 6) * cw
            c | q < 0 = lum 30
              | q == 0 = hsb 50 90 100
              | otherwise = hsba 0 0 55 0.7
        in if i < 0 || i >= U.length sq then Blank else Pictures
             [ box env c x 92 (cw - 3) 28
             , txtC env (x + cw / 2 - 1) 106 14 (lum 10) [pages !! (sq U.! i)] ]
      optMiss = case drop 4 (wLanes w) of
        (l : _) -> max 1 (lMisses l)
        [] -> 1
      lanePic (i, l) =
        let y = 170 + fromIntegral i * 92
            isF = wFocus w == i
            hue = hues !! i
            slot (s, mp) =
              let x = 280 + fromIntegral (s :: Int) * 62
              in case mp of
                   Just p -> Pictures
                     [ box env (hsb hue 40 (if i == 3 && IS.member p (lMarked l) then 70 else 40)) x y 54 52
                     , txtC env (x + 27) (y + 26) 22 (lum 95) [pages !! p] ]
                   Nothing -> box env (lum 15) x y 54 52
            slots = zip [0 .. k - 1] (map Just (lCache l) ++ repeat Nothing)
            bx = 280 + fk * 62 + 30
            lastC = case lLast l of
              Just True -> hsb 140 70 90
              Just False -> hsb 0 80 95
              Nothing -> lum 25
            m = fromIntegral (lMisses l) :: Float
            rateS, ratioS :: String
            rateS = if pos > 0 then printf "  rate %.1f%%" (100 * m / fromIntegral pos) else ""
            ratioS = if i < 4 then printf "  %.2fx OPT" (m / fromIntegral optMiss) else ""
        in Pictures
             ( label' (hsb hue 60 (if isF then 100 else 75)) (y + 26) (if isF then 19 else 17) ((if isF then "> " else "") ++ (names !! i))
             : dot env lastC (bx, y + 26) 20
             : txt env (bx + 30) (y + 32) 17 (lum 95) ("miss " ++ show (lMisses l) ++ rateS ++ ratioS)
             : map slot slots )
      label' c y s str = txt env 40 (y + s * 0.35) s c str
      advice
        | wGen w == 3 = "3: LRU and FIFO miss every time. OPT misses once per k -> exactly k times. Marking is strong by luck"
        | otherwise = "Fixed rules (LRU, FIFO) are at worst k times OPT. Randomizing hides the next victim from the adversary"
  in Pictures
       [ Pictures (map strip [-6 .. 29])
       , txt env (40 + 6 * cw) 140 14 (lum 60) "<- past    next request    future (only OPT knows) ->"
       , Pictures (map lanePic (zip [0 :: Int ..] (wLanes w)))
       , txt env 20 36 17 (lum 100)
           ("Space: " ++ (if wPlaying w then "stop" else "play") ++ "  1: random  2: locality  3: k+1 in turn  Up/Down: shelf k = "
             ++ show k ++ "  Left/Right: heard lane  R")
       , txt env 20 62 15 (lum 65)
           ("Hit = book note, miss = low note. Requests " ++ show pos ++ " / " ++ show (U.length sq))
       , txt env 20 (envH env - 28) 15 (hsb 45 80 100) advice
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> reset w { wK = min 8 (wK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> reset w { wK = max 2 (wK w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wFocus = (wFocus w + 1) `mod` 5 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wFocus = (wFocus w + 4) `mod` 5 }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> reset w { wGen = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> reset w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3 を選んで、LRU と OPT のミスの比が k になるのを確かめる
-- 2. 2（局所性）では LRU が OPT にかなり近い。ふつうの CPU キャッシュが LRU 風なのはなぜ？
-- 3. 左右で音を聞く戦略を変えて、ミスの「リズム」を聞き比べる

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0
