-- ============================================
-- 13 2 つのメロディの共通部分を聞く（LCS × 音）
-- 同じ元のメロディから、音を足したり消したり変えたりした 2 つの「変奏」をつくる
-- LCS（03）で共通の骨格を取り出すと、元のメロディが聞こえてくる。曲の類似度・盗作検出・楽譜の比較にも使われる
-- 1 = メロディ A / 2 = メロディ B / 3 = 共通の骨格 / スペース = 3 つ続けて / 音符をクリック = 半音上げる（Shift で下げる）/ R = 新しい変奏
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_)
import Data.Array
import qualified Data.Set as Set
import Text.Printf (printf)

scaleSteps :: [Int]
scaleSteps = [0, 2, 4, 7, 9]      -- ★ 使う音階（ペンタトニック）。[0, 2, 4, 5, 7, 9, 11] で長音階

baseLen :: Int
baseLen = 10                      -- ★ 元のメロディの長さ

tempo :: Float
tempo = 0.22                      -- ★ 1 音の長さ（秒）

-- 再生の予定：どの列の何番目を、いつ鳴らすか
data Plan = Plan { pWhich :: !Char, pK :: !Int, pV :: !Int, pT :: !Float }

data World = World
  { wA     :: ![Int]
  , wB     :: ![Int]
  , wLcsA  :: ![Int]
  , wLcsB  :: ![Int]
  , wTab   :: !(Array (Int, Int) Int)
  , wPlan  :: ![Plan]    -- 光らせるための全部の予定
  , wQueue :: ![Plan]    -- まだ鳴らしていない予定
  }

main :: IO ()
main = runSketch (sketch "12-13 Common melody (LCS)" (computeLCS (World [4] [4] [] [] (listArray ((0, 0), (0, 0)) [0]) [] [])))
  { skBack  = cBg
  , skStart = \_ -> makeMelodies (computeLCS (World [4] [4] [] [] (listArray ((0, 0), (0, 0)) [0]) [] []))
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 音階の番号を周波数に
freqOf :: Int -> Float
freqOf idx =
  let len = length scaleSteps
      midi = 60 + 12 * (idx `div` len) + nth 0 scaleSteps (idx `mod` len)
  in 440 * 2 ** ((i2f midi - 69) / 12)

clampNote :: Int -> Int
clampNote = max 0 . min 9

makeMelodies :: World -> IO World
makeMelodies w = do
  (_, base) <- foldM (\(cur, acc) _ -> do
                         d <- randomRIO (-2, 2)
                         let c = clampNote (cur + d)
                         pure (c, acc ++ [c])) (4, []) [1 .. baseLen]
  a <- vary base
  b <- vary base
  pure (computeLCS w { wA = a, wB = b })

-- 変奏：ところどころ消す・足す・変える
vary :: [Int] -> IO [Int]
vary base = do
  parts <- mapM one base
  let out = concat parts
  pure (if null out then [4] else out)
  where
    one v = do
      r <- randomRIO (0, 1 :: Float)
      if r < 0.15 then pure []                                                -- 消す
        else if r < 0.3 then (\d -> [clampNote (v + d)]) <$> randomRIO (-2, 2)  -- 変える
        else do
          u <- randomRIO (0, 1 :: Float)
          if u < 0.2 then (\d -> [v, clampNote (v + d)]) <$> randomRIO (-3, 3)  -- 足す
                     else pure [v]

-- ★ LCS の表を作り、右下から逆にたどって共通の音を取り出す
computeLCS :: World -> World
computeLCS w = w { wTab = t, wLcsA = map fst pairs, wLcsB = map snd pairs }
  where
    as = wA w
    bs = wB w
    m = length as
    n = length bs
    t = array ((0, 0), (m, n)) [((i, j), f i j) | i <- [0 .. m], j <- [0 .. n]]
    f i j
      | i == 0 || j == 0 = 0
      | nth 0 as (i - 1) == nth 0 bs (j - 1) = t ! (i - 1, j - 1) + 1
      | otherwise = max (t ! (i - 1, j)) (t ! (i, j - 1))
    pairs = back m n []
    back i j acc
      | i <= 0 || j <= 0 = acc
      | nth 0 as (i - 1) == nth 0 bs (j - 1) = back (i - 1) (j - 1) ((i - 1, j - 1) : acc)
      | t ! (i - 1, j) >= t ! (i, j - 1) = back (i - 1) j acc
      | otherwise = back i (j - 1) acc

-- 再生の予定を作る（p5.js 版の Web Audio の予約の代わりに、時刻が来たら step で鳴らす）
playSeq :: Env -> String -> World -> World
playSeq env order w = w { wPlan = plan, wQueue = plan }
  where
    seqOf 'A' = zip (wA w) [0 ..]
    seqOf 'B' = zip (wB w) [0 ..]
    seqOf _ = [(nth 0 (wA w) k, k) | k <- wLcsA w]
    plan = go order (envTime env + 0.05)
    go [] _ = []
    go (c : cs) t =
      let notes = seqOf c
          count = i2f (length notes)
      in [Plan c k v (t + i2f i * tempo) | (i, (v, k)) <- zip [0 :: Int ..] notes]
         ++ go cs (t + count * tempo + tempo * 2)

step :: Env -> Float -> World -> IO World
step env _ w = do
  let (due, later) = span ((<= envTime env) . pT) (wQueue w)
  forM_ due $ \p ->
    if pWhich p == 'L'
      then playTone (freqOf (pV p)) 0.16 (tempo * 0.95) Square
      else playTone (freqOf (pV p)) 0.3 (tempo * 0.95) Triangle
  pure w { wQueue = later }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char '1') Down _ _ -> pure (playSeq env "A" w)
  EventKey (Char '2') Down _ _ -> pure (playSeq env "B" w)
  EventKey (Char '3') Down _ _ -> pure (playSeq env "L" w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (playSeq env "ABL" w)
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    w1 <- makeMelodies w
    pure w1 { wPlan = [], wQueue = [] }
  EventKey (MouseButton LeftButton) Down m _ -> do
    let (mx, my) = mouseP env
        d = if shift m == Down then -1 else 1
        nw = noteW env w
        k = floor ((mx - rollLeft) / nw) :: Int
        edit mel y0
          | my < y0 - 10 || my > y0 + 160 || k < 0 || k >= length mel = (mel, Nothing)
          | otherwise =
              let v = clampNote (nth 0 mel k + d)
              in ([if i == k then v else x | (i, x) <- zip [0 ..] mel], Just v)
        (a, ha) = edit (wA w) 110
        (b, hb) = edit (wB w) 330
    forM_ (ha ++? hb) $ \v -> playTone (freqOf v) 0.3 0.25 Triangle
    pure (computeLCS w { wA = a, wB = b })
  _ -> pure w
  where
    ha ++? hb = maybe [] pure ha ++ maybe [] pure hb

rollLeft :: Float
rollLeft = 80

noteW :: Env -> World -> Float
noteW env w = (envW env - 120) / i2f (max 1 (max (length (wA w)) (length (wB w))))

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 "Hear the common part of two melodies (LCS)"
    , txt 15 cMuted 32 60 "1: A   2: B   3: common   Space: all three   Click note: up (Shift: down)   R: new variations" ]
  ++ drawRoll env w (wA w) 110 "A" cCyan (wLcsA w) cur
  ++ drawRoll env w (wB w) 330 "B" cRasp (wLcsB w) cur
  ++ links
  ++ [ txt 17 cFg 80 500
         ("A " ++ show lenA ++ " notes   B " ++ show lenB ++ " notes   common " ++ show lenL ++ " notes  ->  similarity "
          ++ fixN 0 (2 * i2f lenL / i2f (max 1 (lenA + lenB)) * 100) ++ "% (2 LCS / sum of lengths)") ]
  ++ table )
  where
    wd = envW env
    ht = envH env
    now = envTime env
    cur = case [p | p <- wPlan w, now >= pT p, now < pT p + tempo] of
      (p : _) -> Just p
      [] -> Nothing
    lenA = length (wA w)
    lenB = length (wB w)
    lenL = length (wLcsA w)
    nw = noteW env w
    -- 共通の音を線でつなぐ
    links =
      [ seg cLime (if lit then 4 else 1.5) (rollLeft + i2f ka * nw + nw / 2, 262) (rollLeft + i2f kb * nw + nw / 2, 328)
      | (ka, kb) <- zip (wLcsA w) (wLcsB w)
      , let lit = case cur of
              Just p -> pWhich p == 'L' && pK p == ka
              Nothing -> False ]
    -- 小さな LCS 表
    m = lenA
    n = lenB
    best = wTab w ! (m, n)
    cell = minimum [26, (ht - 560) / i2f (m + 1), (wd - 160) / i2f (n + 1)]
    onPath = Set.fromList [(a + 1, b + 1) | (a, b) <- zip (wLcsA w) (wLcsB w)]
    table
      | cell <= 8 = []
      | otherwise = concat
          [ rbox (if on then cLime else mixC cPanel (rgb255 0x6b 0x3a 0x55) (i2f v / i2f (max 1 best))) x y (cell - 2) (cell - 2)
            : [txtC 14 (if on then cBg else cMuted) (x + cell / 2 - 1) (y + cell / 2 - 1) (show v) | cell > 17]
          | i <- [1 .. m], j <- [1 .. n]
          , let x = 80 + i2f (j - 1) * cell
          , let y = 540 + i2f (i - 1) * cell
          , let v = wTab w ! (i, j)
          , let on = Set.member (i, j) onPath ]

drawRoll :: Env -> World -> [Int] -> Float -> String -> Color -> [Int] -> Maybe Plan -> [Picture]
drawRoll env w mel y0 name col used cur =
  [ rbox cPanel (x0 - 10) (y0 - 10) (rw + 20) (h + 20)
  , txtR 22 col (x0 - 22) (y0 + h / 2) name ]
  ++ [ rbox (if lit k then cFg else if k `elem` used then cLime else col) (x0 + i2f k * nw + 3) y (nw - 6) (h / 11 - 2)
     | (k, v) <- zip [0 ..] mel
     , let y = y0 + h - i2f (v + 1) * (h / 11) ]
  where
    x0 = rollLeft
    rw = envW env - 120
    h = 150
    nw = noteW env w
    lit k = case cur of
      Just p -> pK p == k && ([pWhich p] == name || (pWhich p == 'L' && name == "A"))
      Nothing -> False

-- ---------- やってみよう ----------
-- 1. scaleSteps を長音階に変えて、聞こえ方を比べる
-- 2. vary の確率を大きくすると、似ている度はどこまで下がる？
-- 3. 音符をクリックして、共通の骨格が一番長くなるように直す

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

-- | マウスの位置を左上原点の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 線分（太さ t）
seg :: Color -> Float -> Point -> Point -> Picture
seg c t (x1, y1) (x2, y2)
  | t <= 1.2  = Color c (Line [(x1, -y1), (x2, -y2)])
  | otherwise = Color c (thickLine t (x1, -y1) (x2, -y2))

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 文字：(x, y) を右端の真ん中にして書く
txtR :: Float -> Color -> Float -> Float -> String -> Picture
txtR size c x y s = label (x - fromIntegral (length s) * size * 0.8) (-y - size / 2) size c s

-- | 2 色を混ぜる（p5.js の lerpColor）
mixC :: Color -> Color -> Float -> Color
mixC a b t0 =
  let t = clampF 0 1 t0
      (r1, g1, b1, _) = rgbaOfColor a
      (r2, g2, b2, _) = rgbaOfColor b
  in makeColor (lerpF r1 r2 t) (lerpF g1 g2 t) (lerpF b1 b2 t) 1

-- | 小数 n 桁の文字列（p5.js の toFixed）
fixN :: Int -> Float -> String
fixN n = printf ("%." ++ show n ++ "f")

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
