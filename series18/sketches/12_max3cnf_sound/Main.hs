-- ============================================
-- 12 MAX-3-CNF を聞く（乱択近似）
-- 3 文字の節がたくさん。全部を真にするのは難しいが、「コインで変数を決める」だけで平均 7/8 の節が真になる
-- 各節が偽になるのは 3 文字とも偽のときだけ：確率 (1/2)^3 = 1/8 → 期待値 7/8（最適の 8/7 倍以内の近似）
-- 割り当てを試すたびに音が鳴る：真の節が多いほど高い音。全部真なら和音
-- スペース：再生・停止 / 1：コインで乱択  2：全部の割り当てを総当たり / R：新しい式 / 上下：速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Data.Bits (shiftL, testBit)
import Data.List (intercalate, sortOn)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nVars :: Int
nVars = 8           -- ★ 変数の数（総当たりは 2^nVars 通り）

mClauses :: Int
mClauses = 40       -- ★ 節の数

-- ★ 音階（ペンタトニック）
tones :: [Int]
tones = [0, 2, 4, 7, 9]

data Lit = Lit { lV :: !Int, lNeg :: !Bool }

data World = World
  { wClauses :: [[Lit]]
  , wAsg     :: [Bool]
  , wSat     :: [Bool]
  , wMode    :: !Int
  , wPlaying :: !Bool
  , wTempo   :: !Float
  , wHistory :: [Int]    -- 新しいものが先頭。400 個まで
  , wBest    :: !Int
  , wBruteI  :: !Int
  , wAcc     :: !Float   -- 次の音までの時間を数える
  }

main :: IO ()
main = runSketch (sketch "18-12 Hear MAX-3-CNF" (World [] [] [] 1 False 0.18 [] 0 0 0))
  { skBack  = hsb 290 30 9
  , skStart = \_ -> newFormula (World [] [] [] 1 False 0.18 [] 0 0 0)
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

newFormula :: World -> IO World
newFormula w = do
  cls <- replicateM mClauses $ do
    vs <- shuffle [0 .. nVars - 1]
    mapM (\v -> Lit v <$> chance 0.5) (take 3 vs)
  let asg = replicate nVars False
  pure w { wClauses = cls, wAsg = asg, wSat = evaluate cls asg, wHistory = [], wBest = 0, wBruteI = 0 }

evaluate :: [[Lit]] -> [Bool] -> [Bool]
evaluate cls asg = map (any (\l -> valueOf (lV l) /= lNeg l)) cls
  where
    table = U.fromList asg
    valueOf v = table U.! v

-- 割り当てを 1 つ試して音を鳴らす
trial :: World -> IO World
trial w = do
  (asg, nextI) <- if wMode w == 1
    then do
      a <- replicateM nVars (chance 0.5)
      pure (a, wBruteI w)
    else pure ( [testBit (wBruteI w) i | i <- [0 .. nVars - 1]]
              , (wBruteI w + 1) `mod` (1 `shiftL` nVars) )
  let sat = evaluate (wClauses w) asg
      k = length (filter id sat)
      frac = fromIntegral k / fromIntegral mClauses :: Float
      -- 音：真の節の割合 → 音の高さ
      idx = floor (clampF 0 14 (remap frac 0.6 1 0 14)) :: Int
      (oct, deg) = idx `divMod` 5
      midi = 60 + oct * 12 + sum (take 1 (drop deg tones))
      tempo = wTempo w
  playTone (midiFreq midi) 0.25 (tempo * 1.5) Triangle
  when (k == mClauses) $ do
    playTone (midiFreq (midi + 4)) 0.18 0.8 Sine
    playTone (midiFreq (midi + 7)) 0.18 0.8 Sine
  when (fromIntegral k < fromIntegral mClauses * (0.8 :: Float)) $
    playTone (midiFreq 36) 0.2 tempo Sawtooth   -- 少ないと低いうなり
  pure w { wAsg = asg, wSat = sat, wBruteI = nextI
         , wHistory = take 400 (k : wHistory w)
         , wBest = max (wBest w) k }

stepW :: Env -> Float -> World -> IO World
stepW _ dt w
  | wPlaying w = drain w { wAcc = min 1 (wAcc w + dt) }
  | otherwise = pure w

drain :: World -> IO World
drain w
  | wAcc w >= wTempo w = trial w { wAcc = wAcc w - wTempo w } >>= drain
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let varBox (i, b) =
        let x = 20 + fromIntegral (i :: Int) * 62
        in Pictures
             [ box env (if b then hsb 50 85 100 else hsb 230 30 35) x 60 54 40
             , txtC env (x + 27) 80 14 (hsb 0 0 (if b then 0 else 100)) ("x" ++ show (i + 1) ++ (if b then " T" else " F"))
             ]
      cols = 5                 -- 文字が読めるよう、節のマスは 5 列にする
      cw = min 150 (envW env * 0.58 / fromIntegral cols)
      ch = 44
      clauseCell (c, (cl, s)) =
        let (r, q) = (c :: Int) `divMod` cols
            x = 20 + fromIntegral q * cw
            y = 130 + fromIntegral r * ch
            str = intercalate "|" [(if lNeg l then "~" else "") ++ "x" ++ show (lV l + 1) | l <- cl]
        in Pictures
             [ box env (if s then hsb 140 60 75 else hsb 0 80 95) x y (cw - 6) (ch - 6)
             , txtC env (x + (cw - 6) / 2) (y + (ch - 6) / 2) 14 (hsb 0 0 (if s then 10 else 100)) str
             ]
      -- ヒストグラム
      hx = envW env * 0.62
      hy = 130
      hw = envW env - hx - 30
      hh = envH env * 0.45
      counts = U.accum (+) (U.replicate (mClauses + 1) (0 :: Int)) [(k, 1) | k <- wHistory w]
      mc = max 1 (U.maximum counts)
      lo = floor (fromIntegral mClauses * (0.6 :: Float)) :: Int
      xOf v = remap v (fromIntegral lo) (fromIntegral mClauses + 1) hx (hx + hw)
      bw = hw / fromIntegral (mClauses + 1 - lo)
      bar k =
        let h = fromIntegral (counts U.! k) / fromIntegral mc * hh
        in box env (if k == mClauses then hsb 50 90 100 else hsb 190 60 90) (xOf (fromIntegral k)) (hy + hh - h) (bw - 2) h
      expected = fromIntegral mClauses * 7 / 8 :: Float
      ex = xOf expected
      k0 = length (filter id (wSat w))
      histLen = length (wHistory w)
      avg = if histLen > 0 then fromIntegral (sum (wHistory w)) / fromIntegral histLen else 0 :: Float
      bruteLine
        | wMode w == 2 =
            txt env hx (hy + hh + 66) 15 (hsb 280 50 100)
              ("Brute force " ++ show (wBruteI w) ++ " / " ++ show (1 `shiftL` nVars :: Int) ++ " (one lap: best = optimum)")
        | otherwise = Blank
  in Pictures
       ( map varBox (zip [0 ..] (wAsg w))
      ++ map clauseCell (zip [0 ..] (zip (wClauses w) (wSat w)))
      ++ map bar [lo .. mClauses]
      ++ [ dashed env (hsb 0 0 100) 2 6 6 (ex, hy) (ex, hy + hh)
         , txtC env ex (hy + hh + 14) 14 (hsb 0 0 100) ("7/8 = " ++ fix 1 expected)
         , txtC env (hx + hw / 2) (hy + hh + 38) 14 (hsb 0 0 60) "Clauses true"
         , txt env 20 34 16 (hsb 0 0 100) "Space: play/stop   1: coin flips   2: brute force   R: new formula   Up/Down: speed"
         , txt env hx (hy - 50) 18 (hsb 45 85 100) ("Now: " ++ show k0 ++ " / " ++ show mClauses ++ " clauses true")
         , txt env hx (hy - 20) 18 (hsb 0 0 90) ("Avg " ++ fix 2 avg ++ " (" ++ show histLen ++ " runs)   Best " ++ show (wBest w))
         , bruteLine
         , txt env 20 (envH env - 22) 15 (hsb 0 0 60) "The average settles near 7/8 (linearity of expectation): a randomized 8/7-approximation"
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure w { wPlaying = not (wPlaying w), wAcc = max 0 (wTempo w - 0.05) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wTempo = max 0.04 (wTempo w * 0.7) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wTempo = min 0.6 (wTempo w / 0.7) }
  EventKey (Char '1') Down _ _ -> pure w { wMode = 1, wHistory = [] }
  EventKey (Char '2') Down _ _ -> pure w { wMode = 2, wHistory = [], wBruteI = 0, wBest = 0 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> newFormula w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. しばらく鳴らして、平均が 7/8 に近づくのを確かめよう
-- 2. mClauses を 80 にすると、全部真（和音）はほとんど鳴らなくなる。なぜ？
-- 3. tones を [0, 3, 5, 7, 10]（マイナー・ペンタ）にして雰囲気を変えよう

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 破線（線の長さ on、すき間 off）
dashed :: Env -> Color -> Float -> Float -> Float -> Point -> Point -> Picture
dashed env c t on off a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      p d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      starts = takeWhile (< l) (iterate (+ (on + off)) 0)
  in if l < 1 then Blank else Pictures [seg env c t (p d) (p (min l (d + on))) | d <- starts]

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- ---------- 道具：その他 ----------
-- リストをランダムに並べかえる（p5.js の shuffle）
shuffle :: [a] -> IO [a]
shuffle xs = do
  ks <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortOn fst (zip ks xs)))

-- 確率 p で True
chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""
