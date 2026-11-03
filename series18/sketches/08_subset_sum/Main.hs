-- ============================================
-- 08 部分和問題（SUBSET-SUM）
-- 数の集まりから何個か選んで、合計をちょうど t にできる？
-- モード 1：表（動的計画法）で解く。手間は「個数 × t」。でも t は桁数に対して指数的に大きくなれる（擬多項式時間）
-- モード 2：3-CNF-SAT → 部分和 の還元。式を「桁ごとの数」に変身させる。繰り上がりが起きないよう各桁は小さく保つ
-- 1：表で解く  2：還元を見る / スペース：再生・停止 / モード 1：上下キーで t、R で新しい数  モード 2：変数をクリックで真偽
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.IntMap.Strict as IM
import Data.List (find, intercalate)

data World = World
  { wMode   :: !Int
    -- ---- モード 1 ----
  , wNums   :: [Int]
  , wTarget :: !Int
  , wCell   :: !Int
  , wPlay   :: !Bool
  , wPath   :: [(Int, Int, Bool)]    -- たどったマス（行、合計、その数を使ったか）
  , wTbl    :: !(IM.IntMap Bool)     -- 埋めたマス（行 × 列の番号）
    -- ---- モード 2 ----
  , wAsg    :: [Bool]
  , wBrute  :: !Int
  }

nV :: Int
nV = 3

-- ★ 例：x1, x2, x3 の 4 節
cls :: [[Int]]
cls = [[1, 2, -3], [-1, 2, 3], [-1, -2, -3], [1, -2, 3]]

main :: IO ()
main = runSketch (sketch "18-08 Subset Sum" initialWorld)
  { skBack  = hsb 330 25 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World 1 [3, 34, 4, 12, 5, 2] 9 0 True [] IM.empty [True, False, True] (-1)

resetDP :: World -> World
resetDP w = w { wTbl = IM.empty, wCell = 0, wPath = [] }

-- 1 マス埋める
fillOne :: World -> World
fillOne w =
  let nums = wNums w
      target = wTarget w
      cols = target + 1
      n = length nums
      total = (n + 1) * cols
      c = wCell w
      (i, s) = c `divMod` cols
      tbl = wTbl w
      get r k = IM.findWithDefault False (r * cols + k) tbl
      num r = sum (take 1 (drop (r - 1) nums))
      v | i == 0 = s == 0
        | otherwise = get (i - 1) s || (s >= num i && get (i - 1) (s - num i))
      tbl' = IM.insert c v tbl
      get' r k = IM.findWithDefault False (r * cols + k) tbl'
      -- 選んだ数をたどる
      trace r s2
        | r <= 0 = []
        | not (get' (r - 1) s2) = (r, s2, True) : trace (r - 1) (s2 - num r)
        | otherwise = (r, s2, False) : trace (r - 1) s2
  in if c >= total
       then w
       else w { wTbl = tbl', wCell = c + 1
              , wPath = if c + 1 == total && get' n target then trace n target else wPath w }

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wMode w == 1 = pure (if wPlay w then iterate fillOne w !! 3 else w)
  | wBrute w >= 0 && envFrame env `mod` 40 == 0 =
      let b = wBrute w
      in pure $ if b < 1 `shiftL` nV
           then w { wAsg = [(b `shiftR` (nV - 1 - i)) .&. 1 == 0 | i <- [0 .. nV - 1]], wBrute = b + 1 }
           else w { wBrute = -1 }
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w = if wMode w == 1 then drawDP env w else drawReduction env w

drawDP :: Env -> World -> Picture
drawDP env w =
  let nums = wNums w
      target = wTarget w
      cols = target + 1
      rows = length nums + 1
      x0 = 110
      y0 = 110
      cw = min 46 ((envW env - x0 - 30) / fromIntegral cols)
      ch = min 46 ((envH env - y0 - 120) / fromIntegral rows)
      colX s = x0 + fromIntegral s * cw
      rowY i = y0 + fromIntegral (i :: Int) * ch
      heads = [txtC env (colX s + cw / 2) (y0 - 12) 14 (hsb 0 0 60) (show s) | s <- [0 .. target]]
      rowLabel i =
        txtR env (x0 - 8) (rowY i + ch / 2) 14 (hsb 0 0 70)
          (if i == 0 then "none" else "up to " ++ concatMap show (take 1 (drop (i - 1) nums)))
      cell i s =
        let v = IM.lookup (i * cols + s) (wTbl w)
            onPath = find (\(r, k, _) -> r == i && k == s) (wPath w)
            base = case v of
              Nothing -> hsb 0 0 14
              Just True -> hsb 150 70 80
              Just False -> hsb 0 0 24
            col | i * cols + s == wCell w = hsb 0 0 100
                | Just (_, _, used) <- onPath = if used then hsb 45 90 100 else hsb 190 60 90
                | otherwise = base
            mark = case v of
              Just b | cw > 22 ->
                txtC env (colX s + cw / 2) (rowY i + ch / 2) 14 (hsb 0 0 (if b then 10 else 50)) (if b then "O" else "x")
              _ -> Blank
        in Pictures [box env col (colX s + 1) (rowY i + 1) (cw - 2) (ch - 2), mark]
      y = rowY rows + 40
      okNow = IM.findWithDefault False ((rows - 1) * cols + target) (wTbl w)
      used = [concatMap show (take 1 (drop (r - 1) nums)) | (r, _, True) <- wPath w]
      result
        | wCell w >= cols * rows =
            if okNow
              then txt env 20 y 18 (hsb 120 80 100)
                     ("Possible! " ++ intercalate " + " used ++ " = " ++ show target ++ " (yellow cells used a number)")
              else txt env 20 y 18 (hsb 0 80 100) "Impossible (bottom-right cell is x)"
        | otherwise = Blank
  in Pictures
       ( heads
      ++ map rowLabel [0 .. rows - 1]
      ++ [cell i s | i <- [0 .. rows - 1], s <- [0 .. target]]
      ++ [ txt env 20 34 16 (hsb 0 0 100) "1: table   2: reduction   Space: play/stop   Up/Down: change t   R: new numbers"
         , txt env 20 72 20 (hsb 45 80 100)
             ("Numbers {" ++ intercalate ", " (map show nums) ++ "}: can they sum to t = " ++ show target ++ "?")
         , result
         , txt env 20 (y + 30) 15 (hsb 0 0 60) "Cell (i, s) = can the first i numbers make sum s? = cell above OR cell above at (s - number)."
         , txt env 20 (y + 54) 15 (hsb 0 0 60)
             ("Work: " ++ show rows ++ " x " ++ show cols ++ " = " ++ show (rows * cols) ++ " cells.")
         , txt env 20 (y + 78) 15 (hsb 0 0 60) "If t has 30 digits the table has 10^30 columns: not polynomial in the input length,"
         , txt env 20 (y + 102) 15 (hsb 0 0 60) "so this does not contradict NP-completeness."
         ] )

-- ---------- モード 2：3-CNF → 部分和 ----------
data Row = Row
  { rName :: String
  , rD    :: [Int]              -- 桁
  , rVar  :: Maybe (Int, Bool)  -- 変数の行：番号と否定かどうか
  , rSl   :: Maybe (Int, Bool)  -- おまけの行：節の番号と「2」かどうか
  }

rowsOf :: [Row]
rowsOf =
  let k = length cls
      varRow i neg =
        let lit = if neg then negate i else i
            xs = [if c == i then 1 else 0 | c <- [1 .. nV]]
            cs = [if lit `elem` cl then 1 else 0 | cl <- cls]
        in Row ("v" ++ show i ++ (if neg then "'" else "")) (xs ++ cs) (Just (i, neg)) Nothing
      slackRow j two =
        let d = replicate nV 0 ++ [if c == j then (if two then 2 else 1) else 0 | c <- [0 .. k - 1]]
        in Row ("s" ++ show (j + 1) ++ (if two then "'" else "")) d Nothing (Just (j, two))
  in [varRow i neg | i <- [1 .. nV], neg <- [False, True]]
       ++ [slackRow j two | j <- [0 .. k - 1], two <- [False, True]]

-- 節の桁を 4 にするよう、おまけの数 s, s' を足す
chooseRows :: [Bool] -> [Row] -> [Bool]
chooseRows asg rows =
  let isVarPick r = case rVar r of
        Just (i, neg) -> or (take 1 (drop (i - 1) asg)) /= neg
        Nothing -> False
      varPick = map isVarPick rows
      clauseSum j = sum [sum (take 1 (drop (nV + j) (rD r))) | (r, True) <- zip rows varPick]
      finalPick r vp = case rSl r of
        Just (j, two) -> case 4 - clauseSum j of
          3 -> True
          2 -> two
          1 -> not two
          _ -> False
        Nothing -> vp
  in zipWith finalPick rows varPick

drawReduction :: Env -> World -> Picture
drawReduction env w =
  let k = length cls
      rows = rowsOf
      asg = wAsg w
      pick = chooseRows asg rows
      x0 = 140
      y0 = 150
      digits = nV + k
      cw = min 70 ((envW env * 0.6 - x0) / fromIntegral digits)
      ch = min 34 ((envH env - 260) / fromIntegral (length rows + 3))
      digX c = x0 + fromIntegral (c :: Int) * cw + cw / 2
      truth l = or (take 1 (drop (abs l - 1) asg)) == (l > 0)
      -- 式
      formula = go 20 (zip [1 :: Int ..] cls)
        where
          go _ [] = []
          go fx ((j, c) : r) =
            let s = "C" ++ show j ++ "=(" ++ intercalate "|" (map lit c) ++ ")  "
                sat = any truth c
            in txt env fx 72 18 (if sat then hsb 130 60 95 else hsb 0 70 100) s
                 : go (fx + fromIntegral (length s) * 18 * 0.8) r
          lit l = (if l > 0 then "x" else "~x") ++ show (abs l)
      -- 変数ボタン
      button (i, b) =
        let bx = 20 + fromIntegral (i :: Int) * 120
        in Pictures
             [ box env (if b then hsb 50 80 100 else hsb 220 30 40) bx 90 108 34
             , txtC env (bx + 54) 107 16 (hsb 0 0 (if b then 0 else 100)) ("x" ++ show (i + 1) ++ " = " ++ (if b then "T" else "F"))
             ]
      heads = ["x" ++ show i | i <- [1 .. nV]] ++ ["C" ++ show j | j <- [1 .. k]]
      headLabel (c, h) = txtC env (digX c) y0 16 (if c < nV then hsb 50 70 100 else hsb 190 60 100) h
      rowPic (ri, (r, p)) =
        let y = y0 + fromIntegral (ri + 1 :: Int) * ch
        in Pictures
             ( box env (if p then hsb 45 70 40 else hsb 0 0 13) 20 (y - ch / 2 + 2) (x0 - 30 + fromIntegral digits * cw + 150) (ch - 4)
             : txtM env 30 y 16 (if p then hsb 45 80 100 else hsb 0 0 70) (rName r)
             : txtM env (x0 + fromIntegral digits * cw + 10) y 15 (hsb 0 0 55) ("= " ++ concatMap show (rD r))
             : [ txtC env (digX c) y 16 (if v /= 0 then hsb 0 0 (if p then 100 else 85) else hsb 0 0 35) (show v)
               | (c, v) <- zip [0 ..] (rD r) ] )
      sumD = foldr (zipWith (+)) (replicate digits 0) [rD r | (r, True) <- zip rows pick]
      tD = replicate nV 1 ++ replicate k 4
      yS = y0 + (fromIntegral (length rows) + 1.5) * ch
      yT = yS + ch
      okAll = sumD == tD
      ex = envW env * 0.66
      notes =
        [ "- Pick v_i (true) or v_i' (false):"
        , "  digit x_i becomes 1"
        , "- Digit C_j = number of true"
        , "  literals in the clause (1-3)"
        , "- Extras s=1, s'=2 bring it to 4"
        , "  (0 true literals can't reach 4)"
        , "- Each digit <= 6: no carries"
        , ""
        , "Count and digits grow with the"
        , "formula: polynomial reduction"
        ]
      noteRow (i, s) = txt env ex (y0 + 16 + fromIntegral (i :: Int) * 26) 15 (hsb 0 0 85) s
  in Pictures
       ( formula
      ++ map button (zip [0 ..] asg)
      ++ map headLabel (zip [0 ..] heads)
      ++ map rowPic (zip [0 ..] (zip rows pick))
      ++ [ txtM env 20 yS 16 (hsb 0 0 100) "Sum"
         , txtM env 20 yT 16 (hsb 0 0 100) "Target t"
         ]
      ++ [txtC env (digX c) yS 17 (if v == t then hsb 120 70 100 else hsb 0 80 100) (show v) | (c, v, t) <- zip3 [0 ..] sumD tD]
      ++ [txtC env (digX c) yT 17 (hsb 45 80 100) (show v) | (c, v) <- zip [0 ..] tD]
      ++ map noteRow (zip [0 ..] notes)
      ++ [ txt env 20 34 15 (hsb 0 0 100) "1: table   2: reduction   Click x buttons: true/false   Space: try all assignments in turn"
         , txt env ex (y0 + 300) 18 (if okAll then hsb 120 80 100 else hsb 0 80 100)
             (if okAll then "Sum = t: formula is true!" else "Sum /= t: a clause is false")
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | wMode w == 2 ->
        let (mx, my) = mouseP5 env p
            inButton i = mx > 20 + fromIntegral i * 120 && mx < 128 + fromIntegral i * 120 && my > 90 && my < 124
            hits = map inButton [0 .. nV - 1 :: Int]
        in pure $ if or hits
             then w { wAsg = zipWith (/=) (wAsg w) hits, wBrute = -1 }
             else w
  EventKey (Char '1') Down _ _ -> pure (resetDP w { wMode = 1 })
  EventKey (Char '2') Down _ _ -> pure w { wMode = 2 }
  EventKey (SpecialKey KeySpace) Down _ _
    | wMode w == 1 ->
        let w1 = if wCell w >= (length (wNums w) + 1) * (wTarget w + 1) then resetDP w else w
        in pure w1 { wPlay = not (wPlay w1) }
    | otherwise -> pure w { wBrute = if wBrute w >= 0 then -1 else 0 }
  EventKey (SpecialKey KeyUp) Down _ _
    | wMode w == 1 -> pure (resetDP w { wTarget = min 40 (wTarget w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _
    | wMode w == 1 -> pure (resetDP w { wTarget = max 1 (wTarget w - 1) })
  EventKey (Char c) Down _ _
    | wMode w == 1 && c `elem` "rR" -> do
        ns <- replicateM 6 (randomRIO (1, 15))
        pure (resetDP w { wNums = ns })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. モード 1 で t を 1 ずつ変え、作れない t を探そう
-- 2. モード 2 で全部の節が真になる割り当てを探し、合計が t と一致するのを確かめよう
-- 3. cls に節を 1 つ足すと、表の行と桁はいくつ増える？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 文字：左端の上下中央が (x, y)
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = label (gx env x) (gy env y - s / 2) s c str

-- 文字：右端の上下中央が (x, y)
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y s c str =
  label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y - s / 2) s c str
