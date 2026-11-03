-- ============================================
-- 02 回路充足可能性（CIRCUIT-SAT）
-- AND・OR・NOT の回路に、出力が 1 になる入力はある？ → 最初に NP 完全と証明された問題のなかま（クック＝レヴィン）
-- 入力を決めれば出力の計算（検証）は一瞬。でも入力が k 本なら候補は 2^k 通り
-- 左の入力をクリックで 0/1 切り替え / スペースで全部の入力を総当たり / 1〜3 で回路を切り替え
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.IntMap.Strict as IM

bruteWait :: Int
bruteWait = 18      -- ★ 1 通り調べるのにかけるフレーム数

data GateType = AND | OR | NOT deriving (Eq, Show)

-- ゲートの入り口：入力 x_i か、ほかのゲートの出力
data Src = In !Int | G !Int

data Gate = Gate { gType :: !GateType, gIns :: [Src] }

data World = World
  { wInputs :: [Int]            -- 入力の値（0 か 1）
  , wGates  :: [Gate]
  , wTruth  :: [(String, Int)]  -- 総当たりの結果（古いものが先頭）
  , wBrute  :: !Int             -- 総当たりで今調べている番号（-1 = 休み）
  }

main :: IO ()
main = runSketch (sketch "18-02 Circuit SAT" (preset 1))
  { skBack  = hsb 220 35 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

preset :: Int -> World
preset k = case k of
  1 -> mk 3   -- 充足可能（答えは 101 と 111）
    [ Gate OR [In 1, In 2], Gate NOT [In 1], Gate AND [In 0, G 0], Gate OR [G 1, In 2], Gate AND [G 2, G 3] ]
  2 -> mk 3   -- 充足不能
    [ Gate OR [In 0, In 1], Gate NOT [In 0], Gate NOT [In 1], Gate AND [G 1, G 2], Gate AND [G 0, G 3, In 2] ]
  _ -> mk 4   -- 4 入力、答えは 1 通りだけ
    [ Gate AND [In 0, In 2], Gate OR [In 1, In 3], Gate NOT [G 1], Gate OR [G 0, In 1], Gate AND [G 3, G 2], Gate AND [G 4, In 0] ]
  where mk n gs = World (replicate n 0) gs [] (-1)

-- 回路の計算：前のゲートから順に値を求める
gateValues :: [Int] -> [Gate] -> IM.IntMap Int
gateValues inputs = foldl' add IM.empty . zip [0 ..]
  where
    inMap = IM.fromList (zip [0 ..] inputs)
    add vals (i, g) =
      let val (In k) = IM.findWithDefault 0 k inMap
          val (G k) = IM.findWithDefault 0 k vals
          vs = map val (gIns g)
          v = case gType g of
            AND -> if all (/= 0) vs then 1 else 0
            OR -> if any (/= 0) vs then 1 else 0
            NOT -> 1 - sum (take 1 vs)
      in IM.insert i v vals

output :: World -> Int
output w =
  let vals = gateValues (wInputs w) (wGates w)
  in IM.findWithDefault 0 (length (wGates w) - 1) vals

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wBrute w >= 0 && envFrame env `mod` bruteWait == 0 =
      let k = length (wInputs w)
          b = wBrute w
      in if b < 1 `shiftL` k
           then
             let bits = [(b `shiftR` (k - 1 - i)) .&. 1 | i <- [0 .. k - 1]]
                 w1 = w { wInputs = bits }
             in pure w1 { wTruth = wTruth w ++ [(concatMap show bits, output w1)], wBrute = b + 1 }
           else pure w { wBrute = -1 }
  | otherwise = pure w

-- ゲートの深さで横位置を決める
gateDepths :: [Gate] -> IM.IntMap Int
gateDepths = foldl' add IM.empty . zip [0 ..]
  where
    add ds (i, g) =
      let d = maximum (0 : [IM.findWithDefault 0 s ds | G s <- gIns g])
      in IM.insert i (d + 1) ds

inputPos :: Env -> World -> Int -> Point
inputPos env w i =
  let n = length (wInputs w)
  in (120, remap (fromIntegral i) 0 (fromIntegral (n - 1)) (envH env * 0.3) (envH env * 0.85))

gatePositions :: Env -> World -> IM.IntMap Point
gatePositions env w =
  let ds = gateDepths (wGates w)
      maxD = maximum (1 : IM.elems ds)
      left = 120
      right = envW env * 0.62
      column d =
        let col = [i | (i, e) <- IM.toList ds, e == d]
            len = length col
            place (j, i) =
              ( i
              , ( remap (fromIntegral d) 0 (fromIntegral maxD) left right
                , if len == 1
                    then envH env * 0.575
                    else remap (fromIntegral (j :: Int)) 0 (fromIntegral (len - 1)) (envH env * 0.33) (envH env * 0.82) ) )
        in map place (zip [0 ..] col)
  in IM.fromList (concatMap column [1 .. maxD])

drawW :: Env -> World -> Picture
drawW env w =
  let inputs = wInputs w
      gates = zip [0 :: Int ..] (wGates w)
      vals = gateValues inputs (wGates w)
      gPos = gatePositions env w
      posOfGate i = IM.findWithDefault (0, 0) i gPos
      valOf (In k) = sum (take 1 (drop k inputs))
      valOf (G k) = IM.findWithDefault 0 k vals
      posOf (In k) = inputPos env w k
      posOf (G k) = posOfGate k
      wire (i, g) =
        let (gx', gy') = posOfGate i
            m = length (gIns g)
            one (j, s) =
              let (ax, ay) = posOf s
                  on = valOf s /= 0
                  ty = gy' + (fromIntegral (j :: Int) - fromIntegral (m - 1) / 2) * 16
                  midX = (ax + gx') / 2
                  ps = bezier (ax + 30, ay) (midX, ay) (midX, ty) (gx' - 34, ty)
              in Color (if on then hsb 50 90 100 else hsb 220 30 40)
                   (thickPath (if on then 5 else 3) (map (pt env) ps))
        in Pictures (map one (zip [0 ..] (gIns g)))
      inputBox (i, v) =
        let (x, y) = inputPos env w i
            on = v /= 0
        in Pictures
             [ box env (if on then hsb 50 90 100 else hsb 220 25 30) (x - 34) (y - 22) 64 44
             , txtC env (x - 2) y 15 (hsb 0 0 (if on then 0 else 100)) ("x" ++ show (i + 1) ++ "=" ++ show v)
             ]
      gateShape (i, g) =
        let (x, y) = posOfGate i
            v = IM.findWithDefault 0 i vals
            on = v /= 0
            hue = case gType g of
              AND -> 190
              OR -> 280
              NOT -> 340
            col = hsb hue 60 (if on then 95 else 45)
            shape = case gType g of
              NOT -> Polygon (map (pt env) [(x - 34, y - 24), (x - 34, y + 24), (x + 22, y)])
              _ -> Polygon (map (pt env) (dShape x y))
            isOut = i == length gates - 1
            tag = case gType g of
              NOT -> txtC env (x - 16) y 14 (hsb 0 0 (if on then 10 else 90)) "NOT"
              t -> txtC env (x - 4) y 14 (hsb 0 0 (if on then 10 else 90)) (show t)
            outLabel
              | isOut = txtC env (x + 90) y 24 (if on then hsb 120 80 100 else hsb 0 70 80) ("Out " ++ show v)
              | otherwise = Blank
        in Pictures [Color col shape, tag, outLabel]
      -- 真理値表
      tx = envW env * 0.74
      ty0 = 120
      truth = wTruth w
      total = 1 `shiftL` length inputs :: Int
      rowT (i, (bits, out)) =
        txt env tx (ty0 + 50 + fromIntegral (i :: Int) * 24) 16
          (if out /= 0 then hsb 120 80 100 else hsb 0 0 55)
          (bits ++ "  ->  " ++ show out ++ (if out /= 0 then "  *" else ""))
      verdict
        | length truth == total =
            let sat = any ((/= 0) . snd) truth
                y = ty0 + 60 + fromIntegral (length truth) * 24
                c = if sat then hsb 120 80 100 else hsb 0 70 100
            in Pictures
                 [ txt env tx (y + 16) 18 c (if sat then "Satisfiable" else "Unsatisfiable")
                 , txt env tx (y + 40) 15 c (if sat then "(some input gives 1)" else "(every input gives 0)")
                 ]
        | otherwise = Blank
  in Pictures
       ( map wire gates
      ++ map inputBox (zip [0 ..] inputs)
      ++ map gateShape gates
      ++ map rowT (zip [0 ..] truth)
      ++ [ verdict
         , txt env tx (ty0 + 18) 16 (hsb 0 0 100) ("Brute force (" ++ show (length truth) ++ " / " ++ show total ++ ")")
         , txt env 20 36 15 (hsb 0 0 100) "Click inputs: 0/1   Space: brute force   1: satisfiable   2: unsatisfiable   3: 4 inputs (one answer)"
         , txt env 20 64 15 (hsb 0 0 65) "Computing the output = checking a certificate (gate count). k inputs: 2^k cases. k = 100: 10^30"
         ] )

-- AND・OR の形：左が四角で右が丸い
dShape :: Float -> Float -> [Point]
dShape x y =
  [(x - 34, y + 26), (x - 34, y - 26)]
    ++ [(x + 4 + 26 * cos a, y + 26 * sin a) | k <- [0 .. 12 :: Int], let a = -pi / 2 + pi * fromIntegral k / 12]

-- 3 次ベジェ曲線の上の点
bezier :: Point -> Point -> Point -> Point -> [Point]
bezier (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  [ (f x0 x1 x2 x3 t, f y0 y1 y2 y3 t) | k <- [0 .. 20 :: Int], let t = fromIntegral k / 20 ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = mouseP5 env p
        toggle i v =
          let (x, y) = inputPos env w i
          in if abs (mx - x) < 36 && abs (my - y) < 24 then Just (1 - v) else Nothing
        results = zipWith toggle [0 ..] (wInputs w)
        hitAny = any (/= Nothing) results
    in pure $ if hitAny
         then w { wInputs = zipWith (\r v -> maybe v id r) results (wInputs w), wBrute = -1 }
         else w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wTruth = [], wBrute = 0 }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (preset (fromEnum c - fromEnum '0'))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 回路 3 で、総当たりする前に自分で答えを見つけよう
-- 2. wGates に新しいゲートを足して、充足不能な回路をつくろう
-- 3. 入力を 5 本にすると表は何行？ 20 本なら？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

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
