-- ============================================
-- 11 量子コンピュータ入門
-- 3 量子ビットの回路エディタ。状態は 2³ = 8 個の複素数（振幅）。ゲートは振幅の組をまぜる操作
--   バー = 振幅（長さ = |振幅|²、色 = 位相）。測定するとその確率で 1 つの答えが出る
-- パレット or 1〜7 = ゲートを選ぶ（H X Z S T ● ⊕）/ E = 消しゴム / マス目をクリック = 置く・消す / 列にマウス = その列までの状態
-- B = ベル状態 / G = GHZ / D = ドイチュ＝ジョサ / V = グローバー探索（|111⟩ を探す） / C = 全部消す / M = 測定をやり直す
-- ●（制御）と ⊕（反転）を同じ列に置く = CNOT。● だけを 2〜3 個 = 制御 Z（全部 1 のとき符号を反転）
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nQ, nCol, dimS :: Int
nQ = 3               -- ★ 量子ビットの数
nCol = 14            -- ★ 列の数
dimS = 1 `shiftL` nQ

-- マス目の中身：'H' 'X' 'Z' 'S' 'T'、'o' = ●（制御）、'+' = ⊕（反転）、' ' = 空
ctrlG, targG, emptyG :: Char
ctrlG = 'o'
targG = '+'
emptyG = ' '

gates :: [Char]
gates = "HXZST" ++ [ctrlG, targG]

gx0, gy0 :: Float
gx0 = 140
gy0 = 130

type State = U.Vector (Float, Float)

data World = World
  { wGrid   :: !(U.Vector Char)  -- 列 c、量子ビット q のマスは c * nQ + q
  , wCur    :: !Char             -- 選んでいるゲート（' ' は消しゴム）
  , wShots  :: !(U.Vector Int)
  , wNShots :: !Int
  , wState  :: !State
  }

main :: IO ()
main = runSketch (sketch "25-11 Quantum Circuit" (preset 'B' (World U.empty 'H' U.empty 0 U.empty)))
  { skBack  = hsb 210 30 10
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

cellOf :: Env -> Float
cellOf env = min 56 ((envW env - 200) / fromIntegral nCol)

clearAll :: World -> World
clearAll w = resetShots w { wGrid = U.replicate (nCol * nQ) emptyG }

resetShots :: World -> World
resetShots w = w { wShots = U.replicate dimS 0, wNShots = 0 }

gateAt :: U.Vector Char -> Int -> Int -> Char
gateAt g c q = g U.! (c * nQ + q)

preset :: Char -> World -> World
preset k w0 =
  let w = clearAll w0
      puts = case k of
        'B' -> [(0, 0, 'H'), (1, 0, ctrlG), (1, 1, targG)]
        'G' -> [(0, 0, 'H'), (1, 0, ctrlG), (1, 1, targG), (2, 1, ctrlG), (2, 2, targG)]
        -- f(x0, x1) = x0 XOR x1（バランス型）。結果の q0 q1 が 00 以外 → バランス型
        'D' -> [(0, 2, 'X')] ++ [ (1, q, 'H') | q <- [0 .. 2] ]
               ++ [(2, 0, ctrlG), (2, 2, targG), (3, 1, ctrlG), (3, 2, targG), (4, 0, 'H'), (4, 1, 'H')]
        -- グローバー：オラクル（|111⟩ の符号を反転）＋ 拡散（平均のまわりで反転）を 2 回
        'V' -> [ (0, q, 'H') | q <- [0 .. 2] ]
               ++ [ (c + i, q, g)
                  | r <- [0, 1], let c = 1 + r * 6
                  , q <- [0 .. 2]
                  , (i, g) <- zip [0 ..] [ctrlG, 'H', 'X', ctrlG, 'X', 'H'] ]
        _ -> []
  in w { wGrid = wGrid w U.// [ (c * nQ + q, g) | (c, q, g) <- puts ] }

-- ★ 1 量子ビットのゲート：ビット q が 0 と 1 の振幅の組 (a, b) を 2×2 行列でまぜる
apply1 :: Char -> Int -> State -> State
apply1 g q s =
  let m = 1 `shiftL` q :: Int
      r2 = sqrt 0.5
  in U.imap (\i _ ->
       let (ar, ai) = s U.! (i .&. complement m)
           (br, bi) = s U.! (i .|. m)
           isOne = i .&. m /= 0
       in case g of
            'H' -> if isOne then ((ar - br) * r2, (ai - bi) * r2) else ((ar + br) * r2, (ai + bi) * r2)
            'X' -> if isOne then (ar, ai) else (br, bi)
            'Z' -> if isOne then (-br, -bi) else (ar, ai)
            'S' -> if isOne then (-bi, br) else (ar, ai)                       -- ×i
            'T' -> if isOne then ((br - bi) * r2, (br + bi) * r2) else (ar, ai) -- ×e^{iπ/4}
            _ -> s U.! i) s

-- 回路を最初から列 upto まで計算
runCircuit :: U.Vector Char -> Int -> State
runCircuit g upto = foldl' column s0 [0 .. upto]
  where
    s0 = U.generate dimS (\i -> if i == 0 then (1, 0) else (0, 0))   -- |000⟩ から
    column s c =
      let qs = [ (q, gateAt g c q) | q <- [0 .. nQ - 1] ]
          ctrl = foldl' (\m (q, x) -> if x == ctrlG then m .|. (1 `shiftL` q) else m) 0 qs :: Int
          tgts = [ q | (q, x) <- qs, x == targG ]
          s1 = foldl' (\st (q, x) -> if x `elem` "HXZST" then apply1 x q st else st) s qs
      in case reverse tgts of
           (t : _) ->            -- 制御 NOT：● が全部 1 なら ⊕ のビットを反転
             U.imap (\i v -> if i .&. ctrl == ctrl then s1 U.! (i `xor` (1 `shiftL` t)) else v) s1
           [] | ctrl /= 0 && ctrl .&. (ctrl - 1) /= 0 ->   -- ● が 2 個以上：制御 Z（全部 1 なら符号反転）
                  U.imap (\i (a, b) -> if i .&. ctrl == ctrl then (-a, -b) else (a, b)) s1
              | otherwise -> s1

basisLabel :: Int -> String
basisLabel i = "|" ++ concat [ show ((i `shiftR` q) .&. 1) | q <- [0 .. nQ - 1] ] ++ ">"

-- マウスが乗っている列（乗っていなければ Nothing）
hoverCol :: Env -> Maybe Int
hoverCol env =
  let (mx, my) = mouseP env
      cell = cellOf env
      hc = floor ((mx - gx0) / cell)
  in if hc >= 0 && hc < nCol && my > gy0 - 20 && my < gy0 + fromIntegral nQ * cell then Just hc else Nothing

stepW :: Env -> Float -> World -> IO World
stepW env _ w = do
  let hover = hoverCol env
      s = runCircuit (wGrid w) (maybe (nCol - 1) id hover)
      ps = map (\(a, b) -> a * a + b * b) (U.toList s)
      -- 確率に従って 1 つ選ぶ
      pick :: Float -> Int
      pick r = go 0 0 ps
        where go !i _ [] = max 0 (i - 1)
              go !i !acc (p : rest)
                | r < acc + p || i == dimS - 1 = i
                | otherwise = go (i + 1) (acc + p) rest
      shot (v, n) _ = do
        r <- randomRIO (0, 1)
        let i = pick r
        pure (v U.// [(i, v U.! i + 1)], n + 1)
  -- 測定のサンプリング（最後まで計算した状態から）
  (shots, n) <- case hover of
    Nothing | wNShots w < 2000 -> foldM shot (wShots w, wNShots w) [1 .. 20 :: Int]
    _ -> pure (wShots w, wNShots w)
  pure w { wState = s, wShots = shots, wNShots = n }

-- ● と ⊕ の形
ctrlPic :: Env -> Float -> Float -> Color -> Picture
ctrlPic env x y c = dotAt env c x y 14

targPic :: Env -> Float -> Float -> Float -> Color -> Picture
targPic env x y r c = Pictures
  [ Color c (at (pt env x y) (ring r 3))
  , seg env 3 c (x - r, y) (x + r, y)
  , seg env 3 c (x, y - r) (x, y + r) ]

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      cell = cellOf env
      hover = hoverCol env
      cur = wCur w
      hi = hsb 55 80 100
      -- パレット
      palette k g =
        let x = gx0 + fromIntegral k * 52
            y = 82
            on = g == cur
            fg = if on then black else white
            cen = (x + 22, y + 16)
        in [ box env (if on then hi else hsb 0 0 25) x y 44 32
           , if g == ctrlG then ctrlPic env (fst cen) (snd cen) fg
             else if g == targG then targPic env (fst cen) (snd cen) 10 fg
             else txtMid env (fst cen) (snd cen) 17 fg [g] ]
      eraser = [ box env (if cur == emptyG then hi else hsb 0 0 25) (gx0 + 7 * 52) 82 60 32
               , txtMid env (gx0 + 7 * 52 + 30) 98 14 (if cur == emptyG then black else white) "Erase" ]
      -- 回路
      wireY q = gy0 + fromIntegral q * cell + cell / 2
      wires = concat
        [ [ seg env 2 (hsb 0 0 50) (gx0 - 10, wireY q) (gx0 + fromIntegral nCol * cell, wireY q)
          , txt env (gx0 - 82) (wireY q + 5) 14 white ("q" ++ show q ++ " |0>") ]
        | q <- [0 .. nQ - 1] ]
      hoverBox = case hover of
        Just hc -> [ box env (hsba 55 80 100 0.12) (gx0 + fromIntegral hc * cell) (gy0 - 10) cell (fromIntegral nQ * cell + 20) ]
        Nothing -> []
      columnPic c =
        let x = gx0 + fromIntegral c * cell + cell / 2
            ys = [ wireY q | q <- [0 .. nQ - 1], let g = gateAt (wGrid w) c q, g == ctrlG || g == targG ]
            link = case ys of
              (a : _ : _) -> [ seg env 3 (hsb 190 70 100) (x, a) (x, last ys) ]
              _ -> []
            one q =
              let g = gateAt (wGrid w) c q
                  y = wireY q
              in if g == emptyG then []
                 else if g == ctrlG then [ ctrlPic env x y (hsb 190 70 100) ]
                 else if g == targG then [ targPic env x y 13 (hsb 190 70 100) ]
                 else [ box env (hsb 230 50 70) (x - cell * 0.38) (y - cell * 0.38) (cell * 0.76) (cell * 0.76)
                      , txtMid env x y 18 white [g] ]
        in link ++ concatMap one [0 .. nQ - 1]
      -- 振幅のバー（色 = 位相）と測定のヒストグラム
      by = gy0 + fromIntegral nQ * cell + 70
      bw = (width - 2 * gx0) / fromIntegral dimS
      bh = height - by - 70
      barPic i =
        let (a, b) = wState w U.! i
            p = a * a + b * b
            x = gx0 + fromIntegral i * bw
            frac = fromIntegral (wShots w U.! i) / fromIntegral (max 1 (wNShots w)) :: Float
        in [ box env (hsb 0 0 20) (x + 6) by (bw * 0.4) bh
           , box env (hsb ((atan2 b a + pi) / tau * 360) 70 100) (x + 6) (by + bh - p * bh) (bw * 0.4) (p * bh) ]
           ++ [ box env (hsb 55 90 100) (x + 8 + bw * 0.4) (by + bh - frac * bh) (bw * 0.35) (frac * bh)
              | hover == Nothing && wNShots w > 0 ]
           ++ [ txt env (x + 6) (by + bh + 22) 16 white (basisLabel i)
              , txt env (x + 6) (by + bh + 42) 14 (gray 0.7) (nf 3 p) ]
           ++ [ txt env (x + 6) (by + bh - p * bh - 6) 14 (gray 0.7) (nf 2 a ++ (if b >= 0 then "+" else "") ++ nf 2 b ++ "i") | p > 1e-6 ]
      curName | cur == emptyG = "Erase"
              | cur == ctrlG = "control"
              | cur == targG = "target"
              | otherwise = [cur]
  in Pictures $
       concat (zipWith palette [0 :: Int ..] gates) ++ eraser
       ++ wires ++ hoverBox
       ++ concatMap columnPic [0 .. nCol - 1]
       ++ [ txt env gx0 (by - 16) 14 (gray 0.7) (case hover of
              Just hc -> "State up to column " ++ show hc ++ " (bar = |amplitude|^2, color = phase)"
              Nothing -> "Final state (bar = |amplitude|^2, color = phase; yellow = " ++ show (wNShots w) ++ " measurements)") ]
       ++ concatMap barPic [0 .. dimS - 1]
       ++ [ txt env 20 24 16 white ("1-7 / palette: gate (" ++ curName ++ ")   E: erase   Click: place   B Bell   G GHZ   D Deutsch-Jozsa")
          , txt env 20 44 16 white "V Grover   C clear all   M measure again"
          , txt env 20 66 14 (gray 0.6) "Labels read |q0 q1 q2>. H makes superposition, CNOT entangles. Grover: 2 rounds give |111> about 95%."
          ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> onClick env w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '7' -> w { wCur = gates !!! (fromEnum c - fromEnum '1') }
    | otherwise -> case toUpper c of
        'E' -> w { wCur = emptyG }
        k | k `elem` "BGDV" -> preset k w
        'C' -> clearAll w
        'M' -> resetShots w
        _ -> w
  _ -> w

onClick :: Env -> World -> World
onClick env w =
  let (mx, my) = mouseP env
      cell = cellOf env
      k = floor ((mx - gx0) / 52) :: Int
      c = floor ((mx - gx0) / cell) :: Int
      q = floor ((my - gy0) / cell) :: Int
      cur = wCur w
      inPalette = my > 82 && my < 114
  in if inPalette && k >= 0 && k < length gates then w { wCur = gates !!! k }
     else if inPalette && k == 7 then w { wCur = emptyG }
     else if c >= 0 && c < nCol && q >= 0 && q < nQ then
       let old = gateAt (wGrid w) c q
           g1 = wGrid w U.// [(c * nQ + q, if old == cur then emptyG else cur)]
           -- ⊕ は 1 列に 1 つ
           g2 = if cur == targG
                  then g1 U.// [ (c * nQ + r, emptyG) | r <- [0 .. nQ - 1], r /= q, gateAt g1 c r == targG ]
                  else g1
       in resetShots w { wGrid = g2 }
     else w

-- ---------- やってみよう ----------
-- 1. B（ベル状態）を測ると |000⟩ と |110⟩ だけが半々。q0 を測れば q1 もわかる = もつれ
-- 2. D のオラクル（列 2・3 の CNOT）を消すと「定数型」。結果の q0 q1 はどうなる？
-- 3. V のグローバーを 1 回だけ（列 7〜12 を消す）にすると |111⟩ の確率は？ nCol を 20 にして 3 回にすると、回しすぎで下がる
-- 4. H → Z → H が X と同じになることを、バーの変化で確かめよう。T を 2 回 = S、S を 2 回 = Z

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- 範囲の外なら空のマスを返す（!! の安全な版）
(!!!) :: [Char] -> Int -> Char
xs !!! i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> emptyG

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

-- (x, y) を中心にした文字
txtMid :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtMid env x y size c s = let (gx, gy) = pt env x y in labelC gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
