-- ============================================
-- 09 シュテルン＝ゲルラッハ実験
-- 銀原子のビームを不均一な磁石（SG 装置）に通すと、スピンの向きで「上（+）」「下（−）」の 2 本にだけ分かれる
--   z で + を選んだあと x で測り、もう一度 z で測ると… また半々に分かれる。x を測ったことで z の情報が消える
-- 箱をクリック = 軸を z → 45° → x → z / ←→ = 選んだ箱の軸を 15° ずつ / 出口（+ / −）をクリック = 次へ通すほう
-- 1〜4 = 例（z ／ z-z ／ z-x ／ z-x-z） / ↑↓ = 原子の数 / C = カウントを消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import Numeric (showFFloat)

speed :: Float
speed = 7            -- ★ 原子の速さ（px/フレーム）

data Stage = Stage
  { sTh   :: !Int    -- 軸の角度（0 = z, 90 = x）
  , sPass :: !Int    -- 通すほう：1（+）か −1（−）
  , sPlus :: !Int    -- + の数
  , sMinus :: !Int   -- − の数
  }

data Atom = Atom
  { aPath :: ![Point]   -- これから通る点の列（先頭 = いまの区間の始点）
  , aF    :: !Float     -- いまの区間の進み具合
  , aOuts :: ![Int]     -- 各装置の結果
  }

data World = World
  { wStages :: ![Stage]
  , wSel    :: !Int
  , wRate   :: !Float   -- ★ 1 フレームに出す原子の数（平均）
  , wAtoms  :: ![Atom]
  , wFinalP :: !Int
  , wFinalM :: !Int
  }

main :: IO ()
main = runSketch (sketch "25-09 Stern-Gerlach" (preset 4 (World [] 0 0.6 [] 0 0)))
  { skBack  = hsb 260 25 10
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

preset :: Int -> World -> World
preset k w =
  let axes = case k of
        1 -> [0]
        2 -> [0, 0]
        3 -> [0, 90]
        _ -> [0, 90, 0]
  in w { wStages = map (\th -> Stage th 1 0 0) axes, wAtoms = [], wFinalP = 0, wFinalM = 0, wSel = 0 }

clearCounts :: World -> World
clearCounts w = w { wStages = map (\s -> s { sPlus = 0, sMinus = 0 }) (wStages w), wAtoms = [], wFinalP = 0, wFinalM = 0 }

-- 画面の配置
cyOf :: Env -> Float
cyOf env = envH env * 0.55

ovenX :: Float
ovenX = 90

boxX :: Env -> Int -> Int -> Float
boxX env n i =
  let sp = (envW env - 420) / fromIntegral n
  in 260 + sp * fromIntegral i + sp * 0.3

portY :: Env -> Int -> Float
portY env s = cyOf env - fromIntegral s * 50     -- + は上、− は下

-- ★ 1 個の原子の運命をシミュレーションして、通り道（点の列）を作る
--   スピンの向き s（単位ベクトル）。軸 n で測ると + になる確率 = (1 + s·n) / 2 = cos²(角度の差 / 2)
--   測ったあと s は +n か −n に「収縮」する
makeAtom :: Env -> [Stage] -> IO Atom
makeAtom env stages = do
  -- 炉から出る原子のスピンはばらばらの向き（3D でランダム）
  u <- randomRIO (-1, 1)
  ph <- randomRIO (0, tau)
  let c = sqrt (max 0 (1 - u * u))
      s0 = V3 (c * cos ph) (c * sin ph) u
      cy = cyOf env
      nSt = length stages
      go _ i [] = pure ([], [])
      go s i (st : rest) = do
        let th = fromIntegral (sTh st) * pi / 180
            n = V3 (sin th) 0 (cos th)
            pPlus = (1 + dot3 s n) / 2
        r <- randomRIO (0, 1)
        let out = if r < pPlus then 1 else -1 :: Int
            fo = fromIntegral out :: Float
            bx = boxX env nSt i
            here = [(bx - 60, cy), (bx + 60, cy - fo * 22), (bx + 100, portY env out)]
        case rest of
          [] -> pure (here ++ [(envW env - 110, cy - fo * 110)], [out])
          _ | out /= sPass st -> pure (here ++ [(bx + 150, cy - fo * 105)], [out])   -- 止められる
            | otherwise -> do
                (more, outs) <- go (scale3 fo n) (i + 1) rest
                pure (here ++ [(boxX env nSt (i + 1) - 90, cy)] ++ more, out : outs)
  (path, outs) <- go s0 (0 :: Int) stages
  pure (Atom ((ovenX, cy) : path) 0 outs)

-- 原子を 1 フレーム動かす
moveAtom :: Atom -> Atom
moveAtom a = case aPath a of
  (p : q : rest) ->
    let d = max 1 (dist2 p q)
        f = aF a + speed / d
    in if f >= 1 then a { aPath = q : rest, aF = 0 } else a { aF = f }
  _ -> a

isDone :: Atom -> Bool
isDone a = case aPath a of
  (_ : _ : _) -> False
  _ -> True

-- 着いた原子を数える
countAtom :: World -> Atom -> World
countAtom w a =
  let outs = aOuts a
      upd st o = if o > 0 then st { sPlus = sPlus st + 1 } else st { sMinus = sMinus st + 1 }
      stages = zipWith ($) (map (flip upd) outs ++ repeat id) (wStages w)
      lastOut = foldl' (\_ o -> o) 0 outs
      complete = length outs == length (wStages w)
  in w { wStages = stages
       , wFinalP = wFinalP w + (if complete && lastOut > 0 then 1 else 0)
       , wFinalM = wFinalM w + (if complete && lastOut < 0 then 1 else 0) }

stepW :: Env -> Float -> World -> IO World
stepW env _ w = do
  r <- randomRIO (0, 1)
  let whole = floor (wRate w) :: Int
      n = whole + (if r < wRate w - fromIntegral whole then 1 else 0)
      room = max 0 (500 - length (wAtoms w))
  new <- replicateM (min n room) (makeAtom env (wStages w))
  let moved = map moveAtom (wAtoms w)
      w1 = foldl' countAtom w (filter isDone moved)
  pure w1 { wAtoms = new ++ filter (not . isDone) moved }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      cy = cyOf env
      stages = wStages w
      nSt = length stages
      stagePic (i, st) =
        let bx = boxX env nSt i
            isSel = i == wSel w
            lastOne = i == nSt - 1
            th = fromIntegral (sTh st) * pi / 180
            ax = 28 * sin th
            ay = 28 * cos th
            tot = sPlus st + sMinus st
            port s =
              let passOK = lastOne || sPass st == s
                  fs = fromIntegral s :: Float
                  count = if s > 0 then sPlus st else sMinus st
              in [ dotAt env (if passOK then hsb 120 70 90 else hsb 0 70 80) (bx + 100) (portY env s) 22
                 , txtC env (bx + 100) (portY env s + 5) 14 (hsb 0 0 10) (if s > 0 then "+" else "-") ]
                 ++ [ box env (hsb 0 0 40) (bx + 140) (cy - fs * 105 - 14) 14 28 | not passOK ]
                 ++ [ txtC env (bx + 100) (portY env s - fs * 22 + (if s > 0 then 0 else 10)) 14 white (show count) ]
        in [ box env (hsb (if isSel then 200 else 230) 40 (if isSel then 55 else 35)) (bx - 60) (cy - 45) 120 90
           , frameThick env 2 (if isSel then hsb 55 80 100 else hsb 0 0 50) (bx - 60) (cy - 45) 120 90
           -- 軸の向き（矢印）
           , seg env 4 (hsb 55 90 100) (bx - ax, cy + ay) (bx + ax, cy - ay)
           , dotAt env (hsb 55 90 100) (bx + ax) (cy - ay) 10
           , txtC env bx (cy - 55) 15 white (case sTh st of { 0 -> "z"; 90 -> "x"; t -> show t ++ " deg" }) ]
           ++ port 1 ++ port (-1)
           ++ [ txtC env bx (cy + 68) 14 (gray 0.7) ("+ " ++ nf 0 (100 * fromIntegral (sPlus st) / fromIntegral tot) ++ "%") | tot > 0 ]
      atomPic a = case aPath a of
        (p : q : _) -> [ dotAt env (hsb 190 60 100) (lerpF (fst p) (fst q) (aF a)) (lerpF (snd p) (snd q) (aF a)) 5 ]
        _ -> []
      theory =
        let lines' = [ "  stage " ++ show (i + 1) ++ " + = " ++ nf 0 (p * 100) ++ "%"
                     | (i, a, b) <- zip3 [1 :: Int ..] stages (drop 1 stages)
                     , let dl = fromIntegral (sTh b - sTh a) * pi / 180 :: Float
                     , let p = if sPass a > 0 then sq (cos (dl / 2)) else sq (sin (dl / 2)) ]
        in if nSt > 1 then "Theory cos^2(d/2):" ++ concat lines'
                      else "Theory: stage 1 is always 50% / 50% (spins are random)"
  in Pictures $
       [ box env (hsb 15 70 70) (ovenX - 50) (cy - 30) 50 60
       , txt env (ovenX - 50) (cy + 50) 14 white "Oven" ]
       ++ concatMap stagePic (zip [0 :: Int ..] stages)
       -- 最後のスクリーン
       ++ [ box env (hsb 0 0 25) (width - 120) (cy - 150) 24 300
          , txt env (width - 90) (cy - 105) 15 white ("+ " ++ show (wFinalP w))
          , txt env (width - 90) (cy + 118) 15 white ("- " ++ show (wFinalM w)) ]
       ++ concatMap atomPic (wAtoms w)
       ++ [ txt env 20 26 16 white ("Click box: axis   Left/Right: selected box 15 deg   Click exit: which one passes")
          , txt env 20 48 16 white ("1-4: examples   Up/Down: atoms " ++ nf 2 (wRate w) ++ " / frame   C: clear")
          , txt env 20 70 14 (gray 0.6) "z+ then z gives 100% +. Put x between and the last z is 50/50. With angle d: P(+) = cos^2(d/2)."
          , txt env 20 (height - 30) 14 (gray 0.6) theory ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> onClick env w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> preset (fromEnum c - fromEnum '0') w
    | toLower c == 'c' -> clearCounts w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRate = min 5 (wRate w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRate = max 0.05 (wRate w / 1.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> turnSel 15 w
  EventKey (SpecialKey KeyLeft) Down _ _ -> turnSel 345 w
  _ -> w

turnSel :: Int -> World -> World
turnSel d w = clearCounts w { wStages = updateAt (wSel w) (\s -> s { sTh = (sTh s + d) `mod` 360 }) (wStages w) }

onClick :: Env -> World -> World
onClick env w =
  let (mx, my) = mouseP env
      cy = cyOf env
      nSt = length (wStages w)
      hits = concat
        [ [ clearCounts w { wStages = updateAt i (\st -> st { sPass = s }) (wStages w) }
          | s <- [1, -1], dist2 (mx, my) (boxX env nSt i + 100, portY env s) < 16, i < nSt - 1 ]
          ++ [ clearCounts w { wSel = i, wStages = updateAt i nextAxis (wStages w) }
             | abs (mx - boxX env nSt i) < 60, abs (my - cy) < 45 ]
        | i <- [0 .. nSt - 1] ]
  in case hits of
       (r : _) -> r
       [] -> w

-- 軸を z → 45° → x → z の順に
nextAxis :: Stage -> Stage
nextAxis st = st { sTh = case sTh st of
                           0 -> 45
                           45 -> 90
                           _ -> 0 }

-- ---------- やってみよう ----------
-- 1. 2（z-z）で 2 台目は 100% +。2 台目の軸を 15° ずつ回して、+ の割合が cos²(Δ/2) になるか確かめよう
-- 2. 4（z-x-z）の 2 台目の出口を − に変えても、最後は半々？
-- 3. 2 台目の軸を 180° にすると？（z の下向き = 「−z で +」）
-- 4. 装置を 4 台以上に増やして（preset に配列を足す）、少しずつ回した軸を並べると、スピンを止めずに 90° 回せる？（量子ゼノン効果）

-- ==================================================
-- ここから下は「道具」
-- ==================================================
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

sq :: Float -> Float
sq x = x * x

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

-- 中央揃えの文字（y はベースライン）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - fromIntegral (length s) * size * 0.36) y size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 太さのある枠線
frameThick :: Env -> Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameThick env t c x y w h = Pictures
  [ seg env t c (x, y) (x + w, y), seg env t c (x + w, y) (x + w, y + h)
  , seg env t c (x + w, y + h) (x, y + h), seg env t c (x, y + h) (x, y) ]

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
