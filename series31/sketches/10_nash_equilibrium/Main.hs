-- ============================================
-- 10 ナッシュ均衡は不動点
-- 2 人のゲーム。「相手の作戦に対するいちばん良い返し（最適反応）」を互いに取り合って、誰も変えたくない状態 = ナッシュ均衡
--   なめらかな最適反応 Φ（ロジット）は正方形 → 正方形の連続写像 → ブラウワーで不動点がある。λ → ∞ でナッシュ均衡に近づく（ナッシュの証明の考え方）
-- 1〜4 = 2×2 のゲーム / 5 = じゃんけん（3×3、レプリケーター動学）/ クリック = 出発点 / ↑↓ = λ（合理性）/ ←→ = じゃんけんの勝ち点 / C = 軌跡を消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

-- 利得表 ((a00, a01), (a10, a11))
type Pay = ((Float, Float), (Float, Float))

data Game = Game
  { gName :: String
  , gR0, gR1 :: String      -- 2 つの作戦の名前
  , gA, gB :: !Pay          -- プレイヤー 1 と 2 の利得
  }

gameOf :: Int -> Game
gameOf 0 = Game "Prisoner's Dilemma" "Cooperate" "Defect" ((3, 0), (5, 1)) ((3, 5), (0, 1))
gameOf 1 = Game "Matching Pennies" "Heads" "Tails" ((1, -1), (-1, 1)) ((-1, 1), (1, -1))
gameOf 2 = Game "Stag Hunt" "Stag" "Hare" ((4, 0), (3, 3)) ((4, 3), (0, 3))
gameOf _ = Game "Chicken" "Swerve" "Straight" ((0, -1), (1, -5)) ((0, 1), (-1, -5))

type X3 = (Float, Float, Float)

data World = World
  { wGi     :: !Int
  , wLam    :: !Float             -- ★ λ：大きいほど「最適反応」に忠実
  , wTrails :: ![[(Float, Float)]]  -- 2×2 の軌跡（それぞれ新しい順）
  , wWinA   :: !Float             -- ★ じゃんけん：勝ったときの点（負けは −1）
  , wRps    :: ![[X3]]            -- じゃんけんの軌跡（それぞれ新しい順）
  }

eta :: Float
eta = 0.08                        -- ★ 1 フレームで最適反応へ近づく割合

main :: IO ()
main = runSketch (sketch "31-10 Nash Equilibrium" (World 0 4 [[(0.85, 0.2)]] 1 []))
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

sig :: Float -> Float
sig z = 1 / (1 + exp (negate (clampF (-50) 50 z)))

-- 相手の確率に対する「上の作戦 − 下の作戦」の得の差
d1, d2 :: Game -> Float -> Float
d1 g q = let ((a00, a01), (a10, a11)) = gA g in (q * a00 + (1 - q) * a01) - (q * a10 + (1 - q) * a11)
d2 g p = let ((b00, b01), (b10, b11)) = gB g in (p * b00 + (1 - p) * b10) - (p * b01 + (1 - p) * b11)

-- ★ なめらかな最適反応 Φ(p, q)
phi :: World -> (Float, Float) -> (Float, Float)
phi w (p, q) = let g = gameOf (wGi w) in (sig (wLam w * d1 g q), sig (wLam w * d2 g p))

nashList :: Game -> [(Float, Float)]
nashList g = pure' ++ mixed
  where
    ((a00, a01), (a10, a11)) = gA g
    ((b00, b01), (b10, b11)) = gB g
    aOf :: Int -> Int -> Float
    aOf 0 0 = a00
    aOf 0 _ = a01
    aOf _ 0 = a10
    aOf _ _ = a11
    bOf :: Int -> Int -> Float
    bOf 0 0 = b00
    bOf 0 _ = b01
    bOf _ 0 = b10
    bOf _ _ = b11
    pure' = [ (fromIntegral (1 - i), fromIntegral (1 - j))
            | i <- [0, 1], j <- [0, 1], aOf i j >= aOf (1 - i) j, bOf i j >= bOf i (1 - j) ]
    -- 混合戦略：相手が無差別になる確率
    qd = (a11 - a01) / (a00 - a01 - a10 + a11)
    pd = (b11 - b10) / (b00 - b10 - b01 + b11)
    mixed = [ (pd, qd) | qd > 0, qd < 1, pd > 0, pd < 1 ]

-- Φ の不動点：p = σ(λ d1(σ(λ d2(p)))) を 1 変数で解く
phiFixed :: World -> [(Float, Float)]
phiFixed w = go 1 (h 0)
  where
    g = gameOf (wGi w)
    lam = wLam w
    h p = sig (lam * d1 g (sig (lam * d2 g p))) - p
    n = 2000 :: Int
    fn = fromIntegral n :: Float
    go i prev
      | i > n = []
      | otherwise =
          let p = fromIntegral i / fn
              v = h p
              rest = go (i + 1) v
          in if prev * v <= 0
               then let pp = bisect (40 :: Int) (fromIntegral (i - 1) / fn) p
                    in (pp, sig (lam * d2 g pp)) : rest
               else rest
    bisect 0 a b = (a + b) / 2
    bisect k a b =
      let m = (a + b) / 2
      in if h a * h m <= 0 then bisect (k - 1) a m else bisect (k - 1) m b

-- じゃんけん：三角形の各点 = 3 つの手を出す割合。レプリケーター動学 ẋᵢ = xᵢ((Mx)ᵢ − xᵀMx)
repl :: Float -> X3 -> X3
repl a (x, y, z) =
  let m0 = a * z - y
      m1 = a * x - z
      m2 = a * y - x
      avg = x * m0 + y * m1 + z * m2
  in (x * (m0 - avg), y * (m1 - avg), z * (m2 - avg))

rpsStep :: Float -> X3 -> X3
rpsStep a p@(x, y, z) =
  let (dx, dy, dz) = repl a p
      nx = max 1e-6 (x + 0.01 * dx)
      ny = max 1e-6 (y + 0.01 * dy)
      nz = max 1e-6 (z + 0.01 * dz)
      s = nx + ny + nz
  in (nx / s, ny / s, nz / s)

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w
  | wGi w == 4 =
      let adv t = case t of
            (x : _) -> take 3000 (nTimes 8 (rpsStep (wWinA w)) x : t)
            [] -> t
      in pure w { wRps = map adv (wRps w) }
  | otherwise =
      -- 軌跡を進める：x ← x + η(Φ(x) − x)
      let adv t = case t of
            (c@(p, q) : _) | length t < 600 ->
              let (fp, fq) = phi w c
              in (p + eta * (fp - p), q + eta * (fq - q)) : t
            _ -> t
      in pure w { wTrails = map adv (wTrails w) }

-- レイアウト
ox, oy :: Float
ox = 70
oy = 105

sizeOf :: Env -> Float
sizeOf env = min (envH env - 170) (envW env * 0.48)

drawWorld :: Env -> World -> Picture
drawWorld env w
  | wGi w == 4 = drawRps env w
  | otherwise = screen env $
      [ box (hsb 0 0 40) ox oy s s ]
      -- 矢印 Φ(x) − x
      ++ [ Color (hsba 0 0 100 0.3) (Line [toS (p, q), toS (lerpF p fp 0.12, lerpF q fq 0.12)])
         | i <- [0 .. 13 :: Int], j <- [0 .. 13 :: Int]
         , let p = (fromIntegral i + 0.5) / 14, let q = (fromIntegral j + 0.5) / 14
         , let (fp, fq) = phi w (p, q) ]
      -- 最適反応（かくかく）とロジット（なめらか）
      ++ [ Color (hsba 190 70 100 0.4) (thickPath 3 [ toS (stepV (d1 g q), q) | q <- ss ])
         , Color (hsba 330 70 100 0.4) (thickPath 3 [ toS (p, stepV (d2 g p)) | p <- ss ])
         , Color (hsb 190 70 100) (thickPath 2.5 [ toS (sig (lam * d1 g q), q) | q <- ss ])
         , Color (hsb 330 70 100) (thickPath 2.5 [ toS (p, sig (lam * d2 g p)) | p <- ss ]) ]
      -- 軌跡
      ++ concat [ [ Color (hsba 0 0 100 0.7) (Line (map toS t)) ] ++ [ spot white (toS c) 8 | c <- take 1 t ] | t <- wTrails w ]
      -- 不動点
      ++ [ hoop 3 (hsb 45 90 100) (toS e) 24 | e <- nashList g ]
      ++ [ spot (hsb 45 90 100) (toS e) 10 | e <- phiFixed w ]
      -- 軸
      ++ [ txt ox (oy + s + 24) 14 (hsb 0 0 70) ("p = prob. of \"" ++ gR0 g ++ "\" (player 1) ->")
         , Translate (ox - 18) (oy + s) (Rotate 90 (txt 0 0 14 (hsb 0 0 70) ("q = prob. of \"" ++ gR0 g ++ "\" (player 2) ->")))
         -- 右：利得表と説明
         , txt tx ty 18 white (show (wGi w + 1) ++ ": " ++ gName g)
         , txt (tx + 110) (ty + 34) 14 white ("vs " ++ gR0 g)
         , txt (tx + 230) (ty + 34) 14 white ("vs " ++ gR1 g)
         , txt tx (ty + 60) 14 white (gR0 g), txt (tx + 110) (ty + 60) 14 white (pair a00 b00), txt (tx + 230) (ty + 60) 14 white (pair a01 b01)
         , txt tx (ty + 86) 14 white (gR1 g), txt (tx + 110) (ty + 86) 14 white (pair a10 b10), txt (tx + 230) (ty + 86) 14 white (pair a11 b11)
         , txt tx (ty + 120) 14 (hsb 0 0 60) "(payoff of player 1, payoff of player 2)"
         ]
      ++ concat [ [ txt tx (ty + 160 + fromIntegral i * 24) 14 (hsb h 70 100) a, txt (tx + 110) (ty + 160 + fromIntegral i * 24) 14 (hsb 0 0 80) b ]
                | (i, (a, h, b)) <- zip [0 :: Int ..] legend ]
      ++ [ txt tx (ty + 280 + fromIntegral i * 26) 14 (hsb 0 0 80) l | (i, l) <- zip [0 :: Int ..] more ]
      ++ [ txt 20 34 15 white ("1-4: games   5: rock-paper-scissors   lambda = " ++ nf 1 lam ++ " (Up/Down)   click: start point   C: clear")
         , txt 20 62 14 (hsb 0 0 60) "Phi(p,q) = (sig(lambda*gain1(q)), sig(lambda*gain2(p))). White path: move a little toward the best response, repeat. It stops at a fixed point."
         ]
  where
    g = gameOf (wGi w)
    s = sizeOf env
    lam = wLam w
    toS (p, q) = (ox + p * s, oy + s - q * s)
    ss = [ fromIntegral i / 200 | i <- [0 .. 200 :: Int] ]
    stepV dd = if dd > 0 then 1 else if dd < 0 then 0 else 0.5
    tx = ox + s + 50
    ty = oy + 10
    ((a00, a01), (a10, a11)) = gA g
    ((b00, b01), (b10, b11)) = gB g
    pair a b = "(" ++ show (round a :: Int) ++ ", " ++ show (round b :: Int) ++ ")"
    legend =
      [ ("Cyan", 190, "= P1 best response (pale: step, solid: logit)")
      , ("Pink", 330, "= P2 best response")
      , ("Yellow ring", 45, "= Nash equilibrium (step lines cross)")
      , ("Yellow dot", 45, "= fixed point of Phi (logit lines cross)")
      ]
    more =
      [ "- Larger lambda (Up): the dot moves toward the ring"
      , "- Matching Pennies: one equilibrium; the path spirals"
      , "- Stag Hunt: 3 equilibria; the start decides the end"
      , "- Prisoner's Dilemma: both better off cooperating,"
      , "  yet the path goes to (Defect, Defect)"
      ]

triCorners :: Env -> (Point, Point, Point)
triCorners env =
  let s = sizeOf env / 0.9
  in ((ox, oy + s * 0.866), (ox + s, oy + s * 0.866), (ox + s / 2, oy))

triPos :: Env -> X3 -> Point
triPos env (x, y, z) =
  let ((ax, ay), (bx, by), (cx, cy)) = triCorners env
  in (x * ax + y * bx + z * cx, x * ay + y * by + z * cy)

drawRps :: Env -> World -> Picture
drawRps env w = screen env $
  [ Color (hsb 0 0 50) (thickPath 2 [pa, pb, pc, pa]) ]
  ++ [ Color (hsba 0 0 100 0.3) (Line [triPos env x, triPos env (x0 + dx * 0.6, y0 + dy * 0.6, z0 + dz * 0.6)])
     | i <- [0 .. 12 :: Int], j <- [0 .. 12 - i]
     , let x0 = (fromIntegral i + 0.3) / 12.9, let y0 = (fromIntegral j + 0.3) / 12.9, let z0 = 1 - x0 - y0
     , z0 >= 0.01, let x = (x0, y0, z0), let (dx, dy, dz) = repl (wWinA w) x ]
  ++ [ Color (hsba 45 80 100 0.7) (Line (map (triPos env) t)) | t <- wRps w ]
  ++ [ hoop 3 (hsb 45 90 100) (triPos env (1 / 3, 1 / 3, 1 / 3)) 22
     , txt (fst pa - 20) (snd pa + 30) 16 white "Rock"
     , txt (fst pb - 30) (snd pb + 30) 16 white "Scissors"
     , txt (fst pc - 20) (snd pc - 8) 16 white "Paper"
     , txt 20 34 15 white ("5: rock-paper-scissors   win points a = " ++ nf 2 (wWinA w) ++ " (Left/Right, loss = -1)   click: start   C: clear   1-4: 2x2 games")
     , txt 20 62 14 (hsb 0 0 60) "The center (1/3, 1/3, 1/3) is the Nash equilibrium = fixed point. a = 1: orbits forever; a > 1: sucked in; a < 1: spreads out."
     ]
  ++ [ txt tx (oy + 40 + fromIntegral i * 26) 14 (hsb 0 0 80) l | (i, l) <- zip [0 :: Int ..] notes ]
  where
    (pa, pb, pc) = triCorners env
    tx = fst pb + 24
    notes =
      [ "Each point = shares of rock, paper"
      , "and scissors players in a population"
      , "Replicator dynamics: moves that earn"
      , "more than average grow"
      , "  dx_i/dt = x_i ((Mx)_i - x^T M x)"
      , ""
      , "A fixed point need not be reached (a = 1)"
      , "-> as with |f'| in sketch 01, stability"
      , "   is a separate question"
      , "Living examples: 3 lizard types,"
      , "3 strains of bacteria"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | wGi w == 4 ->
        -- 画面の点 → 重心座標
        let ((ax, ay), (bx, by), (cx, cy)) = triCorners env
            det = (by - cy) * (ax - cx) + (cx - bx) * (ay - cy)
            l0 = ((by - cy) * (mx - cx) + (cx - bx) * (my - cy)) / det
            l1 = ((cy - ay) * (mx - cx) + (ax - cx) * (my - cy)) / det
            l2 = 1 - l0 - l1
            added = if l0 > 0.005 && l1 > 0.005 && l2 > 0.005 then wRps w ++ [[(l0, l1, l2)]] else wRps w
        in pure w { wRps = keepLast 8 added }
    | mx > ox && mx < ox + s && my > oy && my < oy + s ->
        pure w { wTrails = keepLast 10 (wTrails w ++ [[((mx - ox) / s, (oy + s - my) / s)]]) }
    | otherwise -> pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wLam = min 60 (wLam w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wLam = max 0.2 (wLam w / 1.25) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wWinA = max 0.5 (wWinA w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wWinA = min 1.5 (wWinA w + 0.05) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' ->
        let i = fromEnum c - fromEnum '1'
            rps = if i == 4 && null (wRps w) then [[(0.6, 0.25, 0.15)], [(0.4, 0.35, 0.25)]] else wRps w
        in pure w { wGi = i, wTrails = [], wRps = rps }
    | toLower c == 'c' -> pure w { wTrails = [], wRps = [] }
  _ -> pure w
  where
    (mx, my) = mouseP env
    s = sizeOf env

-- ---------- やってみよう ----------
-- 1. 鹿狩りで、どこから始めると (鹿, 鹿) に、どこからだと (ウサギ, ウサギ) に行く？ 境目はどこ？
-- 2. コイン合わせで λ を 40 にすると、軌跡は真ん中に着かずに回り続ける。eta を 0.02 にすると？
-- 3. 囚人のジレンマの利得 5 を 2 に変えると、ゲームはどう変わる？（協力が均衡になる）
-- 4. 自分で 2×2 のゲーム（デートの場所選び、など）を gameOf に足して、均衡の数を調べよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

keepLast :: Int -> [a] -> [a]
keepLast n xs = drop (length xs - n) xs

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ !x = x
nTimes n f !x = nTimes (n - 1) f (f x)
