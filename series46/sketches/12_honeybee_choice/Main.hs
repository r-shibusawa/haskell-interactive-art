-- ============================================
-- 46-12 ミツバチの巣選び ─ ダンスと定足数（クオラム）
-- 分蜂した群れは、新しい巣の候補地を偵察蜂が探して決める。良い候補地を見つけた蜂ほど長く 8 の字ダンスをして仲間を誘う
-- ダンスは行くたびに短くなる（熱が冷める）ので、悪い候補地の支持は自然に消える。1 つの候補地に Q 匹そろったら決定（定足数）
-- 操作：↑↓ = 定足数 Q / ←→ = 速さ / B = 30 回まとめて実験（速さ vs 正確さ）/ R = 候補地を作り直す / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl', mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Numeric (showFFloat)

nBees :: Int
nBees = 150                  -- ★ 偵察蜂の数

pSpont, pFollow :: Float
pSpont = 0.003               -- ★ 自分で探しに出る確率（1 分あたり）
pFollow = 0.05               -- ★ ダンスを見て付いていく強さ

dMax, dDec, travel, stay :: Int
dMax = 100                   -- ★ 最初のダンスの回数（質 1 のとき）
dDec = 15                    -- ★ 1 往復ごとに減る回数
travel = 8                   -- ★ 片道の時間（分）
stay = 10                    -- ★ 候補地で調べる時間（分）

siteHue :: [Float]
siteHue = [10, 45, 120, 190, 280]

-- 蜂の状態：休み・行き・候補地・帰り・ダンス
data St = Rest | Fly | Site | Back | Dance deriving Eq

data Bee = Bee { bSt :: !St, bK :: !Int, bT :: !Int, bD :: !Int, bX :: !Float, bY :: !Float }

data World = World
  { wQ       :: ![Float]                 -- 候補地の質
  , wBees    :: ![Bee]
  , wQuorum  :: !Int                     -- 定足数 Q（候補地に同時にいる蜂の数）
  , wTicks   :: !Int                     -- 1 フレームに進める分数
  , wNow     :: !Int                     -- 経過（分）
  , wDecided :: !Int                     -- 決まった候補地（−1 = 相談中、−2 = 決まらなかった）
  , wAt      :: !Int                     -- 決まった時刻
  , wHist    :: ![[Int]]                 -- 候補地ごとの蜂の数（新しい順、500 個まで）
  , wResults :: !(Map.Map Int (Float, Float))   -- 定足数ごとの（正確さ、平均の時間）
  , wBatch   :: !(Maybe (Int, Int, Int)) -- まとめ実験（終わった回数、正解の数、時間の合計）
  , wPaused  :: !Bool
  , wSeed    :: !Seed
  }

main :: IO ()
main = runSketch (sketch "46-12 Honeybee nest-site choice" (makeSites (blankWorld 1)))
  { skBack  = hsb 35 30 11
  , skStart = \_ -> makeSites . blankWorld <$> randomRIO (1, 2 ^ (62 :: Int))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

blankWorld :: Seed -> World
blankWorld s = World [] [] 15 1 0 (-1) 0 [] Map.empty Nothing False s

-- ★ 候補地の質（1 つだけ一番良い）を並べかえて置く
makeSites :: World -> World
makeSites w = resetRun w { wQ = q, wResults = Map.empty, wSeed = s' }
  where (q, s') = shuffle [0.3, 0.45, 0.55, 0.7, 0.9] (wSeed w)

resetRun :: World -> World
resetRun w = w { wBees = newBees, wNow = 0, wDecided = -1, wHist = [] }

-- 全員「休み中」の偵察蜂を nBees 匹つくる
newBees :: [Bee]
newBees = replicate nBees (Bee Rest (-1) 0 0 0 0)

siteAng :: Int -> Float
siteAng i = -1.0 + fromIntegral i * 2.0 / 4

bestSite :: [Float] -> Int
bestSite qs = snd (foldl' (\(bq, bi) (q, i) -> if q > bq then (q, i) else (bq, bi)) (-1, 0) (zip qs [0 ..]))

-- ★ 1 分ぶん進める。返り値：決まった候補地（まだなら −1）、候補地ごとの蜂の数、新しい蜂、種
tick :: [Float] -> Int -> Seed -> [Bee] -> (Int, [Int], [Bee], Seed)
tick qs quorum s0 bs = (kDone, atSite, bs', s1)
  where
    nS = length qs
    count st = [ length [ () | b <- bs, bSt b == st, bK b == k ] | k <- [0 .. nS - 1] ]
    dancers = count Dance
    atSite = count Site
    totalD = sum dancers
    (s1, bs') = mapAccumL upd s0 bs
    upd s b = case bSt b of
      Rest ->
        let (u1, sA) = rand01 s
        in if u1 < pSpont
             then let (u2, sB) = rand01 sA in (sB, go b (min (nS - 1) (floor (u2 * fromIntegral nS))))   -- 自分で探す
             else if totalD > 0
               then let (u2, sB) = rand01 sA
                    in if u2 < pFollow * (fromIntegral totalD / fromIntegral nBees)                     -- ダンスを見て付いていく
                         then let (u3, sC) = rand01 sB in (sC, go b (pick (u3 * fromIntegral totalD) 0 dancers))
                         else (sB, b)
               else (sA, b)
      st ->
        let t = bT b - 1
            b1 = b { bT = t }
        in (s, if t > 0 then b1 else case st of
              Fly -> b1 { bSt = Site, bT = stay }
              Site -> b1 { bSt = Back, bT = travel }
              Back ->
                let q = qs !!! bK b
                    d = if bD b < 0 then round (fromIntegral dMax * max 0 ((q - 0.2) / 0.8)) else bD b - dDec   -- 初めてなら質で決まる。2 回目からは減る
                in if d > 0 then b1 { bSt = Dance, bT = max 1 (round (fromIntegral d / 10 :: Float)), bD = d }  -- 1 分に 10 回まわる
                            else b1 { bSt = Rest, bK = -1, bD = d }                                          -- 熱が冷めた → 休む
              _ -> b1 { bSt = Fly, bT = travel })                                                             -- もう一度見に行く
    go b k = b { bSt = Fly, bK = k, bT = travel, bD = -1 }
    pick r k (d : ds) | k < nS - 1 && r >= fromIntegral d = pick (r - fromIntegral d) (k + 1) ds
    pick _ k _ = k
    kDone = case [ k | (k, c) <- zip [0 ..] atSite, c >= quorum ] of
      (k : _) -> k
      [] -> -1

-- まとめ実験の 1 回：決まるか 3000 分たつまで回す
fullRun :: [Float] -> Int -> Seed -> (Int, Int, Seed)
fullRun qs quorum = loop 0 newBees
  where
    loop t bs s
      | t >= 3000 = (-1, t, s)
      | otherwise = let (k, _, bs', s') = tick qs quorum s bs
                    in if k >= 0 then (k, t + 1, s') else loop (t + 1) bs' s'

step :: Env -> World -> World
step env w0 = moveBees env (batchStep w1)
  where
    w1 = if wPaused w0 || wDecided w0 >= 0 then w0 else runTicks (wTicks w0) w0
    runTicks 0 w = w
    runTicks n w =
      let (k, atSite, bs, s) = tick (wQ w) (wQuorum w) (wSeed w) (wBees w)
          t = wNow w + 1
          h = if even t then take 500 (atSite : wHist w) else wHist w
          w' = w { wBees = bs, wSeed = s, wNow = t, wHist = h }
      in if k >= 0 then w' { wDecided = k, wAt = t }
         else if t > 3000 then w' { wDecided = -2 }
         else runTicks (n - 1 :: Int) w'
    -- まとめ実験：1 フレームに 2 回ずつ
    batchStep w = case wBatch w of
      Just (d, c, tm) | d < 30 ->
        let one (d', c', tm', s) = let (k, t, s2) = fullRun (wQ w) (wQuorum w) s
                                   in (d' + 1, if k == bestSite (wQ w) then c' + 1 else c', tm' + t, s2)
            (d2, c2, tm2, s3) = (if d + 1 < 30 then one . one else one) (d, c, tm, wSeed w)
            res = if d2 == 30 then Map.insert (wQuorum w) (fromIntegral c2 / 30, fromIntegral tm2 / 30) (wResults w) else wResults w
        in w { wBatch = Just (d2, c2, tm2), wSeed = s3, wResults = res }
      _ -> w

-- 蜂を描く位置に少しずつ寄せる（休み中・候補地ではその場でうろうろ）
moveBees :: Env -> World -> World
moveBees env w = w { wBees = bs, wSeed = s' }
  where
    (sx, sy, sp) = layout env
    (s', bs) = mapAccumL mv (wSeed w) (wBees w)
    mv s b =
      let (u1, sA) = rand01 s
          (u2, sB) = rand01 sA
          (tx, ty) = case bSt b of
            Rest -> (sx + (u1 - 0.5) * 90, sy + (u2 - 0.5) * 120)
            Site -> let (px, py) = sp (bK b) in (px + (u1 - 0.5) * 50, py + (u2 - 0.5) * 40)
            Dance -> let a = siteAng (bK b)
                         ph = fromIntegral (envFrame env) * 0.25 + fromIntegral (bT b)
                     in (sx + 40 * cos a + 10 * sin ph, sy + 40 * sin a + 6 * sin (2 * ph))
            st -> let u = if st == Fly then 1 - fromIntegral (bT b) / fromIntegral travel else fromIntegral (bT b) / fromIntegral travel
                      (px, py) = sp (bK b)
                  in (lerpF sx px u, lerpF sy py u)
      in (sB, b { bX = bX b + (tx - bX b) * 0.3, bY = bY b + (ty - bY b) * 0.3 })

-- 群れの中心と、候補地 k の位置
layout :: Env -> (Float, Float, Int -> Point)
layout env = (sx, sy, \k -> (sx + rr * cos (siteAng k), sy + rr * sin (siteAng k)))
  where
    sx = envW env * 0.2
    sy = envH env * 0.55
    rr = min (envW env * 0.36) (envH env * 0.4)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 群れ（左）と候補地（右の弧）
  [ Color (hsb 40 50 40) (Translate sx sy (Scale 1 1.27 (disc 55))) ]
  ++ concat [ [ Color (hsb hue 50 35) (rect x y 68 52)
              , Color (hsb hue 70 100) (rect (x - 30 + 30 * q) (y + 18) (60 * q) 8)
              , txt (x - 30) (y - 8) 14 (hsb 0 0 100) ("q " ++ fmt 2 q)
              , txt (x - 30) (y + 10) 14 (hsb 0 0 100) (show c ++ " bees") ]
              ++ [ Color (hsb 50 90 100) (Pictures [ thickLine 4 a b | (a, b) <- box (x - 40) (y - 32) 80 64 ]) | wDecided w == k ]
            | (k, q, hue, c) <- zip4' [0 ..] (wQ w) siteHue cnt, let (x, y) = sp k ]
  ++ [ dot (if bK b >= 0 then hsb (siteHue !!! bK b) 70 100 else hsb 45 20 100) (bX b) (bY b) (if bSt b == Dance then 7 else 4)
     | b <- wBees w ]
  ++ [ txt (sx - 30) (sy + 92) 14 (hsb 0 0 100) "swarm" ]
  -- ---- 右：候補地ごとの「いる蜂の数」の推移と、定足数の線
  ++ [ Color (hsbA 0 0 0 35) (rect (gx + gw / 2) (gy + gh / 2) gw gh)
     , seg 1 (hsbA 0 0 100 50) (gx, qy) (gx + gw, qy) ]
  ++ [ path 2 (hsb hue 70 100) [ (gx + fromIntegral i / 499 * gw, gy + gh - min 1 (fromIntegral c / top) * gh)
                               | (i, h) <- zip [0 :: Int ..] hist, let c = h !!! k ]
     | length hist >= 2, (k, hue) <- zip [0 ..] siteHue ]
  ++ [ txt gx (gy - 8) 14 (hsb 0 0 85) ("Bees at each site (white line = quorum Q = " ++ show (wQuorum w) ++ ")")
     , txt gx (gy + gh + 30) 16 (hsb 0 0 100) msg
  -- 速さ vs 正確さ
     , txt gx ty0 14 (hsb 0 0 85) "B: 30 trials (accuracy and time to decide, per quorum)" ]
  ++ [ txt gx (ty0 + 24) 14 (hsb 50 80 100) ("running... " ++ show d ++ " / 30") | Just (d, _, _) <- [wBatch w], d < 30 ]
  ++ concat [ [ txt gx y 14 (hsb 0 0 80) ("Q = " ++ show q)
              , Color (hsb 120 60 90) (rect (gx + 64 + bw / 2) (y - 5) bw 14)
              , txt (gx + 70 + bw) y 14 (hsb 0 0 80) (fmt 0 (acc * 100) ++ " %")
              , txt (gx + gw - 90) y 14 (hsb 30 70 100) (fmt 0 tm ++ " min") ]
            | (i, (q, (acc, tm))) <- zip [0 :: Int ..] (take 9 (Map.toAscList (wResults w)))
            , let y = ty0 + 50 + fromIntegral i * 26, let bw = acc * (gw - 220) * 0.5 ]
  ++ [ txt gx (ty0 + 50 + fromIntegral (min 9 (Map.size (wResults w))) * 26) 14 (hsb 0 0 60)
         "green bar = share that chose the best site, right = mean time"
     | not (Map.null (wResults w)) ]
  ++ [ txt 20 36 17 (hsb 0 0 100) ("Up/Down: quorum Q = " ++ show (wQuorum w) ++ "   Left/Right: speed x" ++ show (wTicks w)
                                   ++ "   B: 30 trials   R: new sites   Space: " ++ (if wPaused w then "paused" else "running")
                                   ++ " (next run)")
     , txt 20 60 14 (hsb 0 0 70) ("Dance length is set by quality and drops by " ++ show dDec
                                  ++ " each round trip. Only good sites keep their support and keep recruiting") ]
  where
    (sx, sy, sp) = layout env
    cnt = [ length [ () | b <- wBees w, bSt b == Site, bK b == k ] | k <- [0 .. length (wQ w) - 1] ]
    gx = envW env * 0.62
    gw = envW env - gx - 30
    gy = 110
    gh = 170
    top = max (fromIntegral (wQuorum w) * 1.5) 10
    qy = gy + gh - fromIntegral (wQuorum w) / top * gh
    hist = reverse (wHist w)
    ty0 = gy + gh + 80
    msg | wDecided w >= 0 = "Decided! site with q " ++ fmt 2 (wQ w !!! wDecided w) ++ " ("
                            ++ (if wDecided w == bestSite (wQ w) then "the best site: yes" else "not the best site: no")
                            ++ ") after " ++ show (wAt w) ++ " min"
        | wDecided w == -2 = "No decision after 3000 minutes"
        | otherwise = "Elapsed " ++ show (wNow w) ++ " min   discussing..."
    box x y bw bh = let ps = [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)] in zip ps (drop 1 ps ++ take 1 ps)

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wQuorum = min 60 (wQuorum w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wQuorum = max 5 (wQuorum w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTicks = min 16 (wTicks w * 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTicks = max 1 (wTicks w `div` 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> if wDecided w /= -1 then resetRun w else w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> w { wBatch = Just (0, 0, 0) }
    | c `elem` "rR" -> makeSites w
  _ -> w

-- ---------- やってみよう ----------
-- 1. Q = 15 で B。正確さと時間はいくつ？ Q = 5 と Q = 40 でも B をして、表を比べよう（速さ vs 正確さのトレードオフ）
-- 2. 本物のミツバチの定足数は 20〜30 匹くらい。それはなぜ？ 表から考えよう
-- 3. dDec を 0 にすると、ダンスが冷めない。悪い候補地の支持も消えず、決まらなかったり間違えたりする？
-- 4. pSpont を大きくすると、自分で探しに行く蜂が増える。みんなが自分で調べると時間はどうなる？
-- 5. 人間の会議にたとえると？ 「熱が冷める」「定足数」は何に当たるだろう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

-- リストの i 番目（範囲外なら最後の要素、空なら 0）
(!!!) :: Num a => [a] -> Int -> a
xs !!! i = case drop (max 0 i) xs of
  (v : _) -> v
  [] -> foldl' (\_ v -> v) 0 xs

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 0〜1 の乱数と次の種
rand01 :: Seed -> (Float, Seed)
rand01 s = let s' = nextSeed s in (fromIntegral (s' `shiftR` 40) / 16777216, s')

-- リストをでたらめに並べかえる（フィッシャー–イェーツ）
shuffle :: [a] -> Seed -> ([a], Seed)
shuffle xs s0 = go (length xs - 1) xs s0
  where
    go i ys s
      | i <= 0 = (ys, s)
      | otherwise =
          let (u, s') = rand01 s
              j = min i (floor (u * fromIntegral (i + 1)))
              yi = ys !! i
              yj = ys !! j
              ys' = [ if n == i then yj else if n == j then yi else y | (n, y) <- zip [0 ..] ys ]
          in go (i - 1) ys' s'
