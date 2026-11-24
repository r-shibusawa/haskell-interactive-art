-- ============================================
-- 66-12 ゲームの敵キャラの状態機械 ─ 敵の頭の中は「いまの状態」と「何が起きたら次はどれ」の表
-- 通常モードの中に 巡回 → 発見 → 追跡 → 見失う → 探索 → 巡回 の小さな状態機械があり、その外側に「逃走」がある（階層的な状態機械）。
-- 体力が 30 を切ると、中でどの状態にいても外側の規則が優先して逃走へ。回復したら通常モードの入口（巡回）から再開。
-- 操作：マウス = あなた（見つかると追いかけられる）/ クリック = 近くの敵に攻撃（体力 −25）/ 壁のうしろは見えない
--       V 視界を表示 / R 敵を元にもどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 状態：通常モードの中の 5 つ（巡回・発見・追跡・見失う・探索）と、外側の「逃走」
data St = Patrol | Spot | Chase | Lost | Search | Flee deriving (Eq, Show, Enum, Bounded)

subs :: [St]
subs = [Patrol, Spot, Chase, Lost, Search]

stName :: St -> String                        -- 画面の名前
stName s = case s of
  Patrol -> "Patrol"
  Spot -> "Spot"
  Chase -> "Chase"
  Lost -> "Lost"
  Search -> "Search"
  Flee -> "Flee"

stJp :: St -> String                          -- 自己テストの名前（p5.js 版と同じ）
stJp s = case s of
  Patrol -> "巡回"
  Spot -> "発見"
  Chase -> "追跡"
  Lost -> "見失う"
  Search -> "探索"
  Flee -> "逃走"

viewDist, fov :: Double
viewDist = 230                                -- ★ 見える距離
fov = pi / 2                                  -- ★ 視野の角度

speedOf :: St -> Double                       -- ★ 状態ごとの速さ
speedOf s = case s of
  Patrol -> 1.2
  Spot -> 0
  Chase -> 2.4
  Lost -> 1.8
  Search -> 1.0
  Flee -> 2.6

timeSpot, timeLost, timeSearch :: Int
timeSpot = 30                                 -- ★ フレーム数のしきい値
timeLost = 90
timeSearch = 200

lowHp, okHp :: Double
lowHp = 30                                    -- ★ 逃走に入る体力
okHp = 80                                     -- ★ 通常にもどる体力

enemyCols :: [RGB]
enemyCols = [(255, 170, 50), (70, 215, 230), (185, 150, 255)]

data Box = Box { bx :: !Double, by :: !Double, bw :: !Double, bh :: !Double }

data Enemy = Enemy
  { ex :: !Double, ey :: !Double, eDir :: !Double, eHp :: !Double
  , eSt :: !St, eT :: !Int                    -- 状態と、その状態になってからのフレーム数
  , eWay :: [(Double, Double)], eWi :: !Int   -- 巡回の道順
  , eLast :: Maybe (Double, Double)           -- 最後に見たあなたの位置
  , eCol :: RGB, eCool :: !Int }

data Ev = Ev { evSees :: !Bool, evHp :: !Double, evArrived :: !Bool }

data World = World
  { wArena   :: Box
  , wWalls   :: [Box]
  , wEnemies :: [Enemy]
  , wPlayer  :: (Double, Double)
  , wShowView :: !Bool
  , wHits    :: !Int
  , wCaught  :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-12 Enemy AI state machine" (buildWorld 1280 800))
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (buildWorld (realToFrac (envW env)) (realToFrac (envH env))) { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

-- 世界を作る（自己テストは画面の大きさによらず 1280 × 800）
buildWorld :: Double -> Double -> World
buildWorld w h =
  let a = Box 30 90 (w * 0.58) (h - 130)
      walls = [ Box (bx a + x * bw a) (by a + y * bh a) (ww * bw a) (wh * bh a) | (x, y, ww, wh) <- [(0.25, 0.2, 0.06, 0.35), (0.55, 0.55, 0.3, 0.06), (0.6, 0.12, 0.06, 0.25), (0.15, 0.75, 0.25, 0.05)] ]
  in World a walls (map (makeEnemy a) [0, 1, 2]) (200, 400) True 0 0 []

makeEnemy :: Box -> Int -> Enemy
makeEnemy a k =
  let way = [ (bx a + x * bw a, by a + (y + i2d k * 0.25) * bh a) | (x, y) <- [(0.1, 0.1), (0.9, 0.1), (0.9, 0.45), (0.1, 0.45)] ]
      (x0, y0) = headOr (0, 0) way
  in Enemy x0 y0 0 100 Patrol 0 way 0 Nothing (at' enemyCols k ink) 0

-- ★ 状態遷移の表（純粋な関数：いまの状態 + できごと → 次の状態）。外側（逃走）の規則を先に調べる
nextState :: St -> Int -> Ev -> St
nextState s t ev
  | s /= Flee && evHp ev < lowHp = Flee
  | s == Flee = if evHp ev >= okHp then Patrol else Flee
  | otherwise = case s of
      Patrol | evSees ev -> Spot
      Spot | not (evSees ev) -> Patrol
           | t >= timeSpot -> Chase
      Chase | not (evSees ev) -> Lost
      Lost | evSees ev -> Chase
           | t >= timeLost || evArrived ev -> Search
      Search | evSees ev -> Spot
             | t >= timeSearch -> Patrol
      _ -> s

-- ---- 見えるか：距離・視野の角度・壁でさえぎられないか
segHitsRect :: Double -> Double -> Double -> Double -> Box -> Bool
segHitsRect x1 y1 x2 y2 r = or [ x > bx r && x < bx r + bw r && y > by r && y < by r + bh r
                               | k <- [0 .. 24 :: Int], let x = x1 + ((x2 - x1) * i2d k) / 24, let y = y1 + ((y2 - y1) * i2d k) / 24 ]

hypD :: Double -> Double -> Double
hypD a b = sqrt (a * a + b * b)

wrapAng :: Double -> Double                   -- −π〜π にそろえる
wrapAng a = atan2 (sin a) (cos a)

canSee :: [Box] -> Enemy -> (Double, Double) -> Bool -> Bool
canSee walls e (px, py) chasing =
  let dx = px - ex e
      dy = py - ey e
      d = hypD dx dy
      da = wrapAng (atan2 dy dx - eDir e)
  in not (d > viewDist) && not (not chasing && abs da > fov / 2)          -- 追跡中はぐるりと見回せる
     && not (any (segHitsRect (ex e) (ey e) px py) walls)

inWall :: [Box] -> Double -> Double -> Bool
inWall walls x y = any (\w -> x > bx w - 8 && x < bx w + bw w + 8 && y > by w - 8 && y < by w + bh w + 8) walls

-- 目標へ向きを変えながら進む（返り値の 1 つ目 = 動く前の距離）
moveToward :: Box -> [Box] -> Double -> Double -> Double -> Enemy -> (Double, Enemy)
moveToward a walls tx ty sp e
  | d < 1e-6 || sp <= 0 = (d, e)
  | otherwise =
      let want = atan2 dy dx
          da = wrapAng (want - eDir e)
          dir1 = eDir e + clampv da (-0.15) 0.15
          nx = ex e + cos dir1 * sp
          ny = ey e + sin dir1 * sp
          x1 = if not (inWall walls nx (ey e)) then nx else ex e     -- 壁にぶつかったらその向きは動かない
          y1 = if not (inWall walls x1 ny) then ny else ey e
          dir2 = if inWall walls (x1 + cos dir1 * 12) (y1 + sin dir1 * 12) then dir1 + 0.6 else dir1   -- 壁ぞいに向きを変える
      in (d, e { eDir = dir2, ex = clampv x1 (bx a + 10) (bx a + bw a - 10), ey = clampv y1 (by a + 10) (by a + bh a - 10) })
  where dx = tx - ex e
        dy = ty - ey e
        d = hypD dx dy

-- 1 フレーム：見る → 状態ごとに動く → 表で次の状態へ（2 つ目 = つかまえたか）
updateEnemy :: Box -> [Box] -> (Double, Double) -> Enemy -> (Enemy, Bool)
updateEnemy a walls p@(px, py) e0 =
  let chasing = eSt e0 == Chase || eSt e0 == Lost
      sees = canSee walls e0 p chasing
      e1 = if sees then e0 { eLast = Just p } else e0
      sp = speedOf (eSt e1)
      mv tx ty e = moveToward a walls tx ty sp e
      (arrived, e2) = case eSt e1 of
        Patrol -> let (wx, wy) = at' (eWay e1) (eWi e1) (ex e1, ey e1)
                      (d, e') = mv wx wy e1
                  in (False, if d < 12 then e' { eWi = (eWi e' + 1) `mod` max 1 (length (eWay e')) } else e')
        Spot -> let da = atan2 (py - ey e1) (px - ex e1) - eDir e1
                in (False, e1 { eDir = eDir e1 + clampv (wrapAng da) (-0.1) 0.1 })
        Chase -> (False, snd (mv px py e1))
        Lost -> case eLast e1 of
          Just (lx, ly) -> let (d, e') = mv lx ly e1 in (d < 10, e')
          Nothing -> (False, e1)
        Search -> let e' = e1 { eDir = eDir e1 + 0.04 } in (False, snd (mv (ex e' + cos (eDir e') * 30) (ey e' + sin (eDir e') * 30) e'))
        Flee -> let (_, e') = mv (2 * ex e1 - px) (2 * ey e1 - py) e1 in (False, e' { eHp = min 100 (eHp e' + 0.25) })
      ns = nextState (eSt e2) (eT e2) (Ev sees (eHp e2) arrived)
      e3 = if ns /= eSt e2 then e2 { eSt = ns, eT = 0 } else e2 { eT = eT e2 + 1 }
      e4 = if eCool e3 > 0 then e3 { eCool = eCool e3 - 1 } else e3
      caught = eSt e4 == Chase && hypD (px - ex e4) (py - ey e4) < 16 && not (eCool e4 > 0)   -- つかまえた（1.5 秒に 1 回だけ数える）
  in if caught then (e4 { eCool = 90 }, True) else (e4, False)

-- クリック：近くの敵に −25
attack :: Double -> Double -> World -> World
attack x y w =
  let hit e = hypD (ex e - x) (ey e - y) < 60
      es = [ if hit e then e { eHp = max 0 (eHp e - 25) } else e | e <- wEnemies w ]
  in w { wEnemies = es, wHits = wHits w + length (filter hit (wEnemies w)) }

step :: Env -> World -> World
step env w0 =
  let (mx, my) = mouseP env
      (mxd, myd) = (realToFrac mx, realToFrac my)
      a = wArena w0
      pl = if mxd > bx a && mxd < bx a + bw a && myd > by a && myd < by a + bh a then (mxd, myd) else wPlayer w0
      res = map (updateEnemy a (wWalls w0) pl) (wEnemies w0)
  in w0 { wPlayer = pl, wEnemies = map fst res, wCaught = wCaught w0 + length (filter snd res) }

fsmPos :: Env -> [Pos]
fsmPos env = let fx = envW env * 0.62
                 fw = envW env * 0.36
             in circleLayout 5 (fx + fw * 0.5) 300 (min (fw * 0.3) 140) pi ++ [Pos (fx + fw * 0.5) 560 (Just (pi / 2))]

-- 図の辺（ラベル → 行き先）
fsmEdges :: [Edge]
fsmEdges = edgesFrom [ [("found", [1])], [("0.5 s", [2]), ("out of sight", [0])], [("out of sight", [3])]
                     , [("seen", [2]), ("1.5 s", [4])], [("found", [1]), ("3 s", [0])], [("HP>=80", [0])] ]

stIndex :: St -> Int
stIndex = fromEnum

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle "Enemy AI as a state machine (hierarchical)"
             ("Mouse = you   Click = hit nearby enemies (HP -25)   V view   R reset      hits " ++ show (wHits w) ++ "   caught " ++ show (wCaught w))
  -- ---- 世界
  , Color (col (14, 30, 22)) (rectTL (f2 (bx a)) (f2 (by a)) (f2 (bw a)) (f2 (bh a)))
  , Color (col wire) (Pictures [ rectTL (f2 (bx b)) (f2 (by b)) (f2 (bw b)) (f2 (bh b)) | b <- wWalls w ])
  , Pictures (map drawEnemy (wEnemies w))
  , let (px, py) = wPlayer w in Pictures [ Color white (circleAt (f2 px) (f2 py) 16), Color (col rose) (ringAt (f2 px) (f2 py) 16 2) ]
  -- ---- 階層的な状態機械の図
  , Color (col dimc) (frameTL 1.5 (fx + 10) 130 (fw - 20) 360)
  , say "Normal mode" (fx + 26) 154 15 (col dimc)
  , say "entry = Patrol" (fx + 26) 476 15 (col dimc)
  , drawMachine defOpt { oR = 30, oNames = Just (map stName [minBound .. maxBound]), oHi = hi, oLabelSize = 14, oNameSize = Just 14 }
                6 0 (replicate 6 False) fsmEdges fp
  -- 外側の規則（通常のどこからでも → 逃走）
  , arrowLine [(fx + fw * 0.5 + 60, 490), (fx + fw * 0.5 + 60, 520), (pX fleeP + 28, pY fleeP - 12)] rose 2.5 False
  , sayRM "HP < 30 (from anywhere inside)" (fx + fw * 0.5 + 50) 505 14 (col rose)
  , Pictures [ let p = at' fp (stIndex (eSt e)) (Pos 0 0 Nothing) in Color (col (eCol e)) (circleAt (pX p - 20 + i2f k * 20) (pY p + 40) 12)
             | (k, e) <- zip [0 :: Int ..] (wEnemies w) ]
  -- 敵の一覧
  , Pictures [ say ("Enemy " ++ show (k + 1) ++ ": " ++ (if eSt e == Flee then "Flee" else "Normal / " ++ stName (eSt e)) ++ "   HP " ++ show (jsRound (eHp e)) ++ "   " ++ show (eT e) ++ " frames")
                   (fx + 10) (640 + i2f k * 24) 14 (col (eCol e))
             | (k, e) <- zip [0 :: Int ..] (wEnemies w) ]
  , drawSelfTest env (wTest w) ]
  where
    a = wArena w
    fx = envW env * 0.62
    fw = envW env * 0.36
    fp = fsmPos env
    fleeP = at' fp 5 (Pos 0 0 Nothing)
    hi = foldl' (\m e -> IM.insertWith (\_ old -> old) (stIndex (eSt e)) (eCol e) m) IM.empty (wEnemies w)
    drawEnemy e =
      let x = f2 (ex e)
          y = f2 (ey e)
          d = f2 (eDir e)
          vr = f2 viewDist
          ch = eSt e == Chase || eSt e == Lost
          cone = Polygon ((x, y) : [ (x + vr * cos t, y + vr * sin t) | k <- [0 .. 24 :: Int], let t = d - f2 fov / 2 + f2 fov * i2f k / 24 ])
      in Pictures
           [ if wShowView w then Color (colA (if eSt e == Chase then 40 else 22) (eCol e)) (if ch then circleAt x y (vr * 2) else cone) else Blank
           , case eLast e of
               Just (lx, ly) | eSt e == Lost -> Color (col (eCol e)) (Pictures [ ringAt (f2 lx) (f2 ly) 18 1, lineW 1 (f2 lx - 6, f2 ly) (f2 lx + 6, f2 ly) ])
               _ -> Blank
           , Color (col (eCol e)) (circleAt x y 24)
           , Color (col (10, 20, 15)) (circleAt (x + cos d * 7) (y + sin d * 7) 7)
           , Color (col (60, 20, 20)) (rectTL (x - 16) (y - 24) 32 5)
           , Color (col (if eHp e < lowHp then rose else lime)) (rectTL (x - 16) (y - 24) (32 * f2 (eHp e) / 100) 5)
           , sayC (stName (eSt e)) x (y - 30) 14 (col ink) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> let (x, y) = toP5 env p in attack (realToFrac x) (realToFrac y) w
  EventKey (Char c) Down _ _
    | isKey 'v' c -> w { wShowView = not (wShowView w) }
    | isKey 'r' c -> w { wEnemies = map (makeEnemy (wArena w)) [0, 1, 2], wHits = 0, wCaught = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 敵のうしろからそっと近づいてクリックで攻撃しよう。視野の外なら見つからない（V で視界を表示）
-- 2. 体力を減らすと、追跡中でも逃走に切りかわる。逃げ切って回復すると巡回から再開する（入口）
-- 3. 「逃走から戻ったら、前にいた状態から再開」（履歴つきの階層）にするには nextState をどう変える？
-- 4. 新しい状態「援軍を呼ぶ」を足そう：発見したとき近くの仲間も発見にする
-- 5. 状態機械は 00 の自動販売機と同じしくみ。表（nextState）を変えるだけで、キャラクターの性格が変わる

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("巡回 + 見つけた → 発見、発見 + 30 フレーム見え続けた → 追跡、発見 + 見えない → 巡回", ok Patrol 0 seesEv Spot && ok Spot 30 seesEv Chase && ok Spot 5 ev0 Patrol)
  , ("追跡 + 見えない → 見失う、見失う + 90 フレーム → 探索、探索 + 200 フレーム → 巡回、探索 + 見つけた → 発見", ok Chase 0 ev0 Lost && ok Lost 90 ev0 Search && ok Search 200 ev0 Patrol && ok Search 3 seesEv Spot)
  , ("体力 < 30 なら、中の 5 つのどの状態からでも逃走（外側の規則が優先）", all (\s -> ok s 50 (Ev True 20 False) Flee) subs)
  , ("逃走 + 体力 ≥ 80 → 通常の入口（巡回）、体力 < 80 なら逃走のまま", ok Flee 0 ev0 { evHp = 85 } Patrol && ok Flee 0 ev0 { evHp = 50 } Flee)
  , ("模擬プレイ 2400 フレーム：位置と体力はいつも範囲内（NaN なし）", okPos)
  , ("模擬プレイで 6 つの状態を全部通った（" ++ joinWith "・" (map stJp seen) ++ "）", length seen == 6)
  , ("壁の向こうは見えない：壁をはさんだ 2 点の線は壁に当たる"
    , segHitsRect (bx w0 - 40) (by w0 + bh w0 / 2) (bx w0 + bw w0 + 40) (by w0 + bh w0 / 2) w0 && not (segHitsRect (bx w0 - 40) (by w0 - 20) (bx w0 + bw w0 + 40) (by w0 - 20) w0)) ]
  where
    ev0 = Ev False 100 False
    seesEv = Ev True 100 False
    ok s t ev want = nextState s t ev == want
    -- 2400 フレームの模擬プレイ（プレイヤーは円を描いて動き、ときどき攻撃）
    world = buildWorld 1280 800
    ar = wArena world
    w0 = headOr (Box 0 0 0 0) (wWalls world)
    (seen, okPos) = simulate 2400
    simulate n = go 0 world [] True
      where
        go f w sn okp
          | f >= n = (sn, okp)
          | otherwise =
              let p = (bx ar + bw ar * (0.5 + 0.35 * cos (i2d f * 0.01)), by ar + bh ar * (0.5 + 0.35 * sin (i2d f * 0.013)))
                  w1 = if f `mod` 200 == 100
                         then let k = floor ((i2d f / 200) - 3 * i2d (floor (i2d f / 200 / 3) :: Int)) :: Int
                                  e = at' (wEnemies w) k (headOr (makeEnemy ar 0) (wEnemies w))
                              in attack (ex e) (ey e) w
                         else w
                  es = map (fst . updateEnemy ar (wWalls w1) p) (wEnemies w1)
                  sn' = foldl' (\acc e -> if eSt e `elem` acc then acc else acc ++ [eSt e]) sn es
                  okp' = okp && all (\e -> not (isNaN (ex e) || isNaN (ey e) || isInfinite (ex e) || isInfinite (ey e))
                                           && ex e >= bx ar && ex e <= bx ar + bw ar && ey e >= by ar && ey e <= by ar + bh ar && eHp e >= 0 && eHp e <= 100) es
              in go (f + 1) w1 { wEnemies = es } sn' okp'

-- ==================================================
-- ここから下は「オートマトンの道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type RGB = (Float, Float, Float)

bgCol, ink, amber, cyan, lime, rose, violet, dimc, panel, wire :: RGB
bgCol = (9, 20, 15)                           -- 背景（回路基板の黒緑）
ink = (226, 240, 228)                         -- 文字（蛍光の白）
amber = (255, 170, 50)
cyan = (70, 215, 230)
lime = (140, 230, 120)
rose = (255, 100, 120)
violet = (185, 150, 255)
dimc = (125, 155, 138)
panel = (18, 36, 28)
wire = (44, 74, 58)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- i 番目を取りのぞく（splice(i, 1)）
removeAt :: Int -> [a] -> [a]
removeAt i xs = [x | (k, x) <- zip [0 ..] xs, k /= i]

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab が SpecialKey で来る）
data TypedKey = TChar Char | TBack | TEnter | TTab | TOther

typedKey :: Key -> TypedKey
typedKey k = case k of
  Char '\b' -> TBack
  Char '\DEL' -> TBack
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> TBack
  SpecialKey KeyDelete -> TBack               -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  _ -> TOther

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.12、2）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- n を 2 進数の文字列に（n.toString(2)）
toBin :: Int -> String
toBin n = if n <= 0 then "0" else reverse (go n)
  where go 0 = ""
        go k = (if odd k then '1' else '0') : go (k `div` 2)

-- 2 進数の文字列を数に（parseInt(s, 2)。0 と 1 でない文字で止まる）
fromBin :: String -> Int
fromBin = foldl' (\a c -> 2 * a + (if c == '1' then 1 else 0)) 0 . takeWhile (`elem` "01")

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndPick :: String -> R Char                   -- 文字の中から 1 つ
rndPick cs = do u <- rnd; pure (at' cs (floor (u * fromIntegral (length cs))) ' ')

rndString :: String -> Int -> R String        -- alpha の文字で長さ len の文字列
rndString alpha len = mapM (const (rndPick alpha)) [1 .. len]

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 有限オートマトンの道具
-- DFA = 状態の数・記号・遷移（dTr !! q で、記号 → 次の状態）・開始・受理
--   遷移がない記号 = 「行き止まり」（そこで不受理が決まる）
-- NFA = 遷移が「次の状態のリスト」。記号 epsSym が ε 遷移
type Sym = Char

data DFA = DFA { dN :: !Int, dAlpha :: [Sym], dTr :: [M.Map Sym Int], dStart :: !Int, dAcc :: [Bool] }
data NFA = NFA { nN :: !Int, nAlpha :: [Sym], nTr :: [M.Map Sym [Int]], nStart :: !Int, nAcc :: [Bool] }

epsSym :: Sym                                 -- ε（文字を読まずに進む）
epsSym = '\0'

mkDfa :: [Sym] -> [[(Sym, Int)]] -> Int -> [Bool] -> DFA
mkDfa alpha rows st acc = DFA (length rows) alpha (map M.fromList rows) st acc

mkNfa :: [Sym] -> [[(Sym, [Int])]] -> Int -> [Bool] -> NFA
mkNfa alpha rows st acc = NFA (length rows) alpha (map M.fromList rows) st acc

dfaNext :: DFA -> Int -> Sym -> Int
dfaNext d q c
  | q < 0 = -1
  | otherwise = case M.lookup c (at' (dTr d) q M.empty) of
      Just t | t >= 0 && t < dN d -> t
      _ -> -1

-- たどった状態の列（-1 = 行き止まり）
dfaTrace :: DFA -> String -> [Int]
dfaTrace d = scanl (dfaNext d) (dStart d)

dfaAccepts :: DFA -> String -> Bool
dfaAccepts d s = go (dStart d) s
  where go q [] = q >= 0 && at' (dAcc d) q False
        go q (c : cs) = let t = dfaNext d q c in t >= 0 && go t cs

-- 行き止まりを「ごみ箱」状態にまとめて、全部の遷移をそろえる（返り値の 2 つ目 = ごみ箱の番号、なければ −1）
completeDfaS :: DFA -> (DFA, Int)
completeDfaS d =
  let missing = or [ dfaNext d q c < 0 | q <- [0 .. dN d - 1], c <- dAlpha d ]
      sink = dN d
      fillRow q = foldl' (\m c -> if dfaNext d q c < 0 then M.insert c sink m else m) (at' (dTr d) q M.empty) (dAlpha d)
  in if not missing then (d, -1) else
       ( d { dN = dN d + 1
           , dTr = [ fillRow q | q <- [0 .. dN d - 1] ] ++ [M.fromList [ (c, sink) | c <- dAlpha d ]]
           , dAcc = take (dN d) (dAcc d ++ repeat False) ++ [False] }
       , sink )

completeDfa :: DFA -> DFA
completeDfa = fst . completeDfaS

complementDfa :: DFA -> DFA                   -- 受理と不受理を入れかえる
complementDfa d = let e = completeDfa d in e { dAcc = map not (dAcc e) }

data Op = OpAnd | OpOr | OpDiff | OpXor deriving (Eq, Show)

opFun :: Op -> Bool -> Bool -> Bool
opFun o x y = case o of
  OpAnd -> x && y
  OpOr -> x || y
  OpDiff -> x && not y
  OpXor -> x /= y

data Product = Product { prD :: DFA, prPairs :: [(Int, Int)], prLeft :: DFA, prRight :: DFA }

-- 積の構成：状態 = (A の状態, B の状態) の組。たどれる組だけ、見つけた順に番号をつける
productDfa :: DFA -> DFA -> Op -> Product
productDfa a0 b0 o =
  let a = completeDfa a0
      b = completeDfa b0
      alpha = filter (`elem` dAlpha b) (dAlpha a)
      start = (dStart a, dStart b)
      loop h ids prs n rows
        | h >= n = (prs, reverse rows)
        | otherwise =
            let (p, q) = IM.findWithDefault (0, 0) h prs
                addSym (ids1, prs1, n1, row) c =
                  let t = (dfaNext a p c, dfaNext b q c)
                  in case M.lookup t ids1 of
                       Just k -> (ids1, prs1, n1, M.insert c k row)
                       Nothing -> (M.insert t n1 ids1, IM.insert n1 t prs1, n1 + 1, M.insert c n1 row)
                (ids', prs', n', row') = foldl' addSym (ids, prs, n, M.empty) alpha
            in loop (h + 1) ids' prs' n' (row' : rows)
      (pairsM, trs) = loop 0 (M.singleton start 0) (IM.singleton 0 start) 1 []
      pairs = IM.elems pairsM
      accOf (p, q) = opFun o (at' (dAcc a) p False) (at' (dAcc b) q False)
  in Product (DFA (length pairs) alpha trs 0 (map accOf pairs)) pairs a b

-- 幅優先探索：受理状態までの一番短い文字列（Nothing = 空言語）
dfaShortestAccepted :: DFA -> Maybe String
dfaShortestAccepted d
  | dStart d < 0 || dStart d >= dN d = Nothing
  | otherwise = go [dStart d] [] (IM.singleton (dStart d) "")
  where
    go [] [] _ = Nothing
    go [] next seen = go (reverse next) [] seen
    go (q : qs) next seen
      | at' (dAcc d) q False = Just (reverse (IM.findWithDefault "" q seen))
      | otherwise =
          let path = IM.findWithDefault "" q seen
              (next', seen') = foldl' (\(nx, sn) c -> let t = dfaNext d q c in
                                          if t >= 0 && not (IM.member t sn) then (t : nx, IM.insert t (c : path) sn) else (nx, sn))
                                      (next, seen) (dAlpha d)
          in go qs next' seen'

-- Nothing なら同じ言語、そうでなければ反例
dfaEquivalent :: DFA -> DFA -> Maybe String
dfaEquivalent a b = dfaShortestAccepted (prD (productDfa a b OpXor))

-- スタートからたどれない状態を消す（2 つ目 = 新しい番号 → 元の番号）
reachableDfa :: DFA -> (DFA, [Int])
reachableDfa d =
  let go h order idx
        | h >= length order = (order, idx)
        | otherwise =
            let q = at' order h 0
                (order', idx') = foldl' (\(o, ix) c -> let t = dfaNext d q c in
                                            if t >= 0 && not (IM.member t ix) then (o ++ [t], IM.insert t (length o) ix) else (o, ix))
                                        (order, idx) (dAlpha d)
            in go (h + 1) order' idx'
      (ord', idxM) = go 0 [dStart d] (IM.singleton (dStart d) 0)
      row q = M.fromList [ (c, IM.findWithDefault 0 t idxM) | c <- dAlpha d, let t = dfaNext d q c, t >= 0 ]
  in (DFA (length ord') (dAlpha d) (map row ord') 0 (map (\q -> at' (dAcc d) q False) ord'), ord')

data Mini = Mini { mWork :: DFA, mOld :: [Int], mRounds :: [[Int]], mBlockOf :: [Int], mMin :: DFA }

-- 見つけた順に 0, 1, 2, … と番号をつけなおす
renum :: Ord k => [k] -> [Int]
renum keys = reverse (snd (foldl' (\(m, out) k -> case M.lookup k m of
                                     Just v -> (m, v : out)
                                     Nothing -> let v = M.size m in (M.insert k v m, v : out)) (M.empty, []) keys))

-- ムーアの分割の精密化：「区別できない状態」を同じブロックに
minimizeMoore :: DFA -> Mini
minimizeMoore d =
  let (e, old) = reachableDfa (completeDfa d)
      blk0 = renum (map (\v -> if v then 1 else 0 :: Int) (dAcc e))
      nDistinct = S.size . S.fromList
      refine blk rounds =
        let keys = [ (b, [ at' blk (dfaNext e q c) 0 | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
            nb = renum keys
        in if nDistinct nb == nDistinct blk then (blk, reverse rounds) else refine nb (nb : rounds)
      (blk, rounds) = refine blk0 [blk0]
      k = nDistinct blk
      accs = IM.fromList [ (b, at' (dAcc e) q False) | (q, b) <- zip [0 ..] blk ]
      trs = IM.fromListWith M.union [ (b, M.fromList [ (c, at' blk (dfaNext e q c) 0) | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
      mn = DFA k (dAlpha e) [ IM.findWithDefault M.empty b trs | b <- [0 .. k - 1] ] (at' blk (dStart e) 0) [ IM.findWithDefault False b accs | b <- [0 .. k - 1] ]
  in Mini e old rounds blk mn

-- 長さ 0〜maxLen の文字列を全部（短い順）
allStrings :: [Sym] -> Int -> [String]
allStrings alpha maxLen = concat (take (maxLen + 1) (iterate (\layer -> [ s ++ [c] | s <- layer, c <- alpha ]) [""]))

nfaRow :: NFA -> Int -> Sym -> [Int]
nfaRow nfa q c = M.findWithDefault [] c (at' (nTr nfa) q M.empty)

-- ε 遷移だけでたどれる状態を全部足す（小さい順）
epsClosure :: NFA -> [Int] -> [Int]
epsClosure nfa states = S.toAscList (go (S.fromList states) states)
  where go seen [] = seen
        go seen (q : todo) = let new = [ t | t <- nfaRow nfa q epsSym, not (S.member t seen) ]
                             in go (foldl' (flip S.insert) seen new) (new ++ todo)

nfaMove :: NFA -> [Int] -> Sym -> [Int]
nfaMove nfa set c = epsClosure nfa (S.toList (S.fromList (concatMap (\q -> nfaRow nfa q c) set)))

nfaTrace :: NFA -> String -> [[Int]]
nfaTrace nfa = scanl (nfaMove nfa) (epsClosure nfa [nStart nfa])

nfaAccepts :: NFA -> String -> Bool
nfaAccepts nfa s = go (epsClosure nfa [nStart nfa]) s
  where go cur [] = any (\q -> at' (nAcc nfa) q False) cur
        go cur (c : cs) = let nx = nfaMove nfa cur c in not (null nx) && go nx cs

data Subset = Subset { sSets :: [[Int]], sRows :: [(Int, M.Map Sym Int)], sDfa :: DFA }

-- 部分集合構成：DFA の 1 状態 = NFA の状態の集合（たどれる集合だけ、見つけた順に番号）
subsetConstruct :: NFA -> Int -> Subset
subsetConstruct nfa maxStates =
  let first = epsClosure nfa [nStart nfa]
      loop h sets ids n rows
        | h >= n || n > maxStates = (sets, reverse rows)
        | otherwise =
            let cur = IM.findWithDefault [] h sets
                addSym (sets1, ids1, n1, row) c =
                  let t = nfaMove nfa cur c
                  in case M.lookup t ids1 of
                       Just k -> (sets1, ids1, n1, M.insert c k row)
                       Nothing -> (IM.insert n1 t sets1, M.insert t n1 ids1, n1 + 1, M.insert c n1 row)
                (sets', ids', n', row') = foldl' addSym (sets, ids, n, M.empty) (nAlpha nfa)
            in loop (h + 1) sets' ids' n' ((h, row') : rows)
      (setsM, rows) = loop 0 (IM.singleton 0 first) (M.singleton first 0) 1 []
      sets = IM.elems setsM
      dfa = DFA (length sets) (nAlpha nfa) (map snd rows) 0 [ any (\q -> at' (nAcc nfa) q False) st | st <- sets ]
  in Subset sets rows dfa

setName :: [Int] -> String
setName st = "{" ++ joinWith "," (map show st) ++ "}"

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ sep ++ b) xs

-- ---- 状態遷移図を描く道具（円 = 状態、二重丸 = 受理、矢印 = 遷移。曲線は折れ線で描く）
data Pos = Pos { pX :: !Float, pY :: !Float, pLoop :: Maybe Float }   -- 位置と自己ループの向き

-- 辺 = 出る状態・入る状態・ラベル・記号のリスト
data Edge = Edge { eFrom :: !Int, eTo :: !Int, eLabel :: String, eSyms :: [String] }

epsLabel :: String                            -- ε の矢印のラベル（画面は ASCII だけなので "eps"）
epsLabel = "eps"

-- 2 次ベジェ曲線 → 折れ線の点
bezPts :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [(Float, Float)]
bezPts x0 y0 cx cy x1 y1 n =
  [ (u * u * x0 + 2 * u * t * cx + t * t * x1, u * u * y0 + 2 * u * t * cy + t * t * y1) | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t ]

-- 3 次ベジェ曲線 → 折れ線の点（自己ループ用）
cubicPts :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Int -> [(Float, Float)]
cubicPts (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) n =
  [ (a * p0x + b * p1x + c * p2x + d * p3x, a * p0y + b * p1y + c * p2y + d * p3y)
  | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t, let a = u * u * u, let b = 3 * u * u * t, let c = 3 * u * t * t, let d = t * t * t ]

-- 折れ線 + 先の三角（dashed = 点線：1 つおきの区間だけ描く）
arrowLine :: [(Float, Float)] -> RGB -> Float -> Bool -> Picture
arrowLine pts c w dashed = case reverse pts of
  ((x1, y1) : (x0, y0) : _) ->
    let l0 = hyp (x1 - x0) (y1 - y0)
        l = if l0 == 0 then 1 else l0
        ux = (x1 - x0) / l
        uy = (y1 - y0) / l
        s = 7 + w * 1.5
        body = if dashed then Pictures [ lineW w p q | (k, p, q) <- zip3 [0 :: Int ..] pts (drop 1 pts), even k ] else polyline w pts
        tip = Polygon [(x1, y1), (x1 - ux * s - uy * s * 0.5, y1 - uy * s + ux * s * 0.5), (x1 - ux * s + uy * s * 0.5, y1 - uy * s - ux * s * 0.5)]
    in Color (col c) (Pictures [body, tip])
  _ -> Blank

-- 状態ごとの「ラベル → 行き先のリスト」から辺のリスト（同じ向きの辺はラベルをまとめる。p5.js 版の edgeList）
edgesFrom :: [[(String, [Int])]] -> [Edge]
edgesFrom rows =
  let n = length rows
      items = [ ((q, t), lab) | (q, row) <- zip [0 ..] rows, (lab, ts) <- row, t <- ts, t >= 0, t < n ]
      keys = nubOrd (map fst items)
      symsOf k = [ lab | (k', lab) <- items, k' == k ]
      sortSyms = sortOn (\s -> (s == epsLabel, s))
  in [ Edge q t (joinWith "," (sortSyms (symsOf (q, t)))) (symsOf (q, t)) | (q, t) <- keys ]

symLabel :: Sym -> String
symLabel c = if c == epsSym then epsLabel else [c]

dfaEdges :: DFA -> [Edge]
dfaEdges d = edgesFrom [ [ (symLabel c, [t]) | (c, t) <- M.toList row ] | row <- take (dN d) (dTr d ++ repeat M.empty) ]

nfaEdges :: NFA -> [Edge]
nfaEdges m = edgesFrom [ [ (symLabel c, ts) | (c, ts) <- M.toList row ] | row <- take (nN m) (nTr m ++ repeat M.empty) ]

-- 1 本の辺の折れ線の点とラベルの位置
edgeGeometry :: Edge -> Pos -> Pos -> Float -> Bool -> ([(Float, Float)], (Float, Float))
edgeGeometry e a b r both
  | eFrom e == eTo e =                         -- 自己ループ：中心から外向き（または上）に輪
      let ang = maybe (-pi / 2) id (pLoop a)
          ux = cos ang
          uy = sin ang
          px = -uy
          py = ux
          s0 = (pX a + (ux * 0.6 + px * 0.8) * r, pY a + (uy * 0.6 + py * 0.8) * r)
          s1 = (pX a + (ux * 0.6 - px * 0.8) * r, pY a + (uy * 0.6 - py * 0.8) * r)
          pts = cubicPts s0 (fst s0 + (ux * 2.2 + px * 0.9) * r, snd s0 + (uy * 2.2 + py * 0.9) * r)
                            (fst s1 + (ux * 2.2 - px * 0.9) * r, snd s1 + (uy * 2.2 - py * 0.9) * r) s1 18
      in (pts, (pX a + ux * 2.35 * r, pY a + uy * 2.35 * r))
  | otherwise =
      let dx = pX b - pX a
          dy = pY b - pY a
          l0 = hyp dx dy
          l = if l0 == 0 then 1 else l0
          ux = dx / l
          uy = dy / l
          bend = if both then 0.22 * l else 0
          mx = (pX a + pX b) / 2 - uy * bend
          my = (pY a + pY b) / 2 + ux * bend
          d0x = mx - pX a
          d0y = my - pY a
          l0' = nz (hyp d0x d0y)
          d1x = mx - pX b
          d1y = my - pY b
          l1 = nz (hyp d1x d1y)
          pts = bezPts (pX a + (d0x / l0') * r) (pY a + (d0y / l0') * r) mx my (pX b + (d1x / l1) * (r + 2)) (pY b + (d1y / l1) * (r + 2)) 16
          (midX, midY) = at' pts 8 (mx, my)
      in (pts, (midX - uy * 13, midY + ux * 13))
  where nz v = if v == 0 then 1 else v

-- 図の描き方の選択（p5.js 版の opt）
data MOpt = MOpt
  { oR :: Float                               -- 円の半径
  , oNames :: Maybe [String]                  -- 状態の名前（Nothing なら q0, q1, …）
  , oHi :: IM.IntMap RGB                      -- 色をつける状態
  , oEdgeHi :: Edge -> Maybe RGB              -- 色をつける辺
  , oStart :: Bool                            -- 開始の矢印を描くか
  , oLabels :: Bool                           -- 辺のラベルを描くか
  , oLabelSize :: Float
  , oNameSize :: Maybe Float
  , oEdgeCol :: RGB
  , oLabelCol :: RGB
  , oStartCol :: RGB
  , oStartAng :: Maybe Float }

defOpt :: MOpt
defOpt = MOpt 22 Nothing IM.empty (const Nothing) True True 14 Nothing (120, 160, 140) (200, 225, 210) ink Nothing

-- 状態の数 n・開始・受理・辺・位置から図を描く
drawMachine :: MOpt -> Int -> Int -> [Bool] -> [Edge] -> [Pos] -> Picture
drawMachine opt n start acc edges pos = Pictures (edgePics ++ [startPic] ++ statePics)
  where
    r = oR opt
    has = S.fromList [ (eFrom e, eTo e) | e <- edges ]
    posOf q = if q >= 0 then at' (map Just pos) q Nothing else Nothing
    edgePics =
      [ Pictures [ arrowLine pts (maybe (oEdgeCol opt) id hc) (if hc == Nothing then 1.8 else 3.5) (eLabel e == epsLabel && hc == Nothing)
                 , if oLabels opt || hc /= Nothing then sayM (eLabel e) lx ly (max 14 (oLabelSize opt)) (col (maybe (oLabelCol opt) id hc)) else Blank ]
      | e <- edges, Just a <- [posOf (eFrom e)], Just b <- [posOf (eTo e)]
      , let (pts, (lx, ly)) = edgeGeometry e a b r (eFrom e /= eTo e && S.member (eTo e, eFrom e) has)
      , let hc = oEdgeHi opt e ]
    startPic = case posOf start of
      Just p | oStart opt && start < n ->
        let sa = case oStartAng opt of
              Just v -> v
              Nothing -> case pLoop p of
                Just lp | cos lp < -0.5 -> -2.2
                _ -> pi
            ux = cos sa
            uy = sin sa
        in arrowLine [(pX p + ux * (r + 34), pY p + uy * (r + 34)), (pX p + ux * (r + 2), pY p + uy * (r + 2))] (oStartCol opt) 2 False
      _ -> Blank
    statePics =
      [ Pictures [ Color (col (maybe panel (scaleRGB 0.35) hc)) (circleAt (pX p) (pY p) (2 * r))
                 , Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r) sw)
                 , if at' acc q False then Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r - 9) sw) else Blank
                 , sayM nm (pX p) (pY p + 1) (max 14 (maybe (min 16 (r * 0.75)) id (oNameSize opt))) (col (maybe ink (const (255, 255, 255)) hc)) ]
      | q <- [0 .. n - 1], Just p <- [posOf q]
      , let hc = IM.lookup q (oHi opt)
      , let sw = if hc == Nothing then 2 else 3.5
      , let nm = maybe ("q" ++ show q) (\ns -> at' ns q "") (oNames opt) ]

drawDfa :: MOpt -> DFA -> [Pos] -> Picture
drawDfa opt d = drawMachine opt (dN d) (dStart d) (dAcc d) (dfaEdges d)

drawNfa :: MOpt -> NFA -> [Pos] -> Picture
drawNfa opt m = drawMachine opt (nN m) (nStart m) (nAcc m) (nfaEdges m)

-- 円の上に並べる（自己ループは外向き）
circleLayout :: Int -> Float -> Float -> Float -> Float -> [Pos]
circleLayout n cx cy rad a0 =
  [ Pos (cx + rad * cos a) (cy + rad * sin a) (Just a) | i <- [0 .. n - 1], let a = a0 + 2 * pi * i2f i / i2f (max 1 n) ]

-- スタートからの距離（幅優先）で左 → 右に並べる
layerLayout :: Int -> Int -> [Edge] -> Float -> Float -> Float -> Float -> [Pos]
layerLayout n start edges x y w h
  | n <= 0 = []
  | otherwise =
      let st = clampI start 0 (n - 1)
          bfs i order depth
            | i >= length order = (order, depth)
            | otherwise =
                let q = at' order i 0
                    dq = IM.findWithDefault 0 q depth
                    (order', depth') = foldl' (\(o, dp) e -> if eFrom e == q && not (IM.member (eTo e) dp) && eTo e >= 0 && eTo e < n
                                                               then (o ++ [eTo e], IM.insert (eTo e) (dq + 1) dp) else (o, dp)) (order, depth) edges
                in bfs (i + 1) order' depth'
          (order0, depth0) = bfs 0 [st] (IM.singleton st 0)
          maxD0 = maxOr 0 (IM.elems depth0)
          rest = [ q | q <- [0 .. n - 1], not (IM.member q depth0) ]
          depth = foldl' (\dp q -> IM.insert q (maxD0 + 1) dp) depth0 rest
          order = order0 ++ rest
          maxD = maxOr 0 (IM.elems depth)
          cols = IM.fromListWith (flip (++)) [ (IM.findWithDefault 0 q depth, [q]) | q <- order ]
          posM = IM.fromList [ (q, Pos (x + (if maxD > 0 then i2f d / i2f maxD * w else w / 2)) (y + i2f (k + 1) / i2f (length cs + 1) * h) (Just (-pi / 2)))
                             | (d, cs) <- IM.toList cols, (k, q) <- zip [0 :: Int ..] cs ]
      in [ IM.findWithDefault (Pos x y Nothing) q posM | q <- [0 .. n - 1] ]

-- (x, y) の上にある状態（後ろから調べる。なければ −1）
hitState :: [Pos] -> Float -> Float -> Float -> Int
hitState pos x y r = headOr (-1) [ q | (q, p) <- reverse (zip [0 ..] pos), hyp (pX p - x) (pY p - y) < r ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
