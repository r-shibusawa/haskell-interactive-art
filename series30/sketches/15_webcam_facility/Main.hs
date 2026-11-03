-- ============================================
-- 15 カメラ映像で施設配置（おまけ）
-- カメラに映った明るい点（スマホのライト、白い紙）や選んだ色の場所を「お客さんがいる所」とみなして、拠点をリアルタイムに置き直す
--   拠点は「自分に一番近いお客さんたち」の距離の合計が最小になる位置（ワイスフェルト法）へ少しずつ動く → 11 の p メディアンの連続版
-- ↑↓ = 拠点の数 / ←→ = しきい値 / B = 明るい所モード / 映像をクリック = その色を追う / V = 映像の表示
-- （カメラがないときはデモ映像の明るい所と、マウスの通った所がお客さん）
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq

vw, vh, stepPx :: Int
vw = 160                               -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 120
stepPx = 4                             -- ★ 何画素おきに調べるか

-- | お客さん（0〜1 の座標と重み）
data Demand = Demand { dX, dY, dW :: !Float }

data Mode = Bright | ColorMode deriving Eq

data World = World
  { wCam :: !Camera
  , wFrame :: !B.ByteString            -- 最新のカメラ画像
  , wLive :: !Bool                     -- 本物のカメラか
  , wFac :: ![(Float, Float)]          -- 拠点（0〜1 の座標）
  , wThr, wColorThr :: !Int            -- ★ 明るさ・色の近さのしきい値
  , wMode :: !Mode
  , wPick :: !(Int, Int, Int)          -- 追う色
  , wShowVideo :: !Bool
  , wDemand :: ![Demand]
  , wGroups :: ![[Demand]]             -- 拠点ごとの担当
  , wTrail :: !(Seq.Seq Demand)        -- マウスの通った所（300 点まで）
  , wUsingMouse :: !Bool
  , wPrev :: !Point                    -- ひとつ前のマウスの位置
  }

main :: IO ()
main = runSketch (sketch "30-15 Webcam Facility Location" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> do
      cam <- startCamera vw vh
      fac <- replicateM 3 randomFac
      pure (World cam B.empty False fac 200 70 Bright (255, 0, 0) True [] [] Seq.empty True (0, 0))
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

randomFac :: IO (Float, Float)
randomFac = (,) <$> randomRIO (0.2, 0.8) <*> randomRIO (0.2, 0.8)

-- 映像を置く場所（左上 x, y、幅 w、高さ h）
area :: Env -> (Float, Float, Float, Float)
area env =
  let h0 = envH env - 150
      w0 = h0 * 4 / 3
      (w, h) = if w0 > envW env * 0.68 then (envW env * 0.68, envW env * 0.68 * 3 / 4) else (w0, h0)
  in (30, 110, w, h)

inArea :: Env -> Bool
inArea env =
  let (ax, ay, aw, ah) = area env
      (mx, my) = mouseP env
  in mx > ax && mx < ax + aw && my > ay && my < ay + ah

-- (x, y) の画素の色
pixelAt :: B.ByteString -> Int -> Int -> (Int, Int, Int)
pixelAt f x y =
  let i = (x + y * vw) * 3
      g k = fromIntegral (B.index f (i + k))
  in (g 0, g 1, g 2)

frameOk :: B.ByteString -> Bool
frameOk f = B.length f >= vw * vh * 3

-- ★ カメラ画像からお客さんの点を集める
findDemand :: World -> [Demand]
findDemand World{..}
  | not (frameOk wFrame) = []
  | otherwise =
      [ d
      | y <- [0, stepPx .. vh - 1], x <- [0, stepPx .. vw - 1]
      , let (r, g, b) = pixelAt wFrame x y
      , let fx = fromIntegral x / fromIntegral vw
      , let fy = fromIntegral y / fromIntegral vh
      , d <- case wMode of
          Bright ->
            let br = fromIntegral (r + g + b) / 3 :: Float
                th = fromIntegral wThr
            in [Demand fx fy ((br - th) / (255 - th + 1) + 0.2) | br > th]
          ColorMode ->
            let (pr, pg, pb) = wPick
            in [Demand fx fy 1 | abs (r - pr) + abs (g - pg) + abs (b - pb) < wColorThr]
      ]

sq :: Float -> Float
sq v = v * v

-- ★ ワイスフェルト法の 1 回：各拠点を「担当するお客さんの重みつき・距離で割った平均」へ動かす
updateFacilities :: [Demand] -> [(Float, Float)] -> ([(Float, Float)], [[Demand]])
updateFacilities demand fac0 =
  let n = length fac0
      nearestK p fs = fst (foldl' (\(b, bd) (k, (fx, fy)) ->
        let dd = sq (dX p - fx) + sq (dY p - fy) in if dd < bd then (k, dd) else (b, bd)) (0, 1 / 0) (zip [0 :: Int ..] fs))
      groups = [ [p | (p, k') <- tagged, k' == k] | k <- [0 .. n - 1] ]
      tagged = [ (p, nearestK p fac0) | p <- demand ]
      moveOne fs (k, g)
        | null g =                                      -- 担当がいない拠点は、一番遠いお客さんの所へワープ
            let minD p = minimum (1 / 0 : [sq (dX p - fx) + sq (dY p - fy) | (fx, fy) <- fs])
                pickFar (b, bd) p = let m = minD p in if m > bd then (Just p, m) else (b, bd)
            in case fst (foldl' pickFar (Nothing, -1) demand) of
                 Just far -> setAt k (dX far, dY far) fs
                 Nothing -> fs
        | otherwise =
            let (fx, fy) = case drop k fs of
                  v : _ -> v
                  [] -> (0.5, 0.5)
                add (!sx, !sy, !sw) p =
                  let d = max 0.005 (sqrt (sq (dX p - fx) + sq (dY p - fy)))
                      wg = dW p / d
                  in (sx + wg * dX p, sy + wg * dY p, sw + wg)
                (tx, ty, tw) = foldl' add (0, 0, 0) g
            in setAt k (lerpF fx (tx / tw) 0.3, lerpF fy (ty / tw) 0.3) fs
  in (foldl' moveOne fac0 (zip [0 ..] groups), groups)

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 1 フレーム分を進める
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  frame <- cameraFrame (wCam w0)
  live <- camLive (wCam w0)
  jx <- randomRIO (-0.01, 0.01)
  jy <- randomRIO (-0.01, 0.01)
  let w = w0 { wFrame = frame, wLive = live }
      pts = findDemand w
      camOk = live && length pts >= 3
      (ax, ay, aw, ah) = area env
      m@(mx, my) = mouseP env
      moved = m /= wPrev w
      add = not camOk && inArea env && moved
      tr0 = if add then wTrail w Seq.|> Demand ((mx - ax) / aw + jx) ((my - ay) / ah + jy) 1 else wTrail w
      tr1 = if Seq.length tr0 > 300 then Seq.drop 1 tr0 else tr0
      -- 本物のカメラの点が 3 つ以上なら映像だけ。カメラがないときはデモ映像の点とマウスの跡の両方を使う
      demand | camOk = take 600 pts
             | live = toList tr1
             | otherwise = take 600 pts ++ toList tr1
      (fac, groups) = if null demand then (wFac w, map (const []) (wFac w)) else updateFacilities demand (wFac w)
  pure w { wDemand = demand, wGroups = groups, wFac = fac, wTrail = tr1, wUsingMouse = not camOk, wPrev = m }

facHue :: Int -> Float
facHue k = wrapF 360 (fromIntegral (k * 67 + 30))

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures [video, links, sites, side, swatch, hud]
  where
    (ax, ay, aw, ah) = area env
    sxA v = ax + v * aw
    syA v = ay + v * ah

    -- 映像（p5.js の tint の代わりに、背景色と混ぜて薄くする）
    k = aw / fromIntegral vw
    mix :: Int -> Int -> Int
    mix v bg = (v * 45 + bg * 55) `div` 100
    video = Pictures
      [ if wShowVideo && frameOk wFrame
          then Translate (ax + aw / 2) (negate (ay + ah / 2)) $
                 Scale 1 (ah / aw * fromIntegral vw / fromIntegral vh) $
                 pixelImage vw vh k (\x y ->
                   let (r, g, b) = pixelAt wFrame x y
                   in (fromIntegral (mix r 21), fromIntegral (mix g 26), fromIntegral (mix b 40)))
          else box (hsb 225 25 16) ax ay aw ah
      , boxL 1 (hsba 0 0 100 0.3) ax ay aw ah
      ]

    -- お客さんと担当の線
    pairs = zip3 [0 ..] wFac wGroups
    links = Pictures $ concat
      [ [ seg 1 (hsba (facHue i) 60 100 0.25) (sxA (dX p), syA (dY p)) (sxA fx, syA fy)
        , dot (hsba (facHue i) 50 100 0.8) (sxA (dX p)) (syA (dY p)) (3 + dW p * 4) ]
      | (i, (fx, fy), g) <- pairs, p <- g ]
    total = sum [ dW p * sqrt (sq (dX p - fx) + sq (dY p - fy)) | (_, (fx, fy), g) <- pairs, p <- g ]
    sites = Pictures $ concat
      [ [ box (hsb (facHue i) 80 100) (sxA fx - 12) (syA fy - 12) 24 24
        , boxL 2 white (sxA fx - 12) (syA fy - 12) 24 24
        , txtC (sxA fx) (syA fy) 14 (gr 10) (show (i + 1)) ]
      | (i, (fx, fy)) <- zip [0 :: Int ..] wFac ]

    -- 右：説明
    rx = ax + aw + 40
    title | wUsingMouse && wLive = "Mouse mode"
          | wUsingMouse = "Demo + mouse mode"
          | wMode == Bright = "Bright mode"
          | otherwise = "Color mode"
    rows =
      [ "Customers " ++ show (length wDemand) ++ "  Sites " ++ show (length wFac)
      , "Total distance " ++ nf 2 total
      , if wMode == Bright then "Brightness threshold " ++ show wThr else "Color threshold " ++ show wColorThr
      , if wLive then "Camera OK" else "Demo video (no camera)"
      , ""
      , if wUsingMouse then "Move the mouse over the video:" else "Move a light or white paper:"
      , if wUsingMouse then "its path becomes customers" else "the sites chase and re-place"
      , ""
      , "A site with no customers warps"
      , "to the farthest customer"
      , "(escapes local optima)"
      ]
    side = Pictures $
      txt rx 130 18 white title
      : [ txt rx (166 + i * 24) 14 (gr 80) s | (i, s) <- zip [0 ..] rows ]
    swatch
      | wMode == ColorMode =
          let (r, g, b) = wPick
          in Pictures [box (rgb255 r g b) rx 450 50 30, txt (rx + 60) 470 14 (gr 80) "Tracked color"]
      | otherwise = Blank

    hud = Pictures
      [ txt 20 40 16 white ("U/D sites " ++ show (length wFac) ++ "   L/R threshold   B: bright   Click video: track that color   V: video " ++ (if wShowVideo then "ON" else "off"))
      , txt 20 68 14 (gr 60) "Squares = sites. They keep moving toward nearby customers in the video (no camera: demo video + mouse)."
      ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | inArea env && frameOk (wFrame w) ->
        let (ax, ay, aw, ah) = area env
            (mx, my) = mouseP env
            ix = max 0 (min (vw - 1) (floor ((mx - ax) / aw * fromIntegral vw)))
            iy = max 0 (min (vh - 1) (floor ((my - ay) / ah * fromIntegral vh)))
        in pure w { wPick = pixelAt (wFrame w) ix iy, wMode = ColorMode }
  EventKey (SpecialKey KeyUp) Down _ _ | length (wFac w) < 8 -> do
    f <- randomFac
    pure w { wFac = wFac w ++ [f] }
  EventKey (SpecialKey KeyDown) Down _ _ | length (wFac w) > 1 -> pure w { wFac = init (wFac w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure $
    if wMode w == Bright then w { wThr = min 250 (wThr w + 5) } else w { wColorThr = min 250 (wColorThr w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure $
    if wMode w == Bright then w { wThr = max 50 (wThr w - 5) } else w { wColorThr = max 10 (wColorThr w - 10) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'b' -> pure w { wMode = Bright }
    'v' -> pure w { wShowVideo = not (wShowVideo w) }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ライトを 2 つ（スマホ 2 台）映して、拠点 2 つがそれぞれを担当するか見よう
-- 2. 拠点の動きを Weiszfeld（距離の合計）から「平均」（k-means：距離の 2 乗の合計）に変えると、置き場所はどう変わる？
-- 3. 最大被覆（11 の 2）のように「半径 R 以内に入る点の数」を最大にする動き方を考えよう
-- 4. 拠点が動いたときに音を鳴らす（14 の道具をコピー）と、インスタレーション作品になる


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：(x, y) が文字の中心
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC x (negate y)

-- 塗りつぶした長方形：左上 (x, y)、幅 w、高さ h
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (negate (y + h / 2)) w h)

-- 枠だけの長方形（線の太さ t）
boxL :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
boxL t c x y w h = loopPath t c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 閉じた折れ線（太さ t）
loopPath :: Float -> Color -> [Point] -> Picture
loopPath t c ps = path t c (ps ++ take 1 ps)

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

