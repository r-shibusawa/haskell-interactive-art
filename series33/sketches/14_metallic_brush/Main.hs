-- ============================================
-- 14 貴金属比ブラシ ─ 自由制作のベース
-- ドラッグすると、貴金属比の形のスタンプが押される。動かす速さで大きさが変わり、音も鳴る
--   1 = 正方形の渦（01）/ 2 = 対数螺旋（04）/ 3 = ひまわり（07）/ 4 = 正多角形の星（06）
-- ドラッグ = 描く / 1〜4 = ブラシ / ←→ = n / P = 色の組み合わせ / M = 音 ON/OFF / S = 保存 / C = 消す
-- ============================================
-- gloss は毎フレーム描きなおすので、スタンプを絵のリストとして覚えておく。
-- 図形の数が maxPrims を超えると、古いスタンプから消える。
module Main (main) where

import Art
import Data.Foldable (toList)
import qualified Data.Sequence as Q
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import Numeric (showFFloat)

-- 色の組み合わせ（HSB の色相）
data Palette = Palette
  { palName :: String
  , palHues :: [Float]
  , palSat  :: !Float
  , palBg   :: !(Float, Float, Float)
  }

palettes :: [Palette]              -- ★ 色の組み合わせ
palettes =
  [ Palette "gold & silver" [45, 40, 210, 0] 55 (230, 30, 8)
  , Palette "sunset" [10, 30, 330, 280] 75 (260, 40, 10)
  , Palette "sea" [180, 200, 220, 160] 70 (220, 60, 8)
  , Palette "washi" [0, 30, 120, 210] 35 (40, 10, 92)
  ]

paletteAt :: Int -> Palette
paletteAt i = case drop (i `mod` length palettes) palettes of
  (p : _) -> p
  [] -> Palette "" [0] 50 (0, 0, 0)

-- 覚えておくスタンプ：図形の数と絵
data Stamp = Stamp !Int Picture

data World = World
  { wN      :: !Int     -- ★ 貴金属比の n
  , wBrush  :: !Int
  , wPal    :: !Int
  , wSound  :: !Bool
  , wArt    :: !(Seq Stamp)
  , wPrims  :: !Int     -- 覚えている図形の数
  , wLast   :: !(Maybe Point)   -- 前回スタンプを押した位置
  , wPrev   :: !Point   -- 1 フレーム前のマウス（pmouseX, pmouseY）
  , wCount  :: !Int     -- スタンプの個数
  }

maxPrims :: Int
maxPrims = 6800         -- ★ 覚えておく図形の数

initialWorld :: World
initialWorld = World 1 1 0 True Q.empty 0 Nothing (0, 0) 0

main :: IO ()
main = runSketch (sketch "33-14 Metallic Brush" initialWorld)
  { skBack  = black
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 描く
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  let m@(mx, my) = envMouse env
      topY = envH env / 2 - my             -- p5.js の mouseY
      sp = dist2 m (wPrev w)
      far = case wLast w of
        Nothing -> True
        Just l -> dist2 m l > 18 + sp
  w1 <- if envDown env && topY > 80 && far
          then do
            let s = clampF 20 220 (20 + sp * 3)   -- ★ 速いほど大きい
            w2 <- stamp mx my s w
            pure w2 { wLast = Just m }
          else pure w
  pure w1 { wPrev = m }

-- スタンプを 1 つ押す
stamp :: Float -> Float -> Float -> World -> IO World
stamp x y s w = do
  ang <- randomRIO (0, 360)
  let pal = paletteAt (wPal w)
      phi = metallic (fromIntegral (wN w))
      hues = palHues pal
      hue = case drop (wCount w `mod` max 1 (length hues)) hues of
        (h : _) -> h
        [] -> 0
      (cost, shape) = stampShape (wBrush w) (wN w) phi (palSat pal) hue s
      -- y を下向きにして描いた形を、回して置く
      pic = Translate x y (Rotate ang (Scale 1 (-1) shape))
      count = wCount w + 1
      (art, prims) = trim (wArt w |> Stamp cost pic) (wPrims w + cost)
  if wSound w
    then do
      let r = foldOctave (phi ^ (count `mod` 7))
          f = 220 * r * (if wBrush w == 3 then 2 else 1)
      playTone f 0.12 0.4 (if wBrush w == 2 then Triangle else Sine)
    else pure ()
  pure w { wArt = art, wPrims = prims, wCount = count }

-- 古いスタンプから消して、図形の数を上限におさめる
trim :: Seq Stamp -> Int -> (Seq Stamp, Int)
trim art prims
  | prims <= maxPrims = (art, prims)
  | otherwise = case viewl art of
      Stamp c _ :< rest -> trim rest (prims - c)
      EmptyL -> (art, 0)

-- ブラシごとの形（図形の数, 絵）
stampShape :: Int -> Int -> Float -> Float -> Float -> Float -> (Int, Picture)
stampShape brush n phi sat hue s = case brush of
  1 -> let ps = squares in (length ps, Pictures ps)
  2 -> -- 対数螺旋：90° で φ 倍
       let pts = [ (r * cos t, r * sin t)
                 | i <- [0 .. 150 :: Int]
                 , let t = -12 + fromIntegral i * 0.08
                 , let r = s * 0.5 * phi ** (t * 2 / pi) ]
       in (2, Color (hsba hue sat 100 0.8) (wideLine pts))
  3 -> -- ひまわり：360°/φ ずつ回す
       let m = floor (s * 0.8) :: Int
           th = tau / phi
           c = s * 0.5 / sqrt (fromIntegral m)
       in ( m
          , Pictures
              [ Color (hsba (wrapF 360 (hue + fi * 0.6)) sat 100 0.7)
                  (at (c * sqrt fi * cos (fi * th), c * sqrt fi * sin (fi * th)) (disc (c * 0.55)))
              | i <- [1 .. m], let fi = fromIntegral i :: Float ] )
  _ -> -- 正多角形の星：n=1 → 五角形の星（対角線/辺 = φ）、n=2 → 八角形の星（1+√2）、n≥3 → 十二角形など（06）
       let m = pick [5, 8, 12, 10, 12, 14]
           kStep = pick [2, 3, 5, 3, 5, 5]
           corner i = let a = tau * fromIntegral (i * kStep) / fromIntegral m
                      in (s * 0.5 * cos a, s * 0.5 * sin a)
       in (2, Color (hsba hue sat 100 0.8) (wideLine (map corner [0 .. m])))
  where
    pick :: [Int] -> Int
    pick xs = case drop (n - 1) xs of
      (x : _) -> x
      [] -> 5
    -- 正方形の渦：01 と同じ分割を小さく
    squares = goSq 0 (-s / 2) (-(s / phi) / 2) s (s / phi)
    goSq :: Int -> Float -> Float -> Float -> Float -> [Picture]
    goSq d ox oy w h
      | d >= 8 || min w h <= 1.5 = []
      | otherwise =
          let sq = min w h
              dir = d `mod` 4
              k = fromIntegral n * sq
              one j =
                let fj = fromIntegral j :: Float
                    (rx, ry) = case dir of
                      0 -> (ox + fj * sq, oy)
                      1 -> (ox, oy + fj * sq)
                      2 -> (ox + w - (fj + 1) * sq, oy)
                      _ -> (ox, oy + h - (fj + 1) * sq)
                    corners = [(rx, ry), (rx + sq, ry), (rx + sq, ry + sq), (rx, ry + sq)]
                in [ Color (hsba (wrapF 360 (hue + fromIntegral d * 25)) sat (90 - fromIntegral d * 6) 0.45) (Polygon corners)
                   , Color (hsba hue sat 100 0.7) (Line (corners ++ take 1 corners)) ]
              rest = case dir of
                0 -> goSq (d + 1) (ox + k) oy (w - k) h
                1 -> goSq (d + 1) ox (oy + k) w (h - k)
                2 -> goSq (d + 1) ox oy (w - k) h
                _ -> goSq (d + 1) ox oy w (h - k)
          in concatMap one [0 .. n - 1] ++ rest

-- 少し太く見える折れ線（線を 2 本ずらして重ねる。図形の数を節約できる）
wideLine :: [Point] -> Picture
wideLine ps = Pictures [Line ps, Translate 0.8 0.8 (Line ps)]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color (hsb bh bs bb) (rect 0 0 (envW env) (envH env))
  , Pictures [ p | Stamp _ p <- toList (wArt w) ]
  -- カーソルの見本
  , Color (hsba 0 0 100 0.4) (at (envMouse env) (Circle 15))
  -- 上の帯
  , Color (hsba 0 0 (if isLight then 100 else 0) 0.6) (rect 0 (top - 40) (envW env) 80)
  , label left (top - 26) 15 ink ("Brush " ++ show (wBrush w) ++ ": " ++ brushName (wBrush w) ++ " (1-4)   n = " ++ show (wN w) ++ " phi = " ++ nf 3 (metallic (fromIntegral (wN w))) ++ " (L/R)   P: " ++ palName pal)
  , label left (top - 50) 14 sub ("M sound " ++ (if wSound w then "ON" else "off") ++ "   S save   C clear   Stamps " ++ show (wCount w) ++ " (oldest fade out when too many)")
  , label left (top - 72) 14 sub "Pitch rises by phi per stamp, folded into one octave."
  ]
  where
    pal = paletteAt (wPal w)
    (bh, bs, bb) = palBg pal
    isLight = bb > 50
    ink = hsb 0 0 (if isLight then 10 else 100)
    sub = hsb 0 0 (if isLight then 30 else 65)
    top = envH env / 2
    left = 20 - envW env / 2

brushName :: Int -> String
brushName b = case b of
  1 -> "square swirl"
  2 -> "log spiral"
  3 -> "sunflower"
  _ -> "polygon star"

clearArt :: World -> World
clearArt w = w { wArt = Q.empty, wPrims = 0, wCount = 0 }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wLast = Nothing }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wN = min (wN w + 1) 6 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure w { wN = max (wN w - 1) 1 }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure w { wBrush = fromEnum c - fromEnum '0' }
    | c == 'p' || c == 'P' -> pure (clearArt w { wPal = (wPal w + 1) `mod` length palettes })
    | c == 'm' || c == 'M' -> pure w { wSound = not (wSound w) }
    | c == 'c' || c == 'C' -> pure (clearArt w)
    | c == 's' || c == 'S' -> saveSvg "metallic_brush" env (drawWorld env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ゆっくり・速くドラッグして大きさのちがうスタンプを重ね、1 枚の絵にしよう
-- 2. ブラシ 3 の角度 th を tau / phi から少しずらすとどうなる？（07 を思い出そう）
-- 3. palettes に自分の色の組み合わせを足そう
-- 4. 5 番目のブラシ（例：10 のフィボナッチ語の帯、11 のタイル 1 枚）を作ってみよう

-- ---------- 道具 ----------
metallic :: Float -> Float
metallic k = (k + sqrt (k * k + 4)) / 2

foldOctave :: Float -> Float
foldOctave r = if r >= 2 then foldOctave (r / 2) else r

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
