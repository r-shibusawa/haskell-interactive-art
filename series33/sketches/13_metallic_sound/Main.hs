-- ============================================
-- 13 貴金属比の音 ─ 黄金比の音階とフィボナッチのリズム
-- 上：周波数を φn 倍ずつ重ね、1 オクターブ（2 倍）の中に折りたたんだ音階。下の段はふつうの純正律（簡単な分数）
-- 下：10 のフィボナッチ語 ABAABABA… をリズムにして鳴らす（A = 長い・低い、B = 短い・高い）
-- 鍵盤をクリック = 鳴らす / スペース = リズム再生 / ←→ = n / ↑↓ = テンポ / D = 長さの比 φ:1 ⇔ 等間隔
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wN         :: !Int      -- ★ 貴金属比の n
  , wBpm       :: !Float    -- ★ テンポ
  , wPlaying   :: !Bool
  , wEqual     :: !Bool     -- D：A と B を同じ長さにする
  , wWord      :: !(U.Vector Char)
  , wPos       :: !Int      -- いま鳴らしている文字の位置
  , wAIdx      :: !Int      -- A の音が音階の何番目か
  , wNextTime  :: !Float    -- 次の音を鳴らす時刻
  , wFlash     :: !(U.Vector Float)   -- 鍵盤の光（16 個）
  , wGold      :: ![Float]  -- 上段の音階（8 音）
  }

baseHz :: Float
baseHz = 220                -- ★ いちばん低い音（Hz）

just :: [(Int, Int)]
just = [(1, 1), (9, 8), (5, 4), (4, 3), (3, 2), (5, 3), (15, 8), (2, 1)]

initialWorld :: World
initialWorld = rebuild (World 1 150 False False U.empty 0 0 0 (U.replicate 16 0) [])

main :: IO ()
main = runSketch (sketch "33-13 Metallic Sound" initialWorld)
  { skBack  = hsb 275 30 10
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

rebuild :: World -> World
rebuild w =
  let phi = metallic (fromIntegral (wN w))
      -- φ^k を [1, 2) に折りたたむ（7 音）
      gold = sort (map (\k -> foldOctave (phi ^ k)) [0 .. 6 :: Int]) ++ [2]
      -- 置換で語をつくる
      rule = replicate (wN w) 'A' ++ "B"
      subst = concatMap (\ch -> if ch == 'A' then rule else "A")
      long = until ((>= 200) . length) subst "A"
  in w { wGold = gold, wWord = U.fromList (take 200 long), wPos = 0 }

-- 鍵盤の位置（段, 番号, x, y, w, h）
keyRects :: Env -> [(Int, Int, Float, Float, Float, Float)]
keyRects env =
  let kw = min 140 ((envW env - 80) / 8)
  in [ (row, i, 40 + fromIntegral i * kw, 120 + fromIntegral row * 150, kw - 8, 120)
     | row <- [0, 1], i <- [0 .. 7] ]

-- 鍵盤の音の比
ratioOf :: World -> Int -> Int -> Float
ratioOf w row i
  | row == 0 = pick (wGold w)
  | otherwise = pick [ fromIntegral a / fromIntegral b | (a, b) <- just ]
  where pick xs = case drop i xs of
          (x : _) -> x
          [] -> 1

durOf :: World -> Char -> Float
durOf w ch
  | wEqual w = 1
  | ch == 'A' = metallic (fromIntegral (wN w))
  | otherwise = 1

stepWorld :: Env -> World -> IO World
stepWorld env w0 = do
  let w = w0 { wFlash = U.map (* 0.92) (wFlash w0) }
  if wPlaying w then schedule (envTime env) w else pure w

-- 時刻が来た音を鳴らす
schedule :: Float -> World -> IO World
schedule now w0 = go w0 { wNextTime = if wNextTime w0 < now - 0.2 then now else wNextTime w0 }
  where
    go w
      | wNextTime w > now || U.null (wWord w) = pure w
      | otherwise = do
          let ch = wWord w U.! wPos w
              phi = metallic (fromIntegral (wN w))
              unit = 60 / wBpm w / (if wEqual w then 1 else phi * 0.6)
              scaleAt i = case drop (i `mod` 7) (wGold w) of
                (x : _) -> x
                [] -> 1
          if ch == 'A'
            then do
              tone (baseHz * scaleAt (wAIdx w)) 0.22 (unit * durOf w ch * 0.9) Triangle
              tone 70 0.3 0.2 Sine              -- 低い「ドン」
            else tone (baseHz * 4 * scaleAt (wAIdx w + 3)) 0.12 (unit * 0.5) Sine
          go w { wNextTime = wNextTime w + unit * durOf w ch
               , wPos = (wPos w + 1) `mod` U.length (wWord w)
               , wAIdx = if ch == 'A' then wAIdx w + 1 else wAIdx w }

tone :: Float -> Float -> Float -> Wave -> IO ()
tone f v d = playTone (clampF 20 8000 f) (clampF 0.001 0.5 v) (max 0.05 d)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (hud ++ concatMap keyPic (keyRects env) ++ info ++ rhythm)
  where
    n = wN w
    phi = metallic (fromIntegral n)
    hud =
      [ txt env 20 28 15 white ("Click a key   Space rhythm " ++ (if wPlaying w then "playing" else "stopped") ++ "   n = " ++ show n ++ " (L/R)   tempo " ++ show (round (wBpm w) :: Int) ++ " (Up/Dn)   D: " ++ (if wEqual w then "even beats" else "A:B = phi:1"))
      , txt env 20 58 14 (gray 0.75) ("Top: steps of phi" ++ show n ++ " folded into one octave. Bottom: just intonation (simple fractions, no beats).")
      ]
    keyPic (row, i, x, y, kw, kh) =
      let r = ratioOf w row i
          lit = wFlash w U.! (row * 8 + i)
      in [ Color (hsb (if row == 0 then 45 else 200) (60 - lit * 40) (50 + lit * 50)) (box env x y kw kh)
         , txt env (x + 8) (y + 24) 14 white (nf 3 r)
         , txt env (x + 8) (y + 46) 14 (gray 0.85) (show (round (baseHz * r) :: Int) ++ " Hz")
         , txt env (x + 8) (y + 68) 14 (gray 0.85) (show (round (1200 * logBase 2 r) :: Int) ++ " cents")
         ]
         ++ [ txt env (x + 8) (y + 90) 14 (gray 0.85) (show a ++ "/" ++ show b) | row == 1, (a, b) <- take 1 (drop i just) ]
    info =
      [ txt env 40 420 14 (gray 0.7) "phi = 1.618 = 833 cents lies between just 8/5 (814) and 5/3 (884). Compare the beats." ]
    -- リズムの帯
    ty = 460
    tw = envW env - 80
    len = U.length (wWord w)
    shown = min len 60
    chars = U.toList (U.take shown (wWord w))
    total = sum (map (durOf w) chars)
    cur = if len == 0 then -1 else ((wPos w + len - 1) `mod` len) `mod` max 1 shown
    rhythm = go 40 (zip [0 ..] chars) ++
      [ txt env 40 (ty + 84) 14 (gray 0.7) ("Fibonacci-word rhythm (sketch 10: A -> " ++ replicate n 'A' ++ "B, B -> A). Never repeats, yet sounds regular.")
      , txt env 40 (ty + 106) 14 (gray 0.7) ("A plays the top scale in order, B a short note 2 octaves up. Count A / count B -> phi" ++ show n ++ " = " ++ nf 4 phi)
      ]
    go :: Float -> [(Int, Char)] -> [Picture]
    go _ [] = []
    go x ((i, ch) : r) =
      let l = durOf w ch / total * tw
          on = wPlaying w && i == cur
          col = if ch == 'A' then hsb 45 70 (if on then 100 else 70) else hsb 200 70 (if on then 100 else 70)
      in Color col (box env x (ty + (if on then -8 else 0)) (l - 2) (50 + (if on then 16 else 0)))
         : [ txt env (x + l / 2 - 5) (ty + 32) 14 (hsb 0 0 10) [ch] | l > 14 ]
         ++ go (x + l) r

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    let (mx0, my0) = envMouse env
        mx = mx0 + envW env / 2
        my = envH env / 2 - my0
        hits = [ (row, i) | (row, i, x, y, kw, kh) <- keyRects env, mx > x, mx < x + kw, my > y, my < y + kh ]
    forM_ hits $ \(row, i) -> do
      -- 根音と同時に鳴らして「和音」として聴く
      tone baseHz 0.15 1.2 Sine
      tone (baseHz * ratioOf w row i) 0.15 1.2 Sine
    pure w { wFlash = wFlash w U.// [ (row * 8 + i, 1) | (row, i) <- hits ] }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure w { wPlaying = not (wPlaying w), wNextTime = envTime env + 0.05 }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (rebuild w { wN = min (wN w + 1) 4 })
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure (rebuild w { wN = max (wN w - 1) 1 })
  EventKey (SpecialKey KeyUp) Down _ _    -> pure w { wBpm = min 300 (wBpm w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _  -> pure w { wBpm = max 60 (wBpm w - 10) }
  EventKey (Char c) Down _ _
    | c == 'd' || c == 'D' -> pure w { wEqual = not (wEqual w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 上段の 1.618 と下段の 8/5 = 1.6、5/3 = 1.667 を順に鳴らしてうなりを比べよう
-- 2. D で等間隔にすると、リズムは「音の高さ」の並びだけになる。どちらが好き？
-- 3. baseHz を 110 や 330 に変えて雰囲気を変えよう
-- 4. B のとき Sine の代わりに Square で短く鳴らすように改造してみよう

-- ---------- 道具 ----------
metallic :: Float -> Float
metallic k = (k + sqrt (k * k + 4)) / 2

foldOctave :: Float -> Float
foldOctave r = if r >= 2 then foldOctave (r / 2) else r

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]
