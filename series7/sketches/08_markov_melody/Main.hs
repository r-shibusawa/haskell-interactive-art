-- ============================================
-- 08 マルコフ連鎖で作曲（曲から曲を学ぶ）
-- お手本のメロディから「この音の次はどの音が来やすいか」の表（遷移確率）を作り、
-- その表にしたがってサイコロを振ると、お手本に似た新しいメロディが生まれる
-- クリックで再生・停止 / 1〜3 でお手本の曲
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Table = M.Map Char (M.Map Char Int)   -- table['C'] = { D: 3, G: 2, ... }

data World = World
  { wCurrent :: !Int      -- お手本の番号（0〜2）
  , wTable   :: !Table
  , wGen     :: ![Char]   -- 生成したメロディ（古い順、最大 32 音）
  , wPlaying :: !Bool
  , wNext    :: !Float    -- 次の音を鳴らす時刻（秒）
  , wLast    :: !Char
  }

-- ★ お手本の曲（自分の好きな曲をドレミで書いて追加しよう）
songs :: [(String, String)]
songs =
  [ ("Twinkle Star", "CCGGAAGFFEEDDCGGFFEEDGGFFEEDCCGGAAGFFEEDDC")
  , ("Frog Song", "CDEFEDCEFGAGFECCCCCCDDEEFFEDC")
  , ("Butterfly", "GEEFDDCDEFGGGGEEFDDCEGGEDDDDDEFEEEEEFGGEEFDDCEGGE")
  ]

noteHz :: Char -> Float
noteHz c = fromMaybe 261.6 (lookup c
  [('C', 261.6), ('D', 293.7), ('E', 329.6), ('F', 349.2), ('G', 392.0), ('A', 440.0), ('B', 493.9)])

names :: String
names = "CDEFGAB"

stepSec :: Float
stepSec = 0.3              -- ★ 1 音の長さ（秒）

main :: IO ()
main = runSketch (sketch "07-08 Markov Melody" (learn 0 (World 0 M.empty [] False 0 'C')))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> scheduler (envTime env) w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 学習：お手本の「前の音 → 次の音」を数える
learn :: Int -> World -> World
learn i w =
  let notes = snd (songs !! i)
      count t (a, b) = M.insertWith (M.unionWith (+)) a (M.singleton b 1) t
      table = foldl' count M.empty (zip notes (drop 1 notes))
  in w { wCurrent = i, wTable = table, wGen = [], wLast = case notes of { (n : _) -> n; [] -> 'C' } }

-- ★ 生成：表の数に比例した確率で次の音を選ぶ
nextNote :: Table -> Char -> IO Char
nextNote table prev = case M.lookup prev table of
  Nothing -> case M.keys table of
    [] -> pure prev
    ks -> (ks !!) <$> randomRIO (0, length ks - 1)
  Just row -> do
    let total = sum (M.elems row)
    r <- randomRIO (0, total - 1)
    pure (pick r (M.toList row))
  where
    pick _ [] = prev
    pick r ((n, c) : rest)
      | r < c = n
      | otherwise = pick (r - c) rest

-- 時刻が来たら次の音を選んで鳴らす
scheduler :: Float -> World -> IO World
scheduler now w
  | wPlaying w && wNext w <= now = do
      n <- nextNote (wTable w) (wLast w)
      playTone (noteHz n) 0.3 0.3 Triangle
      scheduler now w
        { wLast = n
        , wGen = drop (length (wGen w) + 1 - 32) (wGen w) ++ [n]
        , wNext = wNext w + stepSec }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (table ++ melody ++ hud)
  where
    wd = envW env
    ht = envH env
    -- ---- 遷移表（ヒートマップ）----
    cell = min 60 ((ht - 240) / 8)
    tx = 160
    ty = 130
    count a b = M.findWithDefault 0 b (M.findWithDefault M.empty a (wTable w))
    table =
      txt env 40 (ty - 30) 16 white "Transition table: row = current note, column = next note (number = count)"
      : concat
        [ [ txtM env (tx + fi * cell + cell / 2) (ty - 12) 16 (hsb 0 0 70) [a]
          , txtM env (tx - 30) (ty + fi * cell + cell / 2) 16 (hsb 0 0 70) (a : " ->")
          ]
          ++ concat
            [ [ Color (cellColor v (a == wLast w)) (box env (tx + fj * cell + 1) (ty + fi * cell + 1) (cell - 2) (cell - 2))
              , if v > 0 then txtM env (tx + fj * cell + cell / 2) (ty + fi * cell + cell / 2) 16 white (show v) else Blank
              ]
            | (j, b) <- zip [0 :: Int ..] names
            , let fj = fromIntegral j
                  v = count a b ]
        | (i, a) <- zip [0 :: Int ..] names
        , let fi = fromIntegral i ]
    cellColor :: Int -> Bool -> Color
    cellColor v hot
      | v > 0 = hsb 170 70 (40 + fromIntegral (min v 6) * 10)
      | hot = hsb 260 30 25
      | otherwise = hsb 260 30 15
    -- ---- 生成したメロディ ----
    gx = tx + 7 * cell + 80
    gw = wd - gx - 60
    gy = ht * 0.55
    gh = ht * 0.35
    rowY :: Int -> Float
    rowY i = gy + gh - fromIntegral i / 6 * gh
    newest = length (wGen w) - 1
    melody =
      [ Color (hsb 0 0 25) (Line [toG env (gx, rowY i), toG env (gx + gw, rowY i)]) | i <- [0 .. 6] ]
      ++ [ Color (hsb (if k == newest then 45 else 170) 80 100)
             (at (toG env (gx + fromIntegral k / 32 * gw, rowY (fromMaybe 0 (elemIndex n names))))
                 (disc (if k == newest then 11 else 7)))
         | (k, n) <- zip [0 ..] (wGen w) ]
      ++ [ txt env gx (gy - 12) 14 (hsb 0 0 60) "Generated melody (latest 32 notes)" ]
      ++ [ txt env (gx - 22) (rowY i + 5) 14 (hsb 0 0 60) [n] | (i, n) <- zip [0 ..] names ]
    hud =
      [ txt env 20 40 18 white
          ("Model: " ++ fst (songs !! wCurrent w) ++ " (1-3)   Click: " ++ (if wPlaying w then "stop" else "generate and play")
           ++ "   Brighter cell = more frequent next note")
      , txt env 20 68 15 (hsb 0 0 60) "A melody that sounds like the model song, not a copy. Language models use a far bigger version of this."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let on = not (wPlaying w)
    in pure (if on then w { wPlaying = True, wNext = envTime env + 0.05 } else w { wPlaying = False })
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure (learn (fromEnum c - fromEnum '1') w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3 曲を学習させて、生成されるメロディの「らしさ」を比べよう
-- 2. 「2 音前まで見る」（2 次マルコフ）にすると、もっとお手本に近くなる。Table のキーを (a, b) にしてみよう
-- 3. 自分の好きな曲をドレミで書いて songs に追加

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（上下左右とも中央揃え）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx gy s c str
