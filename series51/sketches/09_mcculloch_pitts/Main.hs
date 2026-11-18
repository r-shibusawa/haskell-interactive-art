-- ============================================
-- 51-09 マカロック-ピッツのニューロン（1943）
-- ニューロン = 「興奮の入力の数がしきい値以上で、抑制の入力が 1 つもなければ、次の時刻に発火（1）」という 0/1 の素子
-- これだけで AND・OR・NOT が作れ、ループがあると「覚える」（フリップフロップ）。脳を論理回路として考えた最初の論文
-- 操作：1 2 = 入力 A・B / 3 4 = セット S・リセット R（クリックでも） / スペース = 1 時刻進める / P = 自動で進める / C = 全部 0 に
-- ============================================
module Main (main) where

import Art
import qualified Data.Map.Strict as M
import System.Environment (lookupEnv)

-- ニューロン：名前、興奮の入力、抑制の入力、しきい値、画面の位置（0〜1）
data Neuron = Neuron { nId :: String, nEx :: [String], nInh :: [String], nTh :: !Int, nX, nY :: !Float }

-- ★ ネットワーク：ex = 興奮の入力、inh = 抑制の入力、th = しきい値。入力（A, B, S, R）は外から決める
net :: [Neuron]
net =
  [ Neuron "AND" ["A", "B"] [] 2 0.30 0.22
  , Neuron "OR" ["A", "B"] [] 1 0.30 0.40
  , Neuron "NOT A" [] ["A"] 0 0.30 0.58
  , Neuron "A&~B" ["A"] ["B"] 1 0.50 0.22
  , Neuron "B&~A" ["B"] ["A"] 1 0.50 0.40
  , Neuron "XOR" ["A&~B", "B&~A"] [] 1 0.66 0.31
  , Neuron "Mem M" ["S", "Mem M"] ["R"] 1 0.50 0.74 ]

-- 入力の名前と位置
inputs :: [(String, Float, Float)]
inputs = [("A", 0.08, 0.28), ("B", 0.08, 0.48), ("S", 0.08, 0.68), ("R", 0.08, 0.84)]

type State = M.Map String Int

autoInterval :: Float
autoInterval = 0.7     -- ★ 自動で 1 時刻進める間隔 [秒]

data World = World
  { wInp    :: State
  , wState  :: State
  , wTick   :: !Int
  , wAuto   :: !Bool
  , wLastT  :: !Float
  , wRaster :: [State]      -- 発火の記録（古い順、30 時刻まで）
  , wPulse  :: !Float
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-09 McCulloch-Pitts neurons" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

zeroInp :: State
zeroInp = M.fromList [(q, 0) | (q, _, _) <- inputs]

zeroState :: State
zeroState = M.fromList [(nId n, 0) | n <- net]

world0 :: World
world0 = World zeroInp zeroState 0 True 0 [] 0 []

-- ---- 1 時刻進める：時刻 t の値（入力 + ニューロン）から、t + 1 のニューロンの値を同時に計算
netTick :: State -> State -> State
netTick s input = M.fromList [ (nId n, fire n) | n <- net ]
  where
    v i = M.findWithDefault (M.findWithDefault 0 i s) i input
    fire n = let inhibited = any (\i -> v i == 1) (nInh n)
                 total = sum (map v (nEx n))
             in if not inhibited && total >= nTh n then 1 else 0

doTick :: Float -> World -> World
doTick t w = w { wRaster = lastN 30 (wRaster w ++ [M.union (wInp w) (wState w)])
               , wState = netTick (wState w) (wInp w), wTick = wTick w + 1, wLastT = t, wPulse = 1 }

step :: Env -> Float -> World -> IO World
step env dt w0 = pure w { wPulse = max 0 (wPulse w - dt * 1000 / 400) }
  where w = if wAuto w0 && envTime env - wLastT w0 > autoInterval then doTick (envTime env) w0 else w0

-- 画面の配置
layout :: Env -> (Float -> Float, Float -> Float, Float)
layout env = let ww = envW env * 0.78
                 hh = envH env - 160
             in (\f -> 20 + f * ww, \f -> 90 + f * hh, ww)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("McCulloch-Pitts neurons: logic and memory (t = " ++ show (wTick w) ++ ")") 20 36 20 cTxt
  , say ("1 2 inputs A, B   3 4 set S / reset R (or click)   Space one step   P auto " ++ (if wAuto w then "ON" else "OFF") ++ "   C all 0") 20 62 15 cSub ]
  ++ concatMap links net
  ++ concatMap inputBox inputs
  ++ concatMap neuron net
  ++ [ say "Green arrow = excite (+1)   red dot = inhibit (any one blocks firing)" 20 (yf 0.96) 14 cSub
     , say "XOR has two layers, so it answers two steps later" 20 (yf 0.96 + 20) 14 cSub ]
  ++ raster
  ++ [ drawSelfTest env (wTest w) ]
  where
    (xf, yf, ww) = layout env
    pos i = case [ (xf x, yf y) | (q, x, y) <- inputs, q == i ] ++ [ (xf (nX n), yf (nY n)) | n <- net, nId n == i ] of
              (p : _) -> p
              [] -> (0, 0)
    val i = M.findWithDefault (M.findWithDefault 0 i (wState w)) i (wInp w)
    -- つながり
    links n = concat [ link n i inh | (list, inh) <- [(nEx n, False), (nInh n, True)], i <- list ]
    link n i inh =
      let (x1, y1) = pos i
          (x2, y2) = pos (nId n)
          on = val i == 1
          c = if inh then cAlarm else cScope
          lc = col c (if on then 90 else 30)
          wt = if on then 3 else 1.5
          ex = x2 - 46
          ey = y2
          f = 1 - wPulse w
      in if i == nId n
           then [ Color lc (thickPath wt [ (x2 + 52 + 30 * cos a, y2 + 23 * sin a) | k <- [0 .. 40 :: Int]
                                         , let a = (-pi / 2 - 1.2) + (pi + 2.4) * fromIntegral k / 40 ]) ]
           else [ Color lc (thickLine wt (x1 + 40, y1) (ex, ey))
                , Color (col c 100) (if inh then at (ex, ey) (disc 5) else Polygon [(ex, ey), (ex - 10, ey - 5), (ex - 10, ey + 5)]) ]
                ++ [ Color (col cTube 100) (at (x1 + 40 + (ex - x1 - 40) * f, y1 + (ey - y1) * f) (disc 4)) | on ]
    -- 入力
    inputBox (q, _, _) =
      let (x, y) = pos q
          on = val q
      in [ Color (if on == 1 then col cTube 100 else hsb 25 30 18) (rectTL (x - 40) (y - 22) 80 44)
         , Color (col cBrass 100) (frameTL 2 (x - 40) (y - 22) 80 44)
         , sayC (q ++ " = " ++ show on) x y 18 (if on == 1 then (0, 0, 5) else cTxt) ]
    -- ニューロン
    neuron n =
      let (x, y) = pos (nId n)
          on = M.findWithDefault 0 (nId n) (wState w)
      in [ Color (if on == 1 then col cTube 100 else hsb 25 30 18) (at (x, y) (disc 44))
         , Color (col cBrass 100) (at (x, y) (ring 44 2))
         , sayC (nId n) x (y - 9) 14 (if on == 1 then (0, 0, 5) else cTxt)
         , sayC ("th=" ++ show (nTh n) ++ "  " ++ show on) x (y + 12) 14 (if on == 1 then (0, 0, 5) else cTxt) ]
    -- 発火の記録（ラスター図）
    rx = ww + 40
    ry = 130
    rw = envW env - rx - 20
    ids = [ q | (q, _, _) <- inputs ] ++ map nId net
    rh = min 30 ((envH env - 200) / fromIntegral (length ids))
    cw = (rw - 70) / 30
    raster = say "Firing record" rx (ry - 34) 14 cSub
      : say "(30 steps, new ->)" rx (ry - 14) 14 cSub
      : concat [ say i rx (ry + fi * rh + rh * 0.7) 14 cSub
                 : [ Color (if M.member i (wInp w) then col cBrass 100 else col cTube 100) (rectTL (rx + 70 + fromIntegral k * cw) (ry + fi * rh + 3) (cw - 1) (rh - 6))
                   | (k, r) <- zip [0 :: Int ..] (wRaster w), M.findWithDefault 0 i r == 1 ]
               | (fi, i) <- zip [0 ..] ids ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '1' -> toggle "A" w
    | c == '2' -> toggle "B" w
    | c == '3' -> toggle "S" w
    | c == '4' -> toggle "R" w
    | c `elem` "pP" -> w { wAuto = not (wAuto w) }
    | c `elem` "cC" -> w { wInp = zeroInp, wState = zeroState, wRaster = [] }
  EventKey (SpecialKey KeySpace) Down _ _ -> doTick (envTime env) w
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = p5Mouse env
        (xf, yf, _) = layout env
    in foldl (\a (q, x, y) -> if abs (mx - xf x) < 42 && abs (my - yf y) < 24 then toggle q a else a) w inputs
  _ -> w

toggle :: String -> World -> World
toggle i w = w { wInp = M.adjust (1 -) i (wInp w) }

-- ---- 自己テスト：真理値表と記憶（p5.js 版と同じ 7 項目）
run :: State -> State -> Int -> State
run s input n = iterate (`netTick` input) s !! max 0 n

inp4 :: Int -> Int -> Int -> Int -> State
inp4 a b s r = M.fromList [("A", a), ("B", b), ("S", s), ("R", r)]

selfTest :: [(String, Bool)]
selfTest =
  [ ("AND truth table 0 0 0 1 (threshold 2)", gate1 "AND" [0, 0, 0, 1])
  , ("OR truth table 0 1 1 1 (threshold 1)", gate1 "OR" [0, 1, 1, 1])
  , ("NOT: A = 0 -> 1, A = 1 -> 0 (inhibit only, threshold 0)", gate1 "NOT A" [1, 1, 0, 0])
  , ("XOR: two layers give 0 1 1 0 (two steps later)", and [ get "XOR" (run zeroState (inp4 a b 0 0) 2) == e | ((a, b), e) <- zip abs4 [0, 1, 1, 0] ])
  , ("memory: S for one step, then held at 1 for 10 steps", hold)
  , ("R makes it 0 and it stays 0", cleared)
  , ("S and R together: R wins (inhibition is absolute)", get "Mem M" (netTick (M.insert "Mem M" 1 zeroState) (inp4 0 0 1 1)) == 0) ]
  where
    get i s = M.findWithDefault (-1) i s
    abs4 = [(0, 0), (0, 1), (1, 0), (1, 1)]
    gate1 g tt = and [ get g (run zeroState (inp4 a b 0 0) 1) == e | ((a, b), e) <- zip abs4 tt ]
    s0 = run zeroState (inp4 0 0 1 0) 1
    holdSeq = take 10 (drop 1 (iterate (`netTick` inp4 0 0 0 0) s0))
    hold = all (\s -> get "Mem M" s == 1) holdSeq
    sEnd = last (s0 : holdSeq)
    sR = netTick sEnd (inp4 0 0 0 1)
    cleared = get "Mem M" sR == 0 && all (\s -> get "Mem M" s == 0) (take 10 (drop 1 (iterate (`netTick` inp4 0 0 0 0) sR)))

-- ---------- やってみよう ----------
-- 1. 1 2 で A・B を切りかえ、AND・OR・XOR の出力が何時刻あとに変わるかラスター図で確かめよう
-- 2. 3 で S を入れてすぐ切る。記憶 M はどうなる？ 4 で R を入れると？
-- 3. net に「NAND」（A と B の両方が来たら発火しない）を足すには？ ヒント：しきい値 0 と抑制
-- 4. 記憶 M の自分へのループ（nEx の "Mem M"）を消すと？ ループが記憶になる理由を説明しよう
-- 5. 3 つのニューロンを輪につなぐと、1 つの発火がぐるぐる回る（リズム）。net に足して作ってみよう

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 最後の n 個
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
