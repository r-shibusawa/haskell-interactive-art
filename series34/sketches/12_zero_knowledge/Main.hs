-- ============================================
-- 12 ゼロ知識証明（洞窟の例・グラフ彩色）
-- 「秘密を知っている」ことだけを、秘密そのものは 1 ビットも見せずに納得させる方法。
--   左：輪になった洞窟の奥に合言葉の扉。ペギーが入り、ビクターが「A から出て」「B から出て」と叫ぶ。
--       合言葉を知らない人は 1 回あたり 1/2 の確率でしか当てられない → k 回続けて成功する確率は (1/2)^k
--   右：グラフの 3 彩色。毎回色をシャッフルして封をし（コミット）、ビクターが選んだ辺の両端だけ開ける
-- スペース = 次のラウンド / A = 自動 / K = ペギーが本当に知っている ⇄ ずるをする / 辺をクリック = その辺を開けさせる / R = リセット
-- ※ 教育用の概念モデル
-- ============================================
module Main (main) where

import Art
import Text.Printf (printf)

-- グラフ（7 頂点）。正しい 3 彩色と、1 本だけ矛盾のある「ずる」の彩色
nodeCount :: Int
nodeCount = 7

edges :: [(Int, Int)]
edges = [(0, 1), (1, 2), (2, 0), (0, 4), (1, 3), (2, 5), (3, 4), (4, 5), (5, 3), (3, 6), (6, 0), (4, 2)]

goodColors, badColors :: [Int]
goodColors = [0, 1, 2, 2, 1, 0, 1]
badColors = [0, 1, 2, 2, 1, 0, 2]        -- 頂点 6 が 頂点 3 と同じ色 → 辺 3-6 が矛盾

palette :: [Color]
palette = [rgb255 255 110 110, rgb255 110 220 140, rgb255 110 160 255]

data World = World
  { knows    :: !Bool          -- ★ True = 秘密を知っている、False = ずるをする
  , auto     :: !Bool
  , phase    :: !Int           -- 洞窟のアニメ：0 入る 1 呼ぶ 2 出る 3 結果
  , tAnim    :: !Float
  , hideSide :: !Int
  , callSide :: !Int
  , exitSide :: !Int
  , caveLog  :: ![Bool]        -- 新しいものが先頭、最大 200 個
  , colorPerm :: ![Int]
  , openEdge :: !Int
  , graphLog :: ![Bool]        -- 新しいものが先頭、最大 200 個
  }

main :: IO ()
main = runSketch (sketch "34-12 Zero-Knowledge Proof" (World True False 3 0 0 0 0 [] [0, 1, 2] (-1) []))
  { skBack  = rgb255 16 16 22
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 洞窟のアニメを進める
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | phase w < 3 =
      let t' = tAnim w + (if auto w then 0.06 else 0.035)
      in pure $ if t' >= 1
           then finishIf w { tAnim = 0, phase = phase w + 1 }
           else w { tAnim = t' }
  | auto w && envFrame env `mod` 30 == 0 = nextRound w
  | otherwise = pure w
  where
    finishIf v
      | phase v == 3 = v { caveLog = take 200 ((exitSide v == callSide v) : caveLog v) }
      | otherwise = v

nextRound :: World -> IO World
nextRound w
  | phase w < 3 = pure w
  | otherwise = do
      hide <- randomRIO (0, 1)                  -- ペギーはどちらかに隠れる（ビクターは見ていない）
      call <- randomRIO (0, 1)                  -- ★ ビクターはランダムに呼ぶ
      -- 合言葉で扉を通れるので、いつも言われた側から出られる。通れなければ隠れた側からしか出られない
      let out = if knows w then call else hide
      e <- randomRIO (0, length edges - 1)
      graphRound e w { hideSide = hide, callSide = call, exitSide = out, phase = 0, tAnim = 0 }

graphRound :: Int -> World -> IO World
graphRound e w = do
  perm <- shuffle [0, 1, 2]                     -- ★ 毎回、色の名前をシャッフルしてから封をする
  let col = if knows w then goodColors else badColors
      (a, b) = edgeAt e
  pure w { colorPerm = perm, openEdge = e, graphLog = take 200 ((nth 0 col a /= nth 0 col b) : graphLog w) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ drawCave env w cx1 cy1 r1 ++ drawGraph env w cx2 cy2 r2)
  where
    sw = envW env
    sh = envH env
    -- 洞窟はビクターの文字が記録の行と重ならないよう、p5.js 版より少し小さい
    cx1 = sw * 0.25
    cy1 = sh * 0.42
    r1 = min (sw * 0.17) (sh * 0.22)
    cx2 = sw * 0.74
    cy2 = sh * 0.42
    r2 = min (sw * 0.18) (sh * 0.25)
    header =
      [ txt env 24 34 18 (gray 0.94)
          ("Zero-knowledge proof  Space next round  A auto " ++ (if auto w then "ON" else "off")
            ++ "  K Peggy: " ++ (if knows w then "really knows" else "cheats") ++ "  click edge  R reset")
      , txt env 24 56 14 (gray 0.6) "Victor never learns the secret (password, coloring), yet gets convinced that Peggy knows it | educational"
      ]

-- ---------------- 洞窟 ----------------
drawCave :: Env -> World -> Float -> Float -> Float -> [Picture]
drawCave env w cx cy r =
  [ -- 岩
    at (toS env (cx, cy)) (Color rock (disc (r + 35)))
  , box env (cx - 30) (cy + r) 60 (r * 0.6) rock
    -- 通路（輪）
  , at (toS env (cx, cy)) (Color tunnel (ring r 34))
  , Color tunnel (thickLine 34 (toS env (cx, cy + r)) (toS env (cx, cy + r * 1.6)))
    -- 扉（上）
  , Color (rgb255 255 200 90) (thickLine 8 (toS env (cx, cy - r - 20)) (toS env (cx, cy - r + 20)))
  , txtC env cx (cy - r - 40) 16 (gray 0.86) "Password door"
  , txtC env (cx - r - 58) (cy + 6) 16 (gray 0.86) "A"
  , txtC env (cx + r + 58) (cy + 6) 16 (gray 0.86) "B"
    -- ビクター
  , dot (cx, vy) 13 blue'
  , txt env (cx + 36) (vy + 6) 14 blue' "Victor"
  , if phase w >= 1 && phase w <= 3 && called
      then txtC env cx (cy + r * 1.9) 18 blue' ("\"Exit from " ++ [sideName (callSide w)] ++ "!\"")
      else Blank
    -- ペギー
  , dot (px, py) 13 pink
  , txtC env px (py - 20) 14 pink "Peggy"
  ]
  ++ resultLine ++ logRow
  where
    rock = rgb255 48 42 38
    tunnel = rgb255 110 96 80
    blue' = rgb255 130 180 255
    pink = rgb255 255 140 180
    good = rgb255 120 230 150
    bad = rgb255 255 110 110
    dot p rad c = at (toS env p) (Color c (disc rad))
    called = phase w < 3 || not (null (caveLog w))
    vy | phase w == 1 = lerpF (cy + r * 1.7) (cy + r + 10) (tAnim w)
       | phase w >= 1 = cy + r + 10
       | otherwise = cy + r * 1.7
    ang = peggyAngle w * pi / 180
    px = cx + r * cos ang
    py = cy + r * sin ang
    -- 結果
    resultLine = case caveLog w of
      (lastOk : _) | phase w == 3 ->
        [ txtC env cx cy 16 (if lastOk then good else bad)
            (if lastOk then "Exited the called side: OK" else "Wrong side: cheat caught") ]
      _ -> []
    -- 記録
    streak = length (takeWhile id (caveLog w))
    oddsText | streak > 30 = "~0"
             | otherwise = "1/" ++ commas (2 ^ streak :: Int)
    ly = envH env - 90
    logRow =
      txt env 24 ly 15 (gray 0.78) ("Streak " ++ show streak ++ " -> odds of a lucky cheat (1/2)^" ++ show streak ++ " = " ++ oddsText)
      : logDots env 34 (ly + 22) (caveLog w)

-- 洞窟の中のペギーの角度（度）。下 = 90°、A（左）側 = 180°、上の扉 = 270°、B（右）側 = 360°
peggyAngle :: World -> Float
peggyAngle w = case phase w of
  0 -> if hideSide w == 0 then lerpF 90 hideA (tAnim w) else lerpF 90 (hideB - 360) (tAnim w)
  1 -> hide
  _ -> let s = if phase w == 3 then 1 else tAnim w
       in if exitSide w == 0 then lerpF hide 90 s else lerpF hide 450 s
  where
    hideA = 225
    hideB = 315
    hide = if hideSide w == 0 then hideA else hideB

-- ---------------- グラフ彩色 ----------------
nodePos :: Int -> Float -> Float -> Float -> Point
nodePos i cx cy r
  | i < 3 = let a = -pi / 2 + fromIntegral i * tau / 3 in (cx + r * 0.45 * cos a, cy + r * 0.45 * sin a)
  | i < 6 = let a = pi / 2 + fromIntegral (i - 3) * tau / 3 in (cx + r * cos a, cy + r * sin a)
  | otherwise = (cx - r * 1.15, cy - r * 0.75)

drawGraph :: Env -> World -> Float -> Float -> Float -> [Picture]
drawGraph env w cx cy r = lines' ++ nodes ++ texts ++ logRow
  where
    col = if knows w then goodColors else badColors
    open = openEdge w >= 0
    (oa, ob) = edgeAt (openEdge w)
    lines' =
      [ Color (if e == openEdge w then rgb255 255 220 120 else rgb255 90 90 110)
          (thickLine (if e == openEdge w then 5 else 3) (toS env (nodePos a cx cy r)) (toS env (nodePos b cx cy r)))
      | (e, (a, b)) <- zip [0 ..] edges ]
    nodes = concat
      [ if shown
          then [ at (toS env p) (Color (nth white palette (nth 0 (colorPerm w) (nth 0 col i))) (disc 17)) ]
          else [ at (toS env p) (Color (rgb255 70 70 86) (disc 17))
               , txtC env (fst p) (snd p + 7) 14 (gray 0.6) "?" ]
      | i <- [0 .. nodeCount - 1]
      , let p = nodePos i cx cy r
            shown = open && (i == oa || i == ob) ]
    okEdge = nth 0 col oa /= nth 0 col ob
    good = rgb255 120 230 150
    bad = rgb255 255 110 110
    texts =
      txtC env cx (cy - r - 30) 14 (gray 0.86) "Knows a 3-coloring: neighbours differ (? = sealed commitment)"
      : if not open then [] else
          if okEdge
            then [ txtC env cx (cy + r + 50) 15 good "The two opened colors differ: OK"
                 , txtC env cx (cy + r + 72) 14 good "(colors are reshuffled each time, so nothing leaks)" ]
            else [ txtC env cx (cy + r + 50) 15 bad "Same color: cheat caught" ]
    passed = length (takeWhile id (graphLog w))
    nE = length edges
    pCheat = (1 - 1 / fromIntegral nE) ^ passed :: Float
    ly = envH env - 90
    lx = envW env * 0.52
    logRow =
      txt env lx ly 14 (gray 0.78)
        ("Streak " ++ show passed ++ " -> cheat survives (1 - 1/" ++ show nE ++ ")^" ++ show passed ++ " = " ++ printf "%.4f" pCheat)
      : logDots env (lx + 10) (ly + 22) (graphLog w)

-- 記録の丸（最新 40 個を古い順に）
logDots :: Env -> Float -> Float -> [Bool] -> [Picture]
logDots env x y logs =
  [ at (toS env (x + fromIntegral i * 14, y)) (Color (if v then rgb255 120 230 150 else rgb255 255 110 110) (disc 5))
  | (i, v) <- zip [0 :: Int ..] (reverse (take 40 logs)) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let sw = envW env
        sh = envH env
        cx = sw * 0.74
        cy = sh * 0.42
        r = min (sw * 0.18) (sh * 0.25)
        (mx0, my0) = envMouse env
        m = (mx0 + sw / 2, sh / 2 - my0)
    in case [ e | (e, (a, b)) <- zip [0 ..] edges, distToSegment m (nodePos a cx cy r) (nodePos b cx cy r) < 12 ] of
         (e : _) -> graphRound e w
         [] -> pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> nextRound w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { auto = not (auto w) }
    | c `elem` "kK" -> pure w { knows = not (knows w), caveLog = [], graphLog = [], openEdge = -1 }
    | c `elem` "rR" -> pure w { caveLog = [], graphLog = [], openEdge = -1, phase = 3 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. K で「ずる」にして A で自動。洞窟は何回目でばれる？ グラフ彩色はなかなかばれないのはなぜ？
-- 2. ずるのペギーが「ビクターが何と言うか前もって知っていたら」？ → 呼ぶ側のランダムさが大切
-- 3. 色をシャッフルしない（colorPerm = [0, 1, 2] のまま）と、何回も開けるうちに塗り方がバレる。なぜ？
-- 4. 成功確率 99.9999% にするには洞窟で何回、グラフで何回のラウンドが要る？

-- ==================================================
-- ここから下は「道具」（点と線分の距離、リストの道具、座標）
-- ==================================================
distToSegment :: Point -> Point -> Point -> Float
distToSegment (px, py) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      u = clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy))
      ex = px - ax - u * dx
      ey = py - ay - u * dy
  in sqrt (ex * ex + ey * ey)

-- i 番目（なければ既定値）
nth :: a -> [a] -> Int -> a
nth d xs i = case drop i xs of
  (x : _) -> x
  [] -> d

edgeAt :: Int -> (Int, Int)
edgeAt e = if e < 0 then (-1, -1) else nth (0, 0) edges e

sideName :: Int -> Char
sideName s = if s == 0 then 'A' else 'B'

shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (a, rest) = splitAt i xs
  case rest of
    (x : b) -> (x :) <$> shuffle (a ++ b)
    [] -> pure a

commas :: Int -> String
commas n = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, b) -> a ++ "," ++ go b

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC e x y s c str = txt e (x - fromIntegral (length str) * s * 0.36) y s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
