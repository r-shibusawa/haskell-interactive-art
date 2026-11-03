-- ============================================
-- 13 しみこみブラシ（自由制作のベース）─ 互いに素な集合で「つながり」をその場で数える
-- マスを開けるたびに、となりの開いたマスと UNION。上の端・下の端は見えない 2 つの要素にまとめておく
-- FIND(上) = FIND(下) になった瞬間、水が上から下まで「しみとおる」（パーコレーション）。開いた割合 p ≈ 0.593 が境目
-- ドラッグで塗る / A = ランダムに自動で開ける / ←→ = ブラシの大きさ / S = 音 ON/OFF / C = 消す / R = 色を変える
-- ============================================
-- ★ のついた数字を変えて、自分の作品に！
module Main (main) where

import Art
import Control.Monad (foldM, forM_, replicateM, when)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Text.Printf (printf)

cols, rows :: Int
cols = 64             -- ★ マスの数
rows = 40

startBrush :: Int
startBrush = 1        -- ★ ブラシの半径（マス）

startHue :: Float
startHue = 200        -- ★ 色の基準

scaleSteps :: [Int]
scaleSteps = [0, 3, 5, 7, 10]  -- ★ 音階（マイナーペンタトニック）

topN, botN :: Int
topN = cols * rows
botN = cols * rows + 1

-- 森（親とランク）。サイズは塊を数える森だけが持つ
data Forest = Forest { fParent :: !(UM.IOVector Int), fRank :: !(UM.IOVector Int) }

data World = World
  { wOpened  :: !(UM.IOVector Bool)
  , wF1      :: !Forest                -- 塊を数える森（マスどうしだけ）
  , wSize    :: !(UM.IOVector Int)
  , wF2      :: !Forest                -- 上下の端も入れたもう 1 つの森（しみとおり判定用）
  , wComps   :: !Int
  , wNOpen   :: !Int
  , wAuto    :: !Bool
  , wSound   :: !Bool
  , wPerc    :: !Bool
  , wFlash   :: !Float
  , wPAt     :: !Float
  , wLast    :: !Float
  , wBrush   :: !Int
  , wHue     :: !Float
  , wState   :: !(U.Vector Int)        -- 描画用：-1 閉じている、-2 しみとおった塊、-3 水、0 以上 = 塊の根
  , wStateSz :: !(U.Vector Int)        -- 描画用：塊の大きさ
  }

main :: IO ()
main = do
  w0 <- resetWorld startBrush startHue False True
  runSketch (sketch "13-13 Percolation Brush" w0)
    { skBack  = hsb startHue 30 7
    , skStep  = stepWorld
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

resetWorld :: Int -> Float -> Bool -> Bool -> IO World
resetWorld brush hue auto sound = do
  let n = cols * rows
  op <- UM.replicate n False
  p1 <- U.thaw (U.enumFromN 0 n)
  r1 <- UM.replicate n 0
  sz <- UM.replicate n 1
  p2 <- U.thaw (U.enumFromN 0 (n + 2))
  r2 <- UM.replicate (n + 2) 0
  pure World
    { wOpened = op, wF1 = Forest p1 r1, wSize = sz, wF2 = Forest p2 r2
    , wComps = 0, wNOpen = 0, wAuto = auto, wSound = sound, wPerc = False, wFlash = 0
    , wPAt = 0, wLast = -1, wBrush = brush, wHue = hue
    , wState = U.replicate n (-1), wStateSz = U.replicate n 0 }

-- ---------- 互いに素な集合の森 ----------
-- FIND-SET（経路半分化：1 つ飛ばしにつなぎ直しながら上る）
findSet :: Forest -> Int -> IO Int
findSet f x = do
  p <- UM.read (fParent f) x
  if p == x then pure x else do
    g <- UM.read (fParent f) p
    UM.write (fParent f) x g
    findSet f g

-- ランクで LINK する。合併したら (子の根, 親の根) を返す
linkSets :: Forest -> Int -> Int -> IO (Maybe (Int, Int))
linkSets f a b = do
  x0 <- findSet f a
  y0 <- findSet f b
  if x0 == y0 then pure Nothing else do
    rx0 <- UM.read (fRank f) x0
    ry0 <- UM.read (fRank f) y0
    let (x, y) = if rx0 > ry0 then (y0, x0) else (x0, y0)
    UM.write (fParent f) x y
    when (rx0 == ry0) (UM.modify (fRank f) (+ 1) y)
    pure (Just (x, y))

-- 両方の森で合併する。塊が 1 つ減ったら True
union :: World -> Int -> Int -> IO Bool
union w a b = do
  _ <- linkSets (wF2 w) a b
  r <- linkSets (wF1 w) a b
  case r of
    Nothing -> pure False
    Just (x, y) -> do
      sx <- UM.read (wSize w) x
      UM.modify (wSize w) (+ sx) y
      pure True

-- ---------- マスを開ける ----------
openCell :: Env -> World -> (Int, Int) -> IO World
openCell env w (c, r)
  | c < 0 || r < 0 || c >= cols || r >= rows = pure w
  | otherwise = do
      let i = r * cols + c
      was <- UM.read (wOpened w) i
      if was then pure w else do
        UM.write (wOpened w) i True
        w1 <- foldM joinNeighbor w { wNOpen = wNOpen w + 1, wComps = wComps w + 1 } [(1, 0), (-1, 0), (0, 1), (0, -1)]
        when (r == 0) (() <$ linkSets (wF2 w1) i topN)
        when (r == rows - 1) (() <$ linkSets (wF2 w1) i botN)
        -- 塊が大きいほど高い音
        let now = envTime env
            ring' = wSound w1 && now - wLast w1 > 0.04
        when ring' $ do
          root <- findSet (wF1 w1) i
          s <- UM.read (wSize w1) root
          let deg = min 14 (floor (logBase 2 (fromIntegral (s + 1) :: Float) * 1.3)) :: Int
              (oct, k) = deg `divMod` length scaleSteps
          playTone (midiFreq (57 + oct * 12 + maybe 0 id (safeAt scaleSteps k))) 0.1 0.2 Sine
        let w2 = if ring' then w1 { wLast = now } else w1
        t <- findSet (wF2 w2) topN
        b <- findSet (wF2 w2) botN
        if not (wPerc w2) && t == b
          then do
            -- しみとおった！
            when (wSound w2) (forM_ [0, 4, 7, 12] (\k -> playTone (midiFreq (57 + k)) 0.2 1.5 Triangle))
            pure w2 { wPerc = True, wFlash = 1, wPAt = fromIntegral (wNOpen w2) / fromIntegral (cols * rows) }
          else pure w2
  where
    joinNeighbor wa (dc, dr) = do
      let cc = c + dc
          rr = r + dr
          i = r * cols + c
          j = rr * cols + cc
      if cc < 0 || rr < 0 || cc >= cols || rr >= rows then pure wa else do
        o <- UM.read (wOpened wa) j
        if not o then pure wa else do
          merged <- union wa i j
          pure (if merged then wa { wComps = wComps wa - 1 } else wa)

-- ---------- 画面の寸法 ----------
geom :: Env -> (Float, Float, Float)
geom env =
  let fc = fromIntegral cols
      cs = min ((envW env - 60) / fc) ((envH env - 190) / fromIntegral rows)
  in (cs, envW env / 2 - fc * cs / 2, 120)

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  w1 <- if wAuto w0
          then do
            cells <- replicateM 4 ((,) <$> randomRIO (0, cols - 1) <*> randomRIO (0, rows - 1))
            foldM (openCell env) w0 cells
          else pure w0
  let (cs, x0, y0) = geom env
      (mx, my) = mouseXY env
      c = floor ((mx - x0) / cs) :: Int
      r = floor ((my - y0) / cs) :: Int
      b = wBrush w1
  w2 <- if envDown env
          then foldM (openCell env) w1 [ (c + dc, r + dr) | dc <- [1 - b .. b - 1], dr <- [1 - b .. b - 1] ]
          else pure w1
  -- 描画のために、各マスの状態を読み出しておく
  top <- findSet (wF2 w2) topN
  bot <- findSet (wF2 w2) botN
  let n = cols * rows
  st <- U.generateM n $ \i -> do
    o <- UM.read (wOpened w2) i
    if not o then pure (-1) else do
      r2 <- findSet (wF2 w2) i
      if r2 == top && top == bot then pure (-2)
        else if r2 == top then pure (-3)
        else findSet (wF1 w2) i
  sz <- U.generateM n $ \i ->
    let v = st U.! i in if v >= 0 then UM.read (wSize w2) v else pure 0
  let fl = wFlash w2 * 0.95
  pure w2 { wState = st, wStateSz = sz, wFlash = if fl < 0.02 then 0 else fl }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (back : cells ++ flash ++ edges ++ panel)
  where
    hue = wHue w
    (cs, x0, y0) = geom env
    fc = fromIntegral cols :: Float
    fr = fromIntegral rows :: Float
    back = fillBox env (hsb hue 30 7) 0 0 (envW env) (envH env)
    cells =
      [ fillBox env (colorOf (wState w U.! i) (wStateSz w U.! i))
          (x0 + fromIntegral c * cs) (y0 + fromIntegral r * cs) (cs - 1) (cs - 1)
      | r <- [0 .. rows - 1], c <- [0 .. cols - 1], let i = r * cols + c ]
    colorOf st sz
      | st == -1 = hsb hue 20 13
      | st == -2 = hsb 45 90 100                 -- しみとおった塊
      | st == -3 = hsb hue 70 90                 -- 上とつながった「水」
      | otherwise = hsb (hue + 120 + fromIntegral ((st * 47) `mod` 140)) 45 (40 + fromIntegral (min 50 (sz * 3)))
    flash = [ fillBox env (hsba 45 90 100 (wFlash w * 0.4)) 0 0 (envW env) (envH env) | wFlash w > 0 ]
    -- 上下の端
    edges =
      [ fillBox env (hsb hue 70 90) x0 (y0 - 8) (fc * cs) 4
      , fillBox env (hsb 45 80 90) x0 (y0 + fr * cs + 4) (fc * cs) 4 ]
    p = fromIntegral (wNOpen w) / fromIntegral (cols * rows) :: Float
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Percolation brush"
      , txt env 24 56 15 (hsb 0 0 80)
          ("Open fraction p = " ++ fmtF 3 p ++ "   Clusters " ++ show (wComps w) ++ "   "
           ++ (if wPerc w then "Percolated! (at p = " ++ fmtF 3 (wPAt w) ++ ")" else "Not connected top to bottom yet"))
      , txt env 24 80 15 (hsb 0 0 80)
          "Blue = joined to top. Gold = joined top to bottom. The test FIND(top) = FIND(bottom) is almost O(1)"
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60)
          ("Drag = paint / A = auto" ++ (if wAuto w then " (ON)" else "") ++ " / Left, Right = brush " ++ show (wBrush w)
           ++ " / S = sound " ++ (if wSound w then "ON" else "OFF") ++ " / C = clear / R = new color")
      ]

-- ---------- キー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wBrush = min 6 (wBrush w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wBrush = max 1 (wBrush w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "sS" -> pure w { wSound = not (wSound w) }
    | c `elem` "cC" -> resetWorld (wBrush w) (wHue w) (wAuto w) (wSound w)
    | c `elem` "rR" -> do
        h <- randomRIO (0, 359 :: Int)
        pure w { wHue = fromIntegral h }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. A で自動に開けて、しみとおるときの p を何回か記録しよう。0.593 に近い？
-- 2. cols, rows を変えて、p のばらつきがどう変わるか調べよう
-- 3. 色・音階・ブラシの形を変えて、自分の「しみこみ」作品にしよう（S の音、flash の光らせ方も）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左上が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx (gy - size) size c s

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 列の i 番目（なければ Nothing）
safeAt :: [a] -> Int -> Maybe a
safeAt xs i = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- 小数を桁数を指定して文字にする
fmtF :: Int -> Float -> String
fmtF n v = printf ("%." ++ show n ++ "f") v
