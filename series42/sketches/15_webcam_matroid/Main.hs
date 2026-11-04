-- ============================================
-- 42-15 カメラのマトロイド（おまけ）
-- カメラ映像の「明るい点」を台集合の要素にして、点どうしを結ぶグラフ的マトロイドの基（全域木）をリアルタイムに描く
--   距離モード：最小全域木（辺の重み = 距離）　明るさモード：最大重み全域木（辺の重み = 両端の明るさ。貪欲法で明るい辺から採用）
--   明るさを「重み」として貪欲法が毎フレーム走る。ライトや白い紙を動かすと木がついてくる（カメラの許可が必要）
-- 操作：M = 距離 ⇄ 明るさ / ←→ = 明るさのしきい値 / ↑↓ = 点の数 / V = 映像の表示 / カメラ不可時はノイズ点群
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector.Unboxed as U
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray)
import Data.List (sortOn)

vw, vh :: Int
vw = 96                  -- ★ カメラ画像の幅（小さいほど速い）
vh = 72                  -- ★ カメラ画像の高さ

minGap :: Int
minGap = 4               -- ★ 点どうしの最小間隔（カメラ画素）

waitFrames :: Int
waitFrames = 180         -- ★ これを過ぎてもカメラが来なければノイズ点群にする

-- 画面座標と明るさ
data Pt = Pt { pX, pY, pB :: !Float }

data TreeEdge = TreeEdge { tA, tB :: !Int, tW :: !Float }

data World = World
  { wCam      :: !(Maybe Camera)
  , wFrameImg :: !B.ByteString
  , wLive     :: !Bool
  , wFallback :: !Bool
  , wMaxPts   :: !Int     -- ★ 点の数の上限
  , wThr      :: !Float   -- ★ 明るさのしきい値（0〜255）
  , wByBright :: !Bool    -- False = 距離の最小全域木, True = 明るさの最大全域木
  , wShow     :: !Bool
  , wPts      :: ![Pt]
  , wTree     :: ![TreeEdge]
  }

main :: IO ()
main = runSketch (sketch "42-15 Camera matroid" world0)
  { skBack  = hsb 0 0 5
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { wCam = Just cam }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World Nothing B.empty False False 40 150 False True [] []

-- ★ 映像から明るい点を集める（3×3 の中でいちばん明るい画素だけ）
findBrightPoints :: Env -> World -> B.ByteString -> [Pt]
findBrightPoints env w f =
  let bright = U.generate (vw * vh) $ \i ->
        let g k = fromIntegral (BU.unsafeIndex f (i * 3 + k)) :: Float
        in (g 0 + g 1 + g 2) / 3
      at2 x y = bright U.! (x + y * vw)
      isMax x y b = and [ at2 (x + dx) (y + dy) <= b | dy <- [-1, 0, 1], dx <- [-1, 0, 1], dx /= 0 || dy /= 0 ]
      cands = [ (x, y, b) | y <- [1 .. vh - 2], x <- [1 .. vw - 2], let b = at2 x y, b >= wThr w, isMax x y b ]
      sorted = sortOn (\(_, _, b) -> negate b) cands
      far (ox, oy, _) (x, y, _) = abs (ox - x) + abs (oy - y) >= minGap
      pickPts acc [] = reverse acc
      pickPts acc (c : r)
        | length acc >= wMaxPts w = reverse acc
        | all (`far` c) acc = pickPts (c : acc) r
        | otherwise = pickPts acc r
      sx = envW env / fromIntegral vw
      sy = envH env / fromIntegral vh
  in [ Pt (fromIntegral x * sx) (fromIntegral y * sy) b | (x, y, b) <- pickPts [] sorted ]

-- カメラが使えないとき：ゆらゆら動くノイズ点群
noisePoints :: Env -> World -> [Pt]
noisePoints env w =
  let t = fromIntegral (envFrame env) * 0.004
  in [ Pt (noise2 (k * 10.3) t * envW env)
          (noise2 (k * 7.7 + 50) t * envH env)
          (120 + 135 * noise2 (k * 3.1 + 100) (t * 2))
     | i <- [0 .. wMaxPts w - 1], let k = fromIntegral i ]

-- ★ 貪欲法：辺を重み順に並べ、閉路ができないものだけ採用（クラスカル）
buildTree :: World -> World
buildTree w =
  let pts = zip [0 ..] (wPts w)
      n = length pts
      cand = [ TreeEdge i j (if wByBright w then (pB p + pB q) / 2 - d * 0.05 else d)
             | (i, p) <- pts, (j, q) <- pts, j > i
             , let d = dist2 (pX p, pY p) (pX q, pY q) ]
      order = sortOn (\e -> if wByBright w then negate (tW e) else tW e) cand
  in w { wTree = kruskal n tA tB order }

step :: Env -> Float -> World -> IO World
step env _ w = case wCam w of
  Just cam | even (envFrame env) -> do
    live <- camLive cam
    if live
      then do
        f <- cameraFrame cam
        pure (if B.length f >= vw * vh * 3
                then buildTree w { wPts = findBrightPoints env w f, wFrameImg = f, wLive = True, wFallback = False }
                else w)
      else do
        let fb = wFallback w || envFrame env > waitFrames
        pure (if fb then buildTree w { wPts = noisePoints env w, wLive = False, wFallback = True }
                    else w { wLive = False })
  _ -> pure w

draw :: Env -> World -> Picture
draw env w =
  let thr = wThr w
      pt = pick (Pt 0 0 0) (wPts w)
      hueOf b = clampF 0 360 (remap b thr 255 200 50)
      video = case wCam w of
        Just cam | wShow w && wLive w && not (wFallback w) ->
          Pictures [ Scale (envW env / fromIntegral vw) (envH env / fromIntegral vh) (camImage cam (wFrameImg w) 1)
                   , Color (withA 0.55 (hsb 0 0 5)) (rect 0 0 (envW env) (envH env)) ]   -- 映像を暗くして重ねる
        _ -> Blank
      -- 木の辺（明るさで色）
      edgePic e =
        let p = pt (tA e)
            q = pt (tB e)
            b = (pB p + pB q) / 2
        in seg env (clampF 0.5 8 (remap b thr 255 1.5 5)) (hsba (hueOf b) 80 100 0.9) (pX p, pY p) (pX q, pY q)
      ptPic p = dot env (max 1 (remap (pB p) thr 255 3 9)) (hsb (hueOf (pB p)) 70 100) (pX p, pY p)
      status | wFallback w = "  (no camera: noise points)"
             | wLive w = ""
             | otherwise = "  waiting for camera..."
  in Pictures $
       [video] ++ map edgePic (wTree w) ++ map ptPic (wPts w) ++
       [ txt env 20 40 16 white ("M: " ++ (if wByBright w then "brightness max tree" else "distance min tree") ++ "  <- ->: threshold " ++ show (roundI thr) ++ "  Up/Down: points " ++ show (wMaxPts w) ++ "  V: video " ++ (if wShow w then "ON" else "off"))
       , txt env 20 66 14 (gray 0.6) ("Points " ++ show (length (wPts w)) ++ "  edges " ++ show (length (wTree w)) ++ "  rank = points - 1. Greedy picks a new basis each frame.")
       , txt env 20 88 14 (gray 0.6) ("Bright points are the elements. Try moving 2 or 3 phone lights." ++ status)
       ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wThr = min 250 (wThr w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wThr = max 40 (wThr w - 10) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wMaxPts = min 80 (wMaxPts w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wMaxPts = max 5 (wMaxPts w - 5) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> w { wByBright = not (wByBright w) }
    | c `elem` "vV" -> w { wShow = not (wShow w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 部屋を暗くしてライトを 3 つ動かす：木の形が「3 本の枝」になる。ライトを近づけると枝がつなぎ変わる瞬間を見る
-- 2. 明るさモードで、明るさの順に辺を採用するのに「遠い辺」が選ばれることがある。なぜ？（独立なら採用、が貪欲法の全て）
-- 3. 点の数を 80 にして最大全域木にすると、1 フレームの辺の候補は 80×79/2 = 3160 本。何点まで 60fps で動く？
-- 4. 13 の音を足す：採用された辺の明るさを音の高さに。14 のブラシの点をカメラの点に置きかえる

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 塗りつぶしの丸（半径 r）
dot :: Env -> Float -> Color -> Point -> Picture
dot env r c p = Color c (at (sp env p) (disc r))

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }

-- 四捨五入（Math.round）
roundI :: Float -> Int
roundI x = floor (x + 0.5)

-- union-find を ST の配列で（辺が多いので速い版）。候補の辺を順に見て、閉路を作らないものだけ残す
kruskal :: Int -> (e -> Int) -> (e -> Int) -> [e] -> [e]
kruskal n ea eb es = runST $ do
  parent <- newListArray (0, max 0 (n - 1)) [0 .. max 0 (n - 1)] :: ST s (STUArray s Int Int)
  let find x = do
        p <- readArray parent x
        if p == x then pure x else do
          g <- readArray parent p
          writeArray parent x g        -- 道を縮める
          find g
      go acc [] = pure (reverse acc)
      go acc (e : r) = do
        ra <- find (ea e)
        rb <- find (eb e)
        if ra == rb then go acc r else do
          writeArray parent ra rb
          go (e : acc) r
  go [] es
