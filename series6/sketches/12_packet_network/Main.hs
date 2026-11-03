-- ============================================
-- 12 パケット・ネットワーク（自由制作のベース）
-- メッセージを小さな「パケット」に分けて、中継点をたどって送る
-- 途中で失われたら、受信側が気づいて再送を頼む（ARQ）。インターネットの基本
-- クリックで送信 / マウス左右 = パケットが失われる確率 / 1〜3 でルートの数
-- ============================================
-- ★ のついた数字を変えて、自分のネットワークをつくろう！
module Main (main) where

import Art
import Control.Monad (foldM, forM, when)
import Data.Char (digitToInt)
import Data.List (intersperse, partition)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data Packet = Packet
  { pIndex :: !Int
  , pCh    :: !Char
  , pPath  :: ![Int]    -- たどるノードの番号
  , pHop   :: !Int      -- いま何本目の回線か
  , pT     :: !Float    -- 回線の上の位置（0〜1）
  }

-- 時刻 t に送るパケット（p5.js の setTimeout の代わり）
data Pending = Pending !Float !Int !Char

data World = World
  { wNodes     :: !(U.Vector (Float, Float))  -- ノードの位置（左上が原点の座標）
  , wRoutes    :: !Int
  , wPackets   :: ![Packet]
  , wPending   :: ![Pending]
  , wDelivered :: !(M.Map Int Char)
  , wSent      :: !Int
  , wLost      :: !Int
  , wOk        :: !Int
  }

message :: String
message = "HELLO"         -- ★ 送るメッセージ

packetSpeed, resendDelay, sendGap :: Float
packetSpeed = 0.03        -- ★ 速さ
resendDelay = 0.6         -- ★ 再送を頼むまでの時間（秒）
sendGap = 0.25            -- ★ パケットを送る間隔（秒）

main :: IO ()
main = runSketch (sketch "06-12 Packet Network" (World U.empty 2 [] [] M.empty 0 0 0))
  { skBack  = hsb 230 40 10
  , skStart = \env -> do
      ns <- buildNetwork env 2
      pure (World ns 2 [] [] M.empty 0 0 0)
  , skStep  = \env _ w -> step env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ノードの番号：0 が送信、最後が受信、そのあいだに 1 ルートあたり 3 個
buildNetwork :: Env -> Int -> IO (U.Vector (Float, Float))
buildNetwork env routes = do
  let w = envW env
      h = envH env
  middle <- forM [(r, k) | r <- [0 .. routes - 1], k <- [1, 2, 3 :: Int]] $ \(r, k) -> do
    jitter <- randomRIO (-30, 30)
    pure ( remap (fromIntegral k) 0 4 100 (w - 100)
         , h / 2 + (fromIntegral r - fromIntegral (routes - 1) / 2) * 180 + jitter )
  pure (U.fromList ([(100, h / 2)] ++ middle ++ [(w - 100, h / 2)]))

-- 回線の一覧
linksOf :: Int -> Int -> [(Int, Int)]
linksOf routes nNodes = concat
  [ [(0, b), (b, b + 1), (b + 1, b + 2), (b + 2, nNodes - 1)]
  | r <- [0 .. routes - 1], let b = 1 + r * 3 ]

lossOf :: Env -> Float
lossOf env = clampF 0 0.5 (remap (fst (mouseXY env)) 0 (envW env) 0 0.5)

step :: Env -> World -> IO World
step env w0 = do
  -- 時刻が来たパケットを送り出す
  let (due, later) = partition (\(Pending t _ _) -> t <= envTime env) (wPending w0)
  w1 <- foldM sendPacket w0 { wPending = later } due
  -- パケットを動かす
  (w2, kept) <- foldM (advance env) (w1, []) (wPackets w1)
  pure w2 { wPackets = reverse kept }

-- ★ ルートをランダムに選んで送り出す
sendPacket :: World -> Pending -> IO World
sendPacket w (Pending _ i ch) = do
  r <- randomRIO (0, wRoutes w - 1)
  let b = 1 + r * 3
      path = [0, b, b + 1, b + 2, U.length (wNodes w) - 1]
  pure w { wPackets = Packet i ch path 0 0 : wPackets w, wSent = wSent w + 1 }

advance :: Env -> (World, [Packet]) -> Packet -> IO (World, [Packet])
advance env (w, kept) p
  | pT p + packetSpeed < 1 = pure (w, p { pT = pT p + packetSpeed } : kept)
  | otherwise = do
      let p' = p { pT = 0, pHop = pHop p + 1 }
      roll <- randomRIO (0, 1)
      -- ★ 回線を通るたびに、確率 lossRate で失われる
      if roll < lossOf env
        then -- 受信側は番号の抜けに気づいて再送を頼む（少し遅れて）
          pure ( w { wLost = wLost w + 1
                   , wPending = Pending (envTime env + resendDelay) (pIndex p) (pCh p) : wPending w }
               , kept )
        else if pHop p' >= length (pPath p') - 1
          then do
            playTone (600 + fromIntegral (pIndex p) * 80) 0.15 0.15 Sine
            pure (w { wDelivered = M.insert (pIndex p) (pCh p) (wDelivered w), wOk = wOk w + 1 }, kept)
          else pure (w, p' : kept)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- ---- 回線とノード ----
    Color (hsb 0 0 35) (Pictures (catMaybes
      [ thickLine 3 <$> nodeG a <*> nodeG b | (a, b) <- linksOf (wRoutes w) nNodes ]))
  , Pictures (zipWith node [0 ..] (U.toList nodes))
  , Pictures (map drawPacket (wPackets w))
    -- ---- 受信した文字（番号順に並べる）----
  , txt env (envW env / 2 - 100) (envH env - 60) 48 (hsb 0 0 100)
      (intersperse ' ' [M.findWithDefault '_' i (wDelivered w) | i <- [0 .. length message - 1]])
  , txt env 20 40 18 (hsb 0 0 100)
      ("Click: send \"" ++ message ++ "\"  Loss " ++ nf 2 (lossOf env) ++ " (mouse X)  1-3: routes  Sent "
       ++ show (wSent w) ++ "  Lost " ++ show (wLost w) ++ "  OK " ++ show (wOk w))
  , txt env 20 68 15 (hsb 0 0 60) "Packets are numbered, so the message can be rebuilt even if some arrive late or get lost"
  ]
  where
    nodes = wNodes w
    nNodes = U.length nodes
    nodeAt i = nodes U.!? i
    nodeG i = uncurry (toG env) <$> nodeAt i
    node :: Int -> (Float, Float) -> Picture
    node i (x, y)
      | i == 0 = named x y "Send"
      | i == nNodes - 1 = named x y "Recv"
      | otherwise = Color (hsb 170 70 90) (at (toG env x y) (disc 14))
    named x y name = Pictures
      [ Color (hsb 45 70 90) (at (toG env x y) (disc 28))
      , txtC env x y 14 (hsb 0 0 10) name ]
    drawPacket p = case drop (pHop p) (pPath p) of
      (a : b : _) | Just (ax, ay) <- nodeAt a, Just (bx, by) <- nodeAt b ->
        let x = lerpF ax bx (pT p)
            y = lerpF ay by (pT p)
            hue = fromIntegral ((pIndex p * 60) `mod` 360)
        in Pictures
             [ roundBox env (x - 22) (y - 12) 44 24 5 (hsb hue 80 100)
             , txtC env x y 14 (hsb 0 0 10) (show (pIndex p) ++ ":" ++ [pCh p]) ]
      _ -> Blank

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd (mouseXY env) < 80 -> pure w
    | otherwise -> pure w
        { wDelivered = M.empty, wSent = 0, wLost = 0, wOk = 0
        , wPending = wPending w ++
            [Pending (envTime env + fromIntegral i * sendGap) i ch | (i, ch) <- zip [0 :: Int ..] message] }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> do
    let routes = digitToInt c
    ns <- buildNetwork env routes
    -- 飛んでいるパケットは捨てる（ネットワークが変わるので）
    pure w { wRoutes = routes, wNodes = ns, wPackets = [], wDelivered = M.empty }
  _ -> pure w

-- ---------- 自由制作のアイデア ----------
-- ・message を自分の言葉に。長いメッセージは何パケットになる？
-- ・ノードをクリックで「故障」させて、ルートが切れたときの動きを見る
-- ・パケットの色や形を、06 のハミング符号で守られているかどうかで変える
-- ・音：到着のたびに鳴るビープを、文字ごとの音階にする

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 文字（中央が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 角の丸い長方形（左上が x, y、角の半径 r）
roundBox :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
roundBox env x y w h r c = Color c (Polygon (concat
  [ corner (x + w - r) (y + r) (-90), corner (x + w - r) (y + h - r) 0
  , corner (x + r) (y + h - r) 90, corner (x + r) (y + r) 180 ]))
  where
    corner cx cy a0 = [ toG env (cx + r * cos a) (cy + r * sin a)
                      | i <- [0 .. 4 :: Int], let a = (a0 + 22.5 * fromIntegral i) * pi / 180 ]

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
