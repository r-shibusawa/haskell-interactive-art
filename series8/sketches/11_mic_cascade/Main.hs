-- ============================================
-- 11 声で連鎖を起こす（しきい値モデル）
-- 各点は「隣の○割以上が活性化したら自分も活性化」する。声の大きさで中心の点を押すと、連鎖が走る
-- 噂・流行・バズ・停電の連鎖、全部このモデル。どこまで広がるかはネットワークの形しだい
-- クリックでマイク ON / マウス左右 = しきい値 / R で新しいネットワーク・リセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wNodes    :: !(V.Vector Point)
  , wAdj      :: !(IM.IntMap IS.IntSet)
  , wEdges    :: ![(Int, Int)]
  , wActive   :: !(U.Vector Bool)
  , wVol      :: !Float
  , wCooldown :: !Int
  , wMic      :: !(Maybe Mic)
  , wLive     :: !Bool            -- 本物のマイクから音が来ているか
  }

n, nearCount :: Int
n = 100                    -- ★ 点の数
nearCount = 4              -- ★ 近い何点とつなぐか

main :: IO ()
main = runSketch (sketch "08-11 Mic Cascade" (World V.empty IM.empty [] (U.replicate n False) 0 0 Nothing False))
  { skBack  = hsb 220 40 10
  , skStart = \env -> build env Nothing False
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 近い点どうしをつなぐ
build :: Env -> Maybe Mic -> Bool -> IO World
build env mic live = do
  pts <- replicateM n $ do
    x <- randomRIO (60, envW env - 60)
    y <- randomRIO (120, envH env - 40)
    pure (x, y)
  let nodes = V.fromList pts
      (adj, edges) = foldl' (connect nodes) (IM.empty, []) [0 .. n - 1]
  pure (World nodes adj (reverse edges) (U.replicate n False) 0 0 mic live)

connect :: V.Vector Point -> (IM.IntMap IS.IntSet, [(Int, Int)]) -> Int -> (IM.IntMap IS.IntSet, [(Int, Int)])
connect nodes acc i = foldl' link acc near
  where
    p = nodes V.! i
    near = take nearCount (sortOn (\j -> dist2 p (nodes V.! j)) [j | j <- [0 .. n - 1], j /= i])
    link (adj, es) j
      | IS.member j (neighbours adj i) = (adj, es)
      | otherwise = (addLink j i (addLink i j adj), (i, j) : es)
    addLink a b = IM.insertWith IS.union a (IS.singleton b)

neighbours :: IM.IntMap IS.IntSet -> Int -> IS.IntSet
neighbours adj i = IM.findWithDefault IS.empty i adj

thresholdOf :: Env -> Float
thresholdOf env = clampF 0.1 0.8 (remap (fst (mouseXY env)) 0 (envW env) 0.1 0.8)

center :: Env -> Point
center env = (envW env / 2, envH env / 2 + 40)

-- ★ 声が大きいとき、マイクの近く（画面中央）の点を活性化する
pushByVoice :: Env -> Float -> World -> World
pushByVoice env vol w
  | vol > 0.06 && wCooldown w <= 0 =
      let r = clampF 40 200 (remap vol 0.06 0.3 40 200)
          hits = [(i, True) | (i, p) <- zip [0 ..] (V.toList (wNodes w)), dist2 p (center env) < r]
      in w { wActive = wActive w U.// hits, wCooldown = 20 }
  | otherwise = w

-- ★ 連鎖：隣の活性化の割合がしきい値を超えたら自分も活性化
cascade :: Float -> World -> World
cascade threshold w = w { wActive = U.imap next act }
  where
    act = wActive w
    next i on
      | on || IS.null nb = on
      | otherwise = fromIntegral count / fromIntegral (IS.size nb) >= threshold
      where
        nb = neighbours (wAdj w) i
        count = length (filter (act U.!) (IS.toList nb))

-- ゆっくり元に戻る（連鎖が止まったら）
relax :: U.Vector Bool -> IO (U.Vector Bool)
relax act = U.fromList <$> mapM fade (U.toList act)
  where
    fade False = pure False
    fade True = (>= (0.15 :: Float)) <$> randomRIO (0, 1)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  (level, live) <- case wMic w of
    Nothing -> pure (0, False)
    Just mic -> (,) <$> micLevel mic <*> micLive mic
  let vol = lerpF (wVol w) level 0.2
      w1 = pushByVoice env vol w { wVol = vol, wLive = live }
      w2 = w1 { wCooldown = max (-1) (wCooldown w1 - 1) }
      w3 = if envFrame env `mod` 10 == 0 then cascade (thresholdOf env) w2 else w2
  if envFrame env `mod` 120 == 0
    then (\a -> w3 { wActive = a }) <$> relax (wActive w3)
    else pure w3

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map edgePic (wEdges w) ++ map nodePic [0 .. V.length (wNodes w) - 1] ++ voice : hud)
  where
    act = wActive w
    pos i = let (x, y) = wNodes w V.! i in toG env x y
    edgePic (a, b)
      | act U.! a && act U.! b = Color (hsb 45 80 90) (Line [pos a, pos b])
      | otherwise = Color (hsb 0 0 35) (Line [pos a, pos b])
    nodePic i
      | act U.! i = Color (hsb 45 90 100) (at (pos i) (disc 8))
      | otherwise = Color (hsb 220 30 45) (at (pos i) (disc 5))
    -- 声の範囲
    (cx, cy) = center env
    voice = Color (hsba 0 80 100 0.6)
      (at (toG env cx cy) (ring (clampF 20 400 (remap (wVol w) 0 0.3 20 400) / 2) 2))
    active = U.length (U.filter id act)
    status = case wMic w of
      Nothing -> "Click to turn the mic on"
      Just _ | wLive w -> "Speak: center nodes fire, then cascade"
             | otherwise -> "No mic found: using a demo signal"
    hud =
      [ txt env 20 32 18 (hsb 0 0 100) status
      , txt env 20 56 15 (hsb 0 0 100)
          ("Threshold " ++ nf 2 (thresholdOf env) ++ " (mouse X)   Active " ++ show active ++ " / " ++ show n ++ "   R: reset")
      , txt env 20 78 15 (hsb 0 0 60) "Low threshold (0.2): a little voice spreads everywhere. High (0.6): no spread. How many neighbors must move?"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case wMic w of
    Nothing -> do
      mic <- startMic
      pure w { wMic = Just mic }
    Just _ -> pure w
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    w1 <- build env (wMic w) (wLive w)
    pure w1 { wVol = wVol w, wCooldown = wCooldown w }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. しきい値 0.5 で、小さな声と大きな声で連鎖の広がりを比べる（最初の人数が大事）
-- 2. R で何回か作り直す。同じしきい値でも、ネットワークの形で広がり方が変わる
-- 3. つなぐ数 nearCount を 8 にすると（密なネットワーク）、連鎖は起きやすい？

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

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
