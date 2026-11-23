-- ============================================
-- 58-15 カメラで鏡の万華鏡 📷 ─ 鏡は法線ベクトル、像は −n v n⁻¹ の積で全部作る
-- 2〜4 枚の鏡（中心を通る直線）を法線ベクトル n で置く。鏡の積（ベルソル）を幅優先で全部集めると「群」になる
-- 鏡のなす角が 180°/k なら像は 2k 個で模様が閉じる（二面体群）。ずれると像がどこまでも増えて、模様が重なってにごる
-- 操作：ドラッグ 法線の丸 / 2 3 4 鏡の枚数 / K 角を 180°/k にそろえる / ↑↓ k / V カメラ ⇔ テスト画像 / S 保存 / R 元にもどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl', sort, transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type P2 = (Double, Double)

g2 :: Alg
g2 = makeAlgebra [1, 1]

normAng0 :: [Double]
normAng0 = [pi / 2, pi / 2 + 0.9, 0.3, 1.2]   -- ★ 鏡の法線の向き（ラジアン）

capMax :: Int
capMax = 48                      -- ★ 集める像の上限（閉じないときの打ち切り）

camW', camH', testSz, pixK :: Int
camW' = 160                      -- カメラの映像の大きさ
camH' = 120
testSz = 240                     -- テスト画像の大きさ
pixK = 5                         -- 万華鏡の絵は 1/5 の大きさで計算して 5 倍に広げる

-- 映像の 1 点 → 画面の各画素がどの像（鏡の積）のどの画素を映すかの表
--   offs：画素ごとの始まりの位置、idx：映像の画素の番号（重なる像の数だけ並ぶ）
data Lut = Lut { lKey :: ([Double], Int, Int, Int), lOffs :: !(U.Vector Int), lIdx :: !(U.Vector Int), lClosed :: !Bool, lW, lH :: !Int }

data World = World
  { wAng    :: [Double]
  , wCount  :: !Int              -- 鏡の枚数
  , wK      :: !Int              -- 180°/k の k
  , wUseCam :: !Bool
  , wCam    :: Maybe Camera
  , wLive   :: !Bool
  , wFrame  :: !B.ByteString
  , wDrag   :: !Int
  , wLut    :: !(Maybe Lut)
  , wPic    :: !Picture          -- 万華鏡の絵（テスト画像なら鏡が変わったときだけ作り直す）
  , wPicKey :: !([Double], Int, Int, Int, Bool)
  , wTest   :: [(String, Bool)] }

vio, cya, sil, gold :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)

main :: IO ()
main = runSketch (sketch "58-15 Camera kaleidoscope from mirrors" world0)
  { skBack  = rgbC 10 10 10
  , skStart = \env0' -> do
      printSelfTest selfTest
      cam <- startCamera camW' camH'
      step env0' 0 world0 { wCam = Just cam, wTest = selfTest }   -- 最初の表と絵はここで作っておく
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World normAng0 2 6 True Nothing False B.empty (-1) Nothing Blank ([], 0, 0, 0, False) []

normals :: World -> [P2]
normals w = [ (cos a, sin a) | a <- take (wCount w) (wAng w) ]

-- 鏡の法線から生まれる群を幅優先で集める（±W は同じ変換なので行列でくらべる）
--   1 つの要素 = (ベルソル V, 行列 [M0, M1, M2, M3] = [[M0, M1], [M2, M3]])
generate :: [P2] -> Int -> ([(MV, [Double])], Bool)
generate ns cap0 = go [(scalar g2 1, [1, 0, 0, 1])] [scalar g2 1]
  where
    gens = [ unit g2 (vec g2 [x, y]) | (x, y) <- ns ]
    go list queue = case queue of
      (v : rest) | length list < cap0 ->
        let (list', added, stop) = foldl' (tryGen v) (list, [], False) gens
        in if stop then (list', null (rest ++ added)) else go list' (rest ++ added)
      _ -> (list, null queue)
    tryGen v (list, added, stop) g
      | stop = (list, added, stop)
      | any (\(_, q) -> and (zipWith (\x y -> abs (x - y) < 1e-6) q m)) list = (list, added, False)
      | otherwise = let list' = list ++ [(ww, m)] in (list', added ++ [ww], length list' >= cap0)
      where ww = gp g2 g v
            e1 = applyVersor g2 ww (blade g2 1 1)
            e2 = applyVersor g2 ww (blade g2 2 1)
            m = [e1 U.! 1, e2 U.! 1, e1 U.! 2, e2 U.! 2]

-- 鏡の直線のうち、となりあう 2 本のいちばんせまいすき間（ここを映像の「種」にする）
seedWedge :: [P2] -> (Double, Double)
seedWedge ns = case ls of
  (l0 : _) -> foldl' (\best@(b0, b1) (a, b) -> if b - a < b1 - b0 then (a, b) else best) (l0, l0 + pi)
                     (zip ls (drop 1 ls ++ [l0 + pi]))
  [] -> (0, pi)
  where
    raw = sort [ jsMod (jsMod (atan2 y x + pi / 2) pi + pi) pi | (x, y) <- ns ]
    dedup = [ a | (i, a) <- zip [0 :: Int ..] raw, i == 0 || a - getAt 0 raw (i - 1) > 1e-6 ]   -- 同じ直線に重なった鏡は 1 本として数える
    ls = case dedup of
      (l0 : _ : _) | l0 + pi - last dedup < 1e-6 -> init dedup
      _ -> dedup

-- 表を作る：画面の画素 p（数学の座標）を各要素の行列で逆に戻し（M⁻¹ = Mᵀ）、種のすき間に入れば映像のその点を使う
buildLut :: Int -> Int -> Int -> Int -> [Double] -> Int -> Lut
buildLut sw sh scrW scrH key cnt = Lut (take cnt key, cnt, sw, sh) offs idx closed iw ih
  where
    ns = [ (cos a, sin a) | a <- take cnt key ]
    (els, closed) = generate ns capMax
    (w0, w1) = seedWedge ns
    (d0x, d0y) = (cos w0, sin w0)
    (d1x, d1y) = (cos w1, sin w1)
    mats = U.fromList (concatMap snd els)
    ne = length els
    iw = (scrW + pixK - 1) `div` pixK
    ih = (scrH + pixK - 1) `div` pixK
    rad = hypot (fromIntegral scrW) (fromIntegral scrH) / 2
    sc = max (2 * rad / fromIntegral sw) (2 * rad / fromIntegral sh)   -- 画面のはしまで映像でうめる
    npix = iw * ih
    -- (画素の番号, 映像の画素の番号) を画素の順・要素の順に全部並べる
    allHits = U.unfoldr nextHit (0, 0)
    nextHit (!i, !e)
      | i >= npix = Nothing
      | e >= ne = nextHit (i + 1, 0)
      | otherwise =
          let (y, x) = i `divMod` iw
              px = (fromIntegral x + 0.5) * fromIntegral pixK - fromIntegral scrW / 2
              py = fromIntegral scrH / 2 - (fromIntegral y + 0.5) * fromIntegral pixK
              m0 = U.unsafeIndex mats (4 * e); m1 = U.unsafeIndex mats (4 * e + 1)
              m2 = U.unsafeIndex mats (4 * e + 2); m3 = U.unsafeIndex mats (4 * e + 3)
              qx = m0 * px + m2 * py
              qy = m1 * px + m3 * py
              inside = d0x * qy - d0y * qx >= 0 && qx * d1y - qy * d1x >= 0
          in if inside
               then let ix = clampI 0 (sw - 1) (floor ((qx + fromIntegral sw * sc / 2) / sc))
                        iy = clampI 0 (sh - 1) (floor ((-qy + fromIntegral sh * sc / 2) / sc))
                    in Just ((i, iy * sw + ix), (i, e + 1))
               else nextHit (i, e + 1)
    counts = U.accumulate (+) (U.replicate npix 0) (U.map (\(i, _) -> (i, 1 :: Int)) allHits)
    offs = U.scanl' (+) 0 counts
    idx = U.map snd allHits

clampI :: Int -> Int -> Int -> Int
clampI a b v = max a (min b v)

-- 表と映像から絵を作る（閉じていれば最後の像、閉じていなければ 0.45 ずつ重ねる）
render :: Lut -> (Int -> (Double, Double, Double)) -> Picture
render lut src = pixelImage (lW lut) (lH lut) (fromIntegral pixK) px
  where
    px x y =
      let i = y * lW lut + x
          a = lOffs lut U.! i
          b = lOffs lut U.! (i + 1)
          (r, g, bl) | lClosed lut = if b > a then src (lIdx lut U.! (b - 1)) else (10, 10, 10)
                     | otherwise = foldl' (\(cr, cg, cb) k -> let (sr, sg, sb) = src (lIdx lut U.! k) in (cr * 0.55 + sr * 0.45, cg * 0.55 + sg * 0.45, cb * 0.55 + sb * 0.45)) (10, 10, 10) [a .. b - 1]
      in (toByte r, toByte g, toByte bl)

toByte :: Double -> Word8
toByte v = fromIntegral (max 0 (min 255 (floor (fin 0 v + 0.5) :: Int)))

-- カメラがないとき用：色の帯と円と「GA」の文字（RGB を 3 つずつ並べた配列）
testImage :: U.Vector Double
testImage = U.concatMap (\i -> let (r, g, b) = px (i `mod` testSz) (i `div` testSz) in U.fromList [r, g, b]) (U.enumFromN 0 (testSz * testSz))
  where
    sz = fromIntegral testSz :: Double
    px x y =
      let fx = fromIntegral x + 0.5
          fy = fromIntegral y + 0.5
          stripe = hsbRgb (fromIntegral ((floor (fy / sz * 24) :: Int) * 15 `mod` 360)) 70 95
          circ = [ hsbRgb (fromIntegral ((i * 40 + 180) `mod` 360)) 60 100 | i <- [0 .. 8 :: Int]
                 , let cx = sz * (0.15 + 0.1 * fromIntegral i); cy = sz * (0.5 + 0.3 * sin (fromIntegral i))
                 , hypot (fx - cx) (fy - cy) < sz * 0.06 ]
          base = if null circ then stripe else last circ
      in if inGA ((fx - sz * 0.62) / (sz * 0.025)) ((fy - sz * 0.4) / (sz * 0.025)) then (255, 255, 255) else base
    -- 5 × 7 のドットで「GA」（中心が原点、1 ドット = 1）
    inGA u v = let col = floor (u + 5.5) :: Int; row = floor (v + 3.5) :: Int
               in row >= 0 && row < 7 && col >= 0 && col < 11 && col /= 5 && getAt ' ' (getAt "" (if col < 5 then glyphG else glyphA) row) (col `mod` 6) == '#'
    glyphG = [" ### ", "#   #", "#    ", "# ###", "#   #", "#   #", " ### "]
    glyphA = [" ### ", "#   #", "#   #", "#####", "#   #", "#   #", "#   #"]

-- HSB（0〜360, 0〜100, 0〜100）→ RGB（0〜255）
hsbRgb :: Double -> Double -> Double -> (Double, Double, Double)
hsbRgb h s b = (255 * (m + r1), 255 * (m + g1), 255 * (m + b1))
  where v = b / 100
        c = v * s / 100
        hp = h / 60
        x = c * (1 - abs (jsMod hp 2 - 1))
        m = v - c
        (r1, g1, b1) | hp < 1 = (c, x, 0)
                     | hp < 2 = (x, c, 0)
                     | hp < 3 = (0, c, x)
                     | hp < 4 = (0, x, c)
                     | hp < 5 = (x, 0, c)
                     | otherwise = (c, 0, x)

step :: Env -> Float -> World -> IO World
step env _ w = do
  (live, fr) <- case wCam w of
    Just cam | wUseCam w -> do
      ok <- camLive cam
      if not ok then pure (False, B.empty) else do
        f <- cameraFrame cam
        pure (if B.length f >= camW' * camH' * 3 then (True, f) else (False, B.empty))
    _ -> pure (False, B.empty)
  let (sw, sh) = if live then (camW', camH') else (testSz, testSz)
      scrW = round (envW env)
      scrH = round (envH env)
      key = (take (wCount w) (wAng w), wCount w, sw, sh)
      lut = case wLut w of
        Just l | lKey l == key && lW l == (scrW + pixK - 1) `div` pixK && lH l == (scrH + pixK - 1) `div` pixK -> l
        _ -> buildLut sw sh scrW scrH (wAng w) (wCount w)
      g k = fromIntegral (BU.unsafeIndex fr k) :: Double
      src | live = \k -> (g (3 * k), g (3 * k + 1), g (3 * k + 2))
          | otherwise = \k -> (testImage U.! (3 * k), testImage U.! (3 * k + 1), testImage U.! (3 * k + 2))
      picKey = (take (wCount w) (wAng w), wCount w, sw, sh, live)
      pic | live || picKey /= wPicKey w = render lut src
          | otherwise = wPic w
  lut `seq` pic `seq` pure w { wLive = live, wFrame = fr, wLut = Just lut, wPic = pic, wPicKey = picKey }

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (f2 (ww / 2), f2 (hh / 2)) (Scale 1 (-1) (wPic w)) ]
  -- 鏡の直線と法線（ドラッグできる丸）
  ++ concat [ let col = if odd i then cya else vio
              in [ Color (colA col 220) (lineS 2.5 (cx + ny * rad, cy + nx * rad) (cx - ny * rad, cy - nx * rad))
                 , arrowS (col3 col) 2 (cx, cy) (cx + nx * hr, cy - ny * hr)
                 , Color (col3 col) (discS (cx + nx * hr, cy - ny * hr) 10) ]
            | (i, (nx, ny)) <- zip [0 :: Int ..] ns ]
  -- 文字（背景を少し暗く）
  ++ [ Color (rgbaC 0 0 0 170) (rectTL 0 0 ww 128), Color (rgbaC 0 0 0 170) (rectTL 0 (hh - 34) ww 34)
     , say ("drag: normal dots   2 3 4: mirrors (" ++ show (wCount w) ++ ")   K: set 180/k deg (Up/Down k = " ++ show (wK w) ++ ")   V: " ++ (if wLive w then "camera" else "test image") ++ "   S: save   R: reset") 16 28 17 (col3 sil)
     , say ("mirror 1 normal n = " ++ str g2 n1 3 ++ ", mirror 2 normal m = " ++ str g2 n2 3 ++ ", angle alpha = " ++ fmt 2 (abs al * 180 / pi) ++ " deg") 16 54 15 (col3 vio)
     , say ("2 reflections = rotor R = mn = " ++ str g2 rotor 3 ++ "  -> a rotation by 2 alpha = " ++ fmt 2 (abs (2 * al) * 180 / pi) ++ " deg") 16 78 15 (col3 cya)
     , say (if closed
              then "closed! " ++ show nAll ++ " images (" ++ show nRef ++ " reflections + " ++ show (nAll - nRef) ++ " rotations) = dihedral group D" ++ show (nAll `div` 2)
              else "not closed: stopped at " ++ show capMax ++ " images (the see-through images blur) -> K aligns the angles") 16 102 15 (col3 (if closed then gold else (255, 120, 120))) ]
  ++ [ say (if wUseCam w then "waiting for the camera (V for the test image)" else "showing the test image") 16 (hh - 12) 14 (grayC 170) | not (wLive w) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    ww = realToFrac (envW env) :: Double
    hh = realToFrac (envH env) :: Double
    cx = ww / 2
    cy = hh / 2
    rad = hypot ww hh / 2
    hr = min ww hh * 0.22
    ns = normals w
    (els, closed) = generate ns capMax
    nAll = length els
    nRef = length (filter (isOdd g2 . fst) els)
    (a0x, a0y) = getAt (1, 0) ns 0
    (a1x, a1y) = getAt (1, 0) ns 1
    n1 = vec g2 [a0x, a0y]
    n2 = vec g2 [a1x, a1y]
    rotor = gp g2 n2 n1
    al = atan2 (a0x * a1y - a0y * a1x) (a0x * a1x + a0y * a1y)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        hr = min (realToFrac (envW env)) (realToFrac (envH env)) * 0.22
        pick (best, k) (i, (nx, ny)) = let d = hypot (mx - (realToFrac (envW env) / 2 + nx * hr)) (my - (realToFrac (envH env) / 2 - ny * hr)) in if d < best then (d, i) else (best, k)
    in pure w { wDrag = snd (foldl' pick (30, -1) (zip [0 ..] (normals w))) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (mx, my) = evP env pos
        a = atan2 (realToFrac (envH env) / 2 - my) (mx - realToFrac (envW env) / 2)
    in pure w { wAng = [ if i == wDrag w then a else x | (i, x) <- zip [0 ..] (wAng w) ] }
  EventKey (Char c) Down _ _
    | c >= '2' && c <= '4' -> pure w { wCount = fromEnum c - fromEnum '0' }
    | c `elem` "kK" -> pure (snapK w)
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "ga_kaleidoscope" env (draw env w) >> pure w
    | c `elem` "rR" -> pure w { wAng = normAng0, wCount = 2, wK = 6 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (snapK w { wK = min 12 (wK w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (snapK w { wK = max 2 (wK w - 1) })
  _ -> pure w
  where
    snapK w' = case wAng w' of
      (a0 : _) -> w' { wAng = a0 : [ a0 + (fromIntegral i * pi) / fromIntegral (wK w') | i <- [1 .. 3 :: Int] ] }
      [] -> w'

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 15 $ do
  rs <- mapM (\_ -> do
          n <- unit g2 <$> randVec g2
          v <- randVec g2
          let [nx, ny] = vecOf g2 n
              vv@[vx, vy] = vecOf g2 v
              d = nx * vx + ny * vy
              ok1 = nearArr (vecOf g2 (reflect g2 n v)) [vx - 2 * d * nx, vy - 2 * d * ny] 1e-12
          m <- unit g2 <$> randVec g2
          let [mx, my] = vecOf g2 m
              al = atan2 (nx * my - ny * mx) (nx * mx + ny * my)
              ok2 = nearArr (vecOf g2 (applyVersor g2 (gp g2 m n) v)) (matVec (rotMat2 (2 * al)) vv) 1e-12
          pure [ok1, ok2]) [1 .. 40 :: Int]
  let col k = all (\x -> getAt False x k) rs
      okK = and [ closed && length els == 2 * k && length (filter (isOdd g2 . fst) els) == k
                | k <- [2 .. 9 :: Int], let (els, closed) = generate [(1, 0), (cos (pi / fromIntegral k), sin (pi / fromIntegral k))] 100 ]
      (g3els, g3closed) = generate [(1, 0), (cos (pi / 3), sin (pi / 3)), (cos ((2 * pi) / 3), sin ((2 * pi) / 3))] 100
      ru = gp g2 (unit g2 (vec g2 [1, 0])) (unit g2 (vec g2 [0.3, 0.8]))
      detM ms = case ms of
        [a, b, c, d] -> a * d - b * c
        _ -> 0
  pure
    [ ("reflection -n v n^-1 = Householder formula v - 2(v.n)n", col 0)
    , ("product mn of 2 mirrors acts as the rotation matrix of 2 alpha", col 1)
    , ("alpha = 180/k deg (k = 2-9) gives 2k images, k of them reflections (dihedral group D_k)", okK)
    , ("alpha = 1 rad stops at " ++ show capMax ++ " images (not closed)", not (snd (generate [(1, 0), (cos 1, sin 1)] capMax)))
    , ("3 mirrors (60 deg apart) also give the group D3 with 6 elements", g3closed && length g3els == 6)
    , ("determinant -1 for reflections, +1 for rotations (orientation flips = versor is odd)", all (\(v, ms) -> near (detM ms) (if isOdd g2 v then -1 else 1) 1e-12) g3els)
    , ("with unit normals R = mn has R R~ = 1", near (gp g2 ru (rev g2 ru) U.! 0) 1 1e-12) ]

-- ---------- やってみよう ----------
-- 1. K を押して模様が閉じたら、↑↓ で k を変えよう。像の数は？
-- 2. 法線の丸を少しだけずらして、模様が閉じなくなる様子を見よう。画面の像の数は？
-- 3. 3 を押して鏡を 3 枚に。K でそろえると 3 枚でも同じ群になる？
-- 4. 2α が 360° の整数分の 1 でないとき、回転 R を何回くり返しても元にもどらない。なぜ？
-- 5. capMax を大きくすると何が起きる？（重くなるので 200 くらいまで）

-- ==================================================
-- ここから下は「幾何代数の道具」（中身は読まなくても OK。消さないでね）
--   マルチベクトル = 2ⁿ 個の Double の配列（unboxed vector）。基底ブレードは「ビットマスク」で番号をつける
--   例（n = 3）：0 = 1、1 = e1、2 = e2、3 = e12、4 = e3、5 = e13、6 = e23、7 = e123（e31 = −e13）
--   幾何積：ブレード a と b を並べかえる回数（入れかえ 1 回で −1）× 同じ e_i が 2 つ並んだら e_i² を掛ける
--   計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
type MV = U.Vector Double

-- 代数：次元 n、基底の数 2ⁿ、各 e_i の 2 乗、次数、掛け算表（符号）、基底の名前
data Alg = Alg
  { aN      :: !Int
  , aNb     :: !Int
  , aMetric :: [Double]
  , aGrade  :: !(U.Vector Int)
  , aTbl    :: !(U.Vector Double)
  , aNames  :: [String] }

-- 基底 a のうしろに基底 b を並べ、標準の順（番号の小さい順）にするまでの入れかえ回数 → 奇数なら −1
reorderSign :: Int -> Int -> Double
reorderSign a0 b = go (a0 `shiftR` 1) 0
  where go 0 s = if odd s then -1 else 1
        go a s = go (a `shiftR` 1) (s + popCount (a .&. b))

-- 代数をつくる：metric = 各 e_i の 2 乗（+1 か −1）。[1,1,1] = G(3)、[-1,-1] = Cl(0,2)、[1,-1,-1,-1] = 時空の代数
makeAlgebra :: [Double] -> Alg
makeAlgebra metric = Alg n nb metric grade tbl names
  where
    n = length metric
    nb = 1 `shiftL` n
    grade = U.generate nb popCount
    tbl = U.generate (nb * nb) $ \k ->
      let (a, b) = k `divMod` nb
          c = a .&. b
      in foldl' (\s (i, m) -> if testBit c i then s * m else s) (reorderSign a b) (zip [0 ..] metric)   -- 同じ e_i どうし → e_i² = ±1
    names = [ if a == 0 then "1" else 'e' : concat [ show (i + 1) | i <- [0 .. n - 1], testBit a i ] | a <- [0 .. nb - 1] ]

-- 基底の名前を取りかえる
withNames :: [String] -> Alg -> Alg
withNames ns al = al { aNames = ns }

nameOf :: Alg -> Int -> String
nameOf al i = getAt "?" (aNames al) i

zeroM :: Alg -> MV
zeroM al = U.replicate (aNb al) 0

scalar :: Alg -> Double -> MV
scalar al s = zeroM al U.// [(0, s)]

-- 基底ブレード（係数 c）
blade :: Alg -> Int -> Double -> MV
blade al mask c = zeroM al U.// [(mask, c)]

-- ベクトル（足りない成分は 0）
vec :: Alg -> [Double] -> MV
vec al vs = zeroM al U.// [ (1 `shiftL` i, v) | (i, v) <- zip [0 .. aN al - 1] vs ]

vecOf :: Alg -> MV -> [Double]
vecOf al m = [ m U.! (1 `shiftL` i) | i <- [0 .. aN al - 1] ]

addM, subM :: MV -> MV -> MV
addM = U.zipWith (+)
subM = U.zipWith (-)

-- いくつも足す
addMs :: Alg -> [MV] -> MV
addMs al = foldl' addM (zeroM al)

smul :: MV -> Double -> MV
smul a s = U.map (* s) a

-- 積の共通部分：kind 0 = 幾何積、1 = 外積 ∧（共通の e_i がないときだけ）、2 = 左縮約 ⌋（a のブレードが b に含まれるときだけ）
prodK :: Alg -> Int -> MV -> MV -> MV
prodK al kind a b = U.accum (+) (zeroM al)
  [ (i `xor` j, U.unsafeIndex tbl (i * nb + j) * x * y)
  | i <- [0 .. nb - 1], let x = U.unsafeIndex a i, x /= 0
  , j <- [0 .. nb - 1], let y = U.unsafeIndex b j, y /= 0
  , kind /= 1 || i .&. j == 0
  , kind /= 2 || i .&. j == i ]
  where nb = aNb al
        tbl = aTbl al

gp, wedge, lc :: Alg -> MV -> MV -> MV
gp al = prodK al 0
wedge al = prodK al 1
lc al = prodK al 2

-- 3 つ以上の積（左から順に）
gps, wedges :: Alg -> [MV] -> MV
gps al ms = case ms of
  (m : rest) -> foldl' (gp al) m rest
  [] -> scalar al 1
wedges al ms = case ms of
  (m : rest) -> foldl' (wedge al) m rest
  [] -> scalar al 1

-- ベクトルどうしなら a·b
inner :: Alg -> MV -> MV -> MV
inner al a b = gradePart al (gp al a b) 0

-- スカラー部分
sp :: Alg -> MV -> MV -> Double
sp al a b = gp al a b U.! 0

gradePart :: Alg -> MV -> Int -> MV
gradePart al a k = U.imap (\i x -> if aGrade al U.! i == k then x else 0) a

gradeSize :: Alg -> MV -> Int -> Double
gradeSize al a k = sqrt (U.ifoldl' (\s i x -> if aGrade al U.! i == k then s + x * x else s) 0 a)

-- 逆順 ã（掛ける順を逆に）
rev :: Alg -> MV -> MV
rev al = U.imap (\i x -> let g = aGrade al U.! i in if odd ((g * (g - 1)) `div` 2) then -x else x)

-- 次数の反転（奇数次に −）
ginv :: Alg -> MV -> MV
ginv al = U.imap (\i x -> if odd (aGrade al U.! i) then -x else x)

normSq, norm :: Alg -> MV -> Double
normSq al a = gp al a (rev al a) U.! 0
norm al a = sqrt (abs (normSq al a))

unit :: Alg -> MV -> MV
unit al a = let n = norm al a in smul a (1 / (if n == 0 || isNaN n then 1 else n))

-- 逆元（ベクトル・ブレード・ベルソル用：a ã がスカラー）
inv :: Alg -> MV -> MV
inv al a = let r = rev al a in smul r (1 / (gp al a r U.! 0))

-- 擬スカラー（一番上のブレード）
pss :: Alg -> MV
pss al = blade al (aNb al - 1) 1

-- 双対 a* = a I⁻¹
dualOf, undual :: Alg -> MV -> MV
dualOf al a = gp al a (inv al (pss al))
undual al a = gp al a (pss al)

-- 交わり（向きと大きさは決めかた次第）
meet :: Alg -> MV -> MV -> MV
meet al a b = undual al (wedge al (dualOf al a) (dualOf al b))

-- R x R̃（回転）
sandwich :: Alg -> MV -> MV -> MV
sandwich al r x = gps al [r, x, rev al r]

-- −n x n⁻¹（法線 n の鏡で反射）
reflect :: Alg -> MV -> MV -> MV
reflect al nv x = smul (gps al [nv, x, inv al nv]) (-1)

isOdd :: Alg -> MV -> Bool
isOdd al v = o > e
  where (o, e) = U.ifoldl' (\(so, se) i x -> if odd (aGrade al U.! i) then (so + x * x, se) else (so, se + x * x)) (0, 0) v

-- ±V x V⁻¹（鏡 k 枚の積なら符号 (−1)^k）
applyVersor :: Alg -> MV -> MV -> MV
applyVersor al v x = smul (gps al [v, x, inv al v]) (if isOdd al v then -1 else 1)

-- 二重ベクトル B の指数関数 e^B：B² がスカラーなら閉じた式、そうでなければ級数（割って 2 乗をくり返す）
expB :: Alg -> MV -> Bool -> MV
expB al b forceSeries
  | not forceSeries && rest < 1e-12 =
      let s = U.head b2
          (c, k) | s < -1e-15 = let t = sqrt (-s) in (cos t, sin t / t)
                 | s > 1e-15 = let t = sqrt s in (cosh t, sinh t / t)
                 | otherwise = (1, 1)
          r = smul b k
      in r U.// [(0, U.head r + c)]
  | otherwise =
      let sz0 = U.foldl' (\m x -> max m (abs x)) 0 b
          halve (sz, q) = if sz > 0.25 && q < 40 then halve (sz / 2, q + 1) else q
          sq = halve (sz0, 0 :: Int)
          x = smul b (1 / (2 ^^ sq))
          series = snd (foldl' (\(term, sm) k -> let t' = smul (gp al term x) (1 / fromIntegral k) in (t', addM sm t')) (scalar al 1, scalar al 1) [1 .. 29 :: Int])
      in iterate (\m -> gp al m m) series !! sq
  where
    b2 = gp al b b
    rest = U.sum (U.map abs (U.drop 1 b2))

near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 2 つのマルチベクトルが同じか（tol の中）
nearM :: MV -> MV -> Double -> Bool
nearM a b tol = U.length a == U.length b && U.and (U.zipWith (\x y -> abs (x - y) <= tol) a b)

isZeroM :: MV -> Double -> Bool
isZeroM a tol = U.all (\x -> abs x <= tol) a

-- ランダムなマルチベクトル・ベクトル（成分は −1〜1）
randM, randVec :: Alg -> R MV
randM al = U.fromList <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aNb al]
randVec al = vec al <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aN al]

-- 文字にする：「1.20 + 0.50 e12」（記号は ASCII）
str :: Alg -> MV -> Int -> String
str al a d = if null s then "0" else s
  where
    s = foldl' piece "" (zip [0 ..] (U.toList a))
    piece acc (i, x)
      | abs x < 10 ^^ negate d / 2 = acc
      | otherwise =
          let v = fmt d (abs x)
              sg = if null acc then (if x < 0 then "-" else "") else (if x < 0 then " - " else " + ")
              body = if i > 0 then (if v == fmt d 1 then "" else v ++ " ") ++ nameOf al i else v
          in acc ++ sg ++ body

-- ---- 比べるための「ふつうの」計算（行列・外積・行列式）。ベクトルは [x, y, z] のリスト
dotv :: [Double] -> [Double] -> Double
dotv a b = foldl' (+) 0 (zipWith (*) a b)

crossv :: [Double] -> [Double] -> [Double]
crossv a b = [a1 * b2 - a2 * b1, a2 * b0 - a0 * b2, a0 * b1 - a1 * b0]
  where [a0, a1, a2] = take 3 (a ++ repeat 0)
        [b0, b1, b2] = take 3 (b ++ repeat 0)

lenv :: [Double] -> Double
lenv a = sqrt (dotv a a)

det3 :: [Double] -> [Double] -> [Double] -> Double
det3 a b c = dotv a (crossv b c)

matVec :: [[Double]] -> [Double] -> [Double]
matVec m v = map (\r -> foldl' (+) 0 (zipWith (*) r v)) m

matMul :: [[Double]] -> [[Double]] -> [[Double]]
matMul p q = [ [ foldl' (+) 0 (zipWith (*) r col) | col <- transpose q ] | r <- p ]

rotMat2 :: Double -> [[Double]]
rotMat2 t = [[cos t, -(sin t)], [sin t, cos t]]

-- 軸 ax（単位ベクトル）まわりに角 t 回す行列
rodrigues :: [Double] -> Double -> [[Double]]
rodrigues ax t =
  [ [c + x * x * k, x * y * k - z * s, x * z * k + y * s]
  , [y * x * k + z * s, c + y * y * k, y * z * k - x * s]
  , [z * x * k - y * s, z * y * k + x * s, c + z * z * k] ]
  where [x, y, z] = take 3 (ax ++ repeat 0)
        c = cos t
        s = sin t
        k = 1 - c

nearArr :: [Double] -> [Double] -> Double -> Bool
nearArr a b tol = length a == length b && and (zipWith (\x y -> abs (x - y) <= tol) a b)

-- ---- 種つきの乱数（自己テストで毎回同じ数を使う。p5.js 版の rnd と同じ mulberry32）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

nextRng :: Rng -> (Double, Rng)
nextRng (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に使う計算（状態を手で受けわたさなくてよいように）
newtype R a = R (Rng -> (a, Rng))

instance Functor R where fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where R g >>= k = R (\s -> let (a, s1) = g s; R h = k a in h s1)

nextR :: R Double
nextR = R nextRng

runRng :: R a -> Rng -> (a, Rng)
runRng (R g) = g

evalRng :: Word32 -> R a -> a
evalRng sd m = fst (runRng m (seedRng sd))

-- ---- 数の道具
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の % と同じ（符号は割られる数と同じ）
jsMod :: Double -> Double -> Double
jsMod a m = a - m * fromIntegral (truncate (a / m) :: Integer)

-- Math.round と同じ（.5 は大きい方へ）
jsRound :: Double -> Double
jsRound v = if isNaN v || isInfinite v then 0 else fromIntegral (floor (v + 0.5) :: Integer)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . clampv (-1e6) 1e6 . fin 0

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 1.2e-5 のような書き方（toExponential(1)）
fmtE :: Double -> String
fmtE v = showEFloat (Just 1) (fin 0 v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
mouseP :: Env -> (Double, Double)
mouseP env = let (mx, my) = envMouse env in (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

-- gloss のイベントの位置 → p5.js の座標
evP :: Env -> (Float, Float) -> (Double, Double)
evP env (mx, my) = (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- [r, g, b] の色（透明度 a）
colA :: (Int, Int, Int) -> Int -> Color
colA (r, g, b) a = rgbaC r g b a

col3 :: (Int, Int, Int) -> Color
col3 (r, g, b) = rgbC r g b

-- 左下が (x, y) の文字
say :: String -> Double -> Double -> Float -> Color -> Picture
say s x y size c = Translate (f2 x) (f2 y) (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Double -> Double -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Double
textW s size = fromIntegral (length s) * realToFrac size * 0.72

pt2 :: (Double, Double) -> (Float, Float)
pt2 (x, y) = (f2 x, f2 y)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Double -> Double -> Double -> Double -> Picture
rectTL x y w h = Polygon [pt2 (x, y), pt2 (x + w, y), pt2 (x + w, y + h), pt2 (x, y + h)]

-- 線（太さ wt）
lineS :: Float -> (Double, Double) -> (Double, Double) -> Picture
lineS wt a b = if wt <= 1.2 then Line [pt2 a, pt2 b] else thickLine wt (pt2 a) (pt2 b)

-- 点線（dash：線の長さ、gap：すき間）
dashed :: Float -> Double -> Double -> (Double, Double) -> (Double, Double) -> Picture
dashed wt dash gap (x1, y1) (x2, y2) = Pictures
  [ lineS wt (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + dash), y1 + uy * min l (s + dash)) | s <- takeWhile (< l) [0, dash + gap ..] ]
  where l = min 5000 (fin 0 (hypot (x2 - x1) (y2 - y1)))
        ux = if l > 0 then (x2 - x1) / l else 0
        uy = if l > 0 then (y2 - y1) / l else 0

-- 点が 2 つ以上あるときだけ折れ線を描く（closed なら閉じる）
polyline :: Float -> Bool -> [(Double, Double)] -> Picture
polyline wt closed pts = case pts of
  (p0 : _ : _) -> let ps = map pt2 (if closed then pts ++ [p0] else pts) in if wt <= 1.2 then Line ps else thickPath wt ps
  _ -> Blank

-- 閉じた形を塗る（へこんだ形でも大丈夫なように、重心からの三角形で塗る）
fillPoly :: [(Double, Double)] -> Picture
fillPoly pts = case pts of
  (_ : _ : _ : _) -> Pictures [ Polygon [pt2 c, pt2 a, pt2 b] | (a, b) <- zip pts (drop 1 pts ++ take 1 pts) ]
  _ -> Blank
  where n = fromIntegral (length pts)
        c = (sum (map fst pts) / n, sum (map snd pts) / n)

-- 最初の点から扇のように塗る（beginShape の扇形・星形の形）
fanFill :: [(Double, Double)] -> Picture
fanFill pts = case pts of
  (p0 : rest@(_ : _ : _)) -> Pictures [ Polygon [pt2 p0, pt2 a, pt2 b] | (a, b) <- zip rest (drop 1 rest) ]
  _ -> Blank

-- 円（中心・半径）の枠と塗り
circleS :: Float -> (Double, Double) -> Double -> Picture
circleS wt (x, y) r = Translate (f2 x) (f2 y) (if wt <= 1.2 then Circle (f2 r) else ThickCircle (f2 r) wt)

discS :: (Double, Double) -> Double -> Picture
discS (x, y) r = Translate (f2 x) (f2 y) (circleSolid (f2 r))

-- 矢印（画面の座標）：線の太さ sw
arrowS :: Color -> Float -> (Double, Double) -> (Double, Double) -> Picture
arrowS c sw (x1, y1) (x2, y2) = Color c $ Pictures $
  lineS sw (x1, y1) (x2, y2) :
  [ Polygon [pt2 (x2, y2), pt2 (x2 - h * cos (a - 0.4), y2 - h * sin (a - 0.4)), pt2 (x2 - h * cos (a + 0.4), y2 - h * sin (a + 0.4))]
  | hypot (x2 - x1) (y2 - y1) >= 2 ]
  where a = atan2 (y2 - y1) (x2 - x1)
        h = 8 + realToFrac sw * 2

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
drawSelfTest env rs = say (selfTestLine rs) (realToFrac (envW env) - 200) (realToFrac (envH env) - 12) 14 (if all snd rs then rgbC 150 220 200 else rgbC 255 100 110)
