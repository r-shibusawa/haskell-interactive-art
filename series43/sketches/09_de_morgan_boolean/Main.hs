-- ============================================
-- 43-09 論理とブール代数の双対：ド・モルガン則
-- 式 E の「双対」E^d = AND と OR を入れ替えた式（0 と 1 も入れ替える）
--   ド・モルガン：¬E(A, B, C) = E^d(¬A, ¬B, ¬C)　　左の回路の出力を反転 = 右の回路（入力を全部反転）の出力
--   真理値表で全部の行を比べると一致する。双対の双対は元の式
-- 操作：入力の丸をクリック = 0/1 切替 / 1〜4 = 式 / T = 真理値表
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate, nub, sort)
import Data.Maybe (fromMaybe)

-- ★ 式：変数、否定、AND、OR
data Expr = V Char | Not Expr | And [Expr] | Or [Expr]

exprs :: [Expr]
exprs =
  [ And [V 'A', V 'B']
  , Or [And [V 'A', V 'B'], V 'C']
  , And [Or [V 'A', V 'B'], Or [Not (V 'A'), V 'C']]
  , Or [And [V 'A', Not (V 'B')], And [V 'B', V 'C'], Not (V 'C')]
  ]

type Inputs = [(Char, Int)]

data World = World
  { wNo     :: !Int
  , wInputs :: !Inputs
  , wTable  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "43-09 De Morgan duality" (World 1 [('A', 1), ('B', 0), ('C', 1)] True))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

exprOf :: Int -> Expr
exprOf k = case drop (k - 1) exprs of
  (e : _) -> e
  _ -> V 'A'

-- ★ 式の評価と双対化
evalE :: Expr -> Inputs -> Int
evalE e env = case e of
  V c -> val env c
  Not s -> 1 - evalE s env
  And ss -> if all (\s -> evalE s env == 1) ss then 1 else 0
  Or ss -> if any (\s -> evalE s env == 1) ss then 1 else 0

val :: Inputs -> Char -> Int
val env c = fromMaybe 0 (lookup c env)

dualE :: Expr -> Expr
dualE e = case e of
  V c -> V c
  Not s -> Not (dualE s)
  And ss -> Or (map dualE ss)
  Or ss -> And (map dualE ss)

-- 式を文字にする（& = AND、| = OR、~ = NOT）
toStr :: Bool -> Expr -> String
toStr top e = case e of
  V c -> [c]
  Not s -> "~" ++ toStr False s
  And ss -> wrap (intercalate " & " (map (toStr False) ss))
  Or ss -> wrap (intercalate " | " (map (toStr False) ss))
  where wrap s = if top then s else "(" ++ s ++ ")"

varsOf :: Expr -> [Char]
varsOf e = case e of
  V c -> [c]
  Not s -> varsOf s
  And ss -> concatMap varsOf ss
  Or ss -> concatMap varsOf ss

depthOf :: Expr -> Int
depthOf e = case e of
  V _ -> 0
  Not s -> 1 + depthOf s
  And ss -> 1 + maximum (0 : map depthOf ss)
  Or ss -> 1 + maximum (0 : map depthOf ss)

-- 回路の配置の結果：出力の位置と値、絵、次の行の高さ、入力の丸の位置
data Laid = Laid
  { outX :: !Float, outY :: !Float, outV :: !Int
  , laidPic :: [Picture], nextRow :: !Float, inputPos :: [(Char, Point)] }

-- ★ 回路を描く：入力を左、出力を右。invertIn = 入力を反転して与える（右側の回路）
drawCircuit :: Expr -> Inputs -> Float -> Float -> Float -> Bool -> String -> (Laid, Picture)
drawCircuit e env x0 y0 x1 invertIn title =
  let env2 = [(k, if invertIn then 1 - v else v) | (k, v) <- env]
      laid = layout e env2 (x0 + 60) (x1 - 60) (depthOf e) (y0 + 20) invertIn
      v = outV laid
      c = if v == 1 then hsb 55 90 100 else hsb 0 0 35
      pic = Pictures $
        [ txt x0 (y0 - 10) 14 (hsb 0 0 70) title ]
        ++ laidPic laid
        ++ [ seg 2 (hsb 0 0 60) (outX laid, outY laid) (x1 - 20, outY laid)
           , dot c (x1 - 20) (outY laid) 22
           , txtC (x1 - 20) (outY laid) 14 (hsb 0 0 100) (show v) ]
  in (laid, pic)

layout :: Expr -> Inputs -> Float -> Float -> Int -> Float -> Bool -> Laid
layout e env x0 x1 dMax row invertIn = case e of
  V c ->
    let v = val env c
        fillC = if v == 1 then hsb 55 90 100 else hsb 0 0 35
    in Laid (x0 + 12) row v
         [ dot fillC x0 row 24
         , txt (x0 - 40) (row + 5) 14 (hsb 0 0 100) ((if invertIn then "~" else "") ++ [c]) ]
         (row + 60)
         [(c, (x0, row)) | not invertIn]
  Not s -> gate [s] "NOT" 300
  And ss -> gate ss "AND" 10
  Or ss -> gate ss "OR" 180
  where
    gate ss name hue =
      let go (acc, r) s = let l = layout s env x0 x1 dMax r invertIn in (acc ++ [l], nextRow l)
          (kids, rowEnd) = foldl go ([], row) ss
          -- 深さの順に左から右へ並べる（信号が左から右へ流れる）
          gx = x0 + 60 + fromIntegral (depthOf e) * ((x1 - x0 - 60) / fromIntegral (max 1 dMax)) - 30
          gy = sum (map outY kids) / fromIntegral (max 1 (length kids))
          v = evalE e env
          wire k =
            let on = outV k == 1
                c = if on then hsb 55 80 100 else hsb 0 0 45
                wt = if on then 3 else 1.5
            in [ seg wt c (outX k, outY k) (gx - 30, outY k), seg wt c (gx - 30, outY k) (gx - 30, gy) ]
          -- OR は丸い形、AND と NOT は角の少し丸い形
          shape = roundBox (gx - 24) (gy - 16) 48 32 (if name == "OR" then 16 else 6)
      in Laid (gx + 24) gy v
           ( concatMap laidPic kids ++ concatMap wire kids
             ++ [ seg 1.5 (hsb 0 0 50) (gx - 30, gy) (gx - 24, gy)
                , Color (hsbA hue 70 100 (if v == 1 then 40 else 10)) (Polygon shape)
                , closed 2 (hsb hue 70 100) shape
                , txtC gx gy 14 (hsb 0 0 100) name ] )
           rowEnd
           (concatMap inputPos kids)

-- 左右の回路（クリック判定にも使う）
circuits :: Env -> World -> ((Laid, Picture), (Laid, Picture), Float)
circuits env w =
  let e = exprOf (wNo w)
      ed = dualE e
      midX = envW env / 2
      bottom = if wTable w then envH env * 0.55 else envH env - 40
  in ( drawCircuit e (wInputs w) 40 120 (midX - 40) False ("E = " ++ toStr True e ++ "  (left: as is)")
     , drawCircuit ed (wInputs w) (midX + 40) 120 (envW env - 40) True ("E^d = " ++ toStr True ed ++ "  (right: AND<->OR, inputs flipped)")
     , bottom )

draw :: Env -> World -> Picture
draw env w = view env $
  [ picL, picR
  -- 中央で比較：¬E と E^d(¬·)
  , txt (midX - 170) (bottom + 30) 16 (hsb 0 0 100)
      ("~E = " ++ show (1 - outV outL) ++ "    E^d(~A, ~B, ..) = " ++ show (outV outR) ++ (if 1 - outV outL == outV outR then "  OK match" else "  X")) ]
  ++ (if wTable w then table else [])
  ++ [ txt 20 32 18 (hsb 0 0 100) ("Expr " ++ show (wNo w) ++ " (1-4): E = " ++ toStr True e ++ "    dual E^d = " ++ toStr True ed ++ "    T: table " ++ (if wTable w then "ON" else "off"))
     , txt 20 58 14 (hsb 0 0 60) "Click an input circle to flip 0/1. Pushing NOT inward swaps AND and OR: ~(A & B) = ~A | ~B."
     , txt 20 78 14 (hsb 0 0 60) "Turn the circuit inside out and the answer is the same." ]
  where
    e = exprOf (wNo w)
    ed = dualE e
    vars = sort (nub (varsOf e))
    midX = envW env / 2
    ((outL, picL), (outR, picR), bottom) = circuits env w
    -- 真理値表
    table =
      let nv = length vars
          rows = (2 :: Int) ^ nv
          ty = envH env * 0.55 + 60
          rh = min 24 ((envH env - ty - 30) / fromIntegral (rows + 1))
          cols = map (: []) vars ++ ["E", "~E", "E^d(~.)", "same"]
          colX ci = 60 + fromIntegral (ci :: Int) * 90
          rowOf r =
            let bits = [(v, (r `div` (2 ^ (nv - 1 - i))) `mod` 2) | (i, v) <- zip [0 ..] vars]
                envN = [(k, 1 - v) | (k, v) <- bits]
                vE = evalE e bits
                vD = evalE ed envN
                current = all (\(k, v) -> v == val (wInputs w) k) bits
                yy = ty + fromIntegral (r + 1) * rh
                vals = map (show . snd) bits ++ [show vE, show (1 - vE), show vD, if 1 - vE == vD then "OK" else "X"]
                colC ci | ci == nv + 1 || ci == nv + 2 = hsb 55 70 100
                        | ci >= nv + 1 = hsb 0 70 100
                        | otherwise = hsb 0 0 100
            in [ solid (hsbA 55 80 100 20) 50 (yy - rh + 6) (fromIntegral (length cols) * 90) rh | current ]
               ++ [ txt (colX ci) yy 14 (colC ci) s | (ci, s) <- zip [0 ..] vals ]
      in [ txt (colX ci) ty 14 (hsb 0 0 70) s | (ci, s) <- zip [0 ..] cols ]
         ++ concatMap rowOf [0 .. rows - 1]
         ++ [ txt (colX (length cols) + 20) (ty + rh) 14 (hsb 0 0 60) "Columns ~E and E^d(~.) agree on every row" ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let ((outL, _), _, _) = circuits env w
        m = mouseP env
        hit k = any (\(c, p) -> c == k && dist2 m p < 18) (inputPos outL)
    in w { wInputs = [(k, if hit k then 1 - v else v) | (k, v) <- wInputs w] }
  EventKey (Char k) Down _ _
    | k `elem` "1234" -> w { wNo = fromEnum k - fromEnum '0' }
    | k `elem` "tT" -> w { wTable = not (wTable w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 式 1 で 4 通りの入力をすべて試す。¬(A ∧ B) と ¬A ∨ ¬B の真理値表が同じ
-- 2. exprs に自分の式を足す（Or [V 'A', Not (And [V 'B', V 'C'])] など）。双対が自動で出る
-- 3. 定数 0・1 も双対では入れ替わる：A ∨ 1 = 1 の双対は A ∧ 0 = 0。式に定数を足すなら evalE も直そう
-- 4. 集合の世界では ∧ = ∩、∨ = ∪、¬ = 補集合。ベン図で同じ法則を描いてみよう

-- ---------- 道具 ----------
-- 左上が (x, y)、角の半径 r の角丸長方形の頂点
roundBox :: Float -> Float -> Float -> Float -> Float -> [Point]
roundBox x y w h r = concat
  [ corner (x + r) (y + r) pi, corner (x + w - r) (y + r) (1.5 * pi)
  , corner (x + w - r) (y + h - r) 0, corner (x + r) (y + h - r) (0.5 * pi) ]
  where
    corner cx cy t0 = [(cx + r * cos t, cy + r * sin t) | k <- [0 .. 6 :: Int], let t = t0 + fromIntegral k / 6 * pi / 2]

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中心が (x, y) の文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = Translate x y (Scale 1 (-1) (labelC 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の塗りつぶした長方形
solid :: Color -> Float -> Float -> Float -> Float -> Picture
solid c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 太さ w の閉じた折れ線
closed :: Float -> Color -> [Point] -> Picture
closed w c ps = path w c (ps ++ take 1 ps)

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
