-- ============================================
-- 11 モールス信号を声で送る（マイクで受信）
-- 「トン（短）」「ツー（長）」を口笛や手拍子、「あ」の声で送ると、文字に戻る
-- 音がある時間の長さで . と - を判定し、無音の長さで文字の区切りを判定する
-- クリックでマイク ON / マウス左右 = 感度 / C で消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import qualified Data.Map.Strict as M

morse :: M.Map String Char
morse = M.fromList
  [ (".-", 'A'), ("-...", 'B'), ("-.-.", 'C'), ("-..", 'D'), (".", 'E'), ("..-.", 'F'), ("--.", 'G')
  , ("....", 'H'), ("..", 'I'), (".---", 'J'), ("-.-", 'K'), (".-..", 'L'), ("--", 'M'), ("-.", 'N')
  , ("---", 'O'), (".--.", 'P'), ("--.-", 'Q'), (".-.", 'R'), ("...", 'S'), ("-", 'T'), ("..-", 'U')
  , ("...-", 'V'), (".--", 'W'), ("-..-", 'X'), ("-.--", 'Y'), ("--..", 'Z'), (".----", '1')
  , ("..---", '2'), ("...--", '3'), ("....-", '4'), (".....", '5'), ("-....", '6'), ("--...", '7')
  , ("---..", '8'), ("----.", '9'), ("-----", '0') ]

data World = World
  { wMic      :: !(Maybe Mic)
  , wLive     :: !Bool      -- 本物のマイクか
  , wIsOn     :: !Bool      -- いま音があるか
  , wOnStart  :: !Float     -- 音が始まった時刻（ミリ秒）
  , wOffStart :: !Float     -- 音が止まった時刻（ミリ秒）
  , wCurrent  :: !String    -- いま打っている文字の . と -
  , wDecoded  :: !String    -- 確定した文字
  , wHist     :: ![Float]   -- 音量の履歴（新しいものが先頭）
  }

dotMax :: Float
dotMax = 220                -- ★ これより短い音 = .、長い = -（ミリ秒）

main :: IO ()
main = runSketch (sketch "06-11 Mic Morse" (World Nothing False False 0 0 "" "" []))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> step env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- マウス左右で感度（しきい値）を決める
thresholdOf :: Env -> Float
thresholdOf env = clampF 0.01 0.2 (remap (fst (mouseXY env)) 0 (envW env) 0.01 0.2)

step :: Env -> World -> IO World
step env w = do
  lvl <- maybe (pure 0) micLevel (wMic w)
  live <- maybe (pure False) micLive (wMic w)
  let hist = take (round (envW env)) (lvl : wHist w)
  pure (detect (envTime env * 1000) (thresholdOf env) lvl w { wLive = live, wHist = hist })

-- ★ 音の ON/OFF の切り替わりを検出して、長さで判定
detect :: Float -> Float -> Float -> World -> World
detect now th lvl w0 = settle (edge w0)
  where
    edge w
      | not (wIsOn w) && lvl > th =
          -- 無音が長かったら、文字の区切り
          let w' = if longSilence w then commitLetter w else w
          in w' { wIsOn = True, wOnStart = now }
      | wIsOn w && lvl < th * 0.7 =
          let dur = now - wOnStart w
              mark | dur <= 40 = ""
                   | dur < dotMax = "."
                   | otherwise = "-"
          in w { wIsOn = False, wOffStart = now, wCurrent = wCurrent w ++ mark }
      | otherwise = w
    -- 無音が十分長ければ確定
    settle w = if not (wIsOn w) && longSilence w then commitLetter w else w
    longSilence w = not (null (wCurrent w)) && now - wOffStart w > dotMax * 3

commitLetter :: World -> World
commitLetter w =
  let letter = M.findWithDefault '?' (wCurrent w) morse
      out = wDecoded w ++ [letter]
  in w { wDecoded = drop (length out - 24) out, wCurrent = "" }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- ---- 音量の波形としきい値 ----
    Color (hsb 0 0 30) (Line [toG env 0 base, toG env width' base])
  , Color (hsb 40 90 100) (Line [toG env 0 thY, toG env width' thY])
  , Color (hsb 170 80 100) (thickPath 2 (zipWith histPoint [0 ..] (reverse (wHist w))))
    -- ---- 表示 ----
  , Color (if wIsOn w then hsb 45 90 100 else hsb 230 30 35) (at (toG env (width' - 80) 110) (disc 30))
  , txt env 40 (h * 0.3) 60 (hsb 0 0 100) (if null (wCurrent w) then "_" else wCurrent w)
  , txt env 40 (h * 0.45) 48 (hsb 45 90 100) (wDecoded w)
  , txt env 20 40 18 (hsb 0 0 100) status
  , txt env 20 68 15 (hsb 0 0 60) "Examples: S = ...   O = ---   E = .   T = -     SOS = ... --- ..."
  , case wMic w of
      Just _ | not (wLive w) -> txt env 20 92 15 (hsb 0 0 60) "No mic found: demo signal"
      _ -> Blank
  ]
  where
    width' = envW env
    h = envH env
    base = h * 0.7
    thY = base - thresholdOf env * h * 1.2
    histPoint i v = toG env i (base - min v 0.5 * h * 1.2)
    status = case wMic w of
      Nothing -> "Click to turn the mic ON"
      Just _ -> "Short sound = .  Long sound = -  Pause to finish a letter  Sensitivity: mouse X"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case wMic w of
    Nothing -> do
      m <- startMic
      pure w { wMic = Just m }
    Just _ -> pure w
  EventKey (Char c) Down _ _ | c == 'c' || c == 'C' -> pure w { wDecoded = "", wCurrent = "" }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「あ・あ・あ」で S、「あーあーあー」で O を送ってみよう。SOS が出せたら成功
-- 2. dotMax（220 ms）を変えて、自分の速さに合わせよう
-- 3. 10 の DTMF のように「周波数」で送ると、声より確実。micSpectrum を使うとどう変えられる？

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
