-- ============================================
-- 14 ライブコーディング・スタジオ
-- 音楽と映像を「鳴らしながら・動かしながら」コードで変えていく
--
--  ・左の黒い欄にコードを書いて  Ctrl + Enter（または F5）で反映
--  ・Ctrl + .（ピリオド）または F6 で音を全部止める
--  ・Esc でコード欄を隠す／出す（映像だけを見せたいとき）
--  ・PageUp / PageDown でお手本のレシピ L01〜L08 を読みこむ
--
-- コード欄で使える命令（ここだけの小さな言語。p5.js 版の書き方と同じ）：
--   bpm(120)
--   drum('kick', 'x...x...', 0.8)       x=鳴らす X=強く .=休み（kick snare hat clap）
--   synth('bass', 'C2 . G1 ?', { wave: 'sawtooth', cutoff: 800, len: 0.25, vol: 0.3 })
--   scene('pulse')                      pulse grid tunnel wave stars spin orbit
--   hush()
-- ============================================
-- 映像を自分で書きたいときは、この Main.hs の drawScene に場面を足す。
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Map.Strict as Map

-- ==================================================
-- 演奏の状態
-- ==================================================

data SynthOpts = SynthOpts
  { oWave :: !Wave, oCutoff :: !Float, oLen :: !Float, oVol :: !Float }

data Track
  = DrumTrack  { tSound :: !String, tTokens :: ![String], tVol :: !Float }
  | SynthTrack { tTokens :: ![String], tNotes :: ![String], tOpts :: !SynthOpts }

-- コードを実行してできる「今の演奏」
data Live = Live
  { lBpm    :: !Float
  , lTracks :: !(Map.Map String Track)
  , lScene  :: !String
  }

-- 映像に渡す情報（1 フレームぶん）。kick などは音が鳴ると 1 になり、だんだん 0 に戻る
data V = V
  { vT :: !Float, vStep :: !Int, vBar :: !Int
  , vKick, vSnare, vHat, vClap, vNote :: !Float
  , vAngle :: !Float }

-- 今鳴っている音（波形の表示に使う）
data Voice = Voice
  { vcFreq :: !Float, vcAmp :: !Float, vcWave :: !Wave, vcStart :: !Float, vcDur :: !Float }

data OrbitRing = OrbitRing { oR :: !Float, oHue :: !Float }

data World = World
  { -- コード欄
    wText    :: !String
  , wPos     :: !Int            -- カーソルの位置（何文字目か）
  , wShown   :: !Bool           -- コード欄を表示しているか
  , wRecipe  :: !Int            -- 読みこんだレシピの番号
  , wStatus  :: !String
  , wError   :: !Bool
    -- 演奏
  , wLive    :: !Live
  , wPlaying :: !Bool
  , wNext    :: !Float          -- 次のステップを鳴らす時刻
  , wCount   :: !Int            -- 何ステップ目か
  , wVoices  :: ![Voice]
    -- 映像
  , wNow     :: !V              -- 今の値（音が鳴ると 1 になる）
  , wHist    :: ![V]            -- 過去のフレーム（残像のため。新しいものが先頭）
  , wSparks  :: ![(Float, [Point])]  -- 点のかたまり（透明度, 位置）
  , wWaves   :: ![[Point]]      -- 波形の線
  , wRings   :: ![OrbitRing]
  }

initialWorld :: World
initialWorld = World
  { wText = startCode, wPos = 0, wShown = True, wRecipe = 0
  , wStatus = "Ctrl + Enter (or F5) to start playing", wError = False
  , wLive = Live 120 Map.empty "pulse"
  , wPlaying = False, wNext = 0, wCount = 0, wVoices = []
  , wNow = V 0 0 0 0 0 0 0 0 0
  , wHist = [], wSparks = [], wWaves = [], wRings = []
  }

main :: IO ()
main = runSketch (sketch "01-14 Live Coding Studio" initialWorld)
  { skBack  = black
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ==================================================
-- コード欄で使える命令：字句 → 文 → 演奏
-- ==================================================

data Tok = TName String | TStr String | TNum Float | TSym Char
data Arg = AStr String | ANum Float | AObj [(String, Arg)]

-- 文字列を字句に分ける（// から行末まではコメント）
tokenize :: String -> Either String [Tok]
tokenize s = case s of
  [] -> Right []
  '/' : '/' : r -> tokenize (dropWhile (/= '\n') r)
  c : r
    | isSpace c -> tokenize r
    | c `elem` "'\"`" -> case break (== c) r of
        (str, _ : r') -> (TStr str :) <$> tokenize r'
        _ -> Left "A string is missing its closing quote"
    | isDigit c || (c `elem` ".-" && startsDigit r) ->
        let (numText, r') = span (`elem` "0123456789.-") s
        in case readNumber numText of
             Just n -> (TNum n :) <$> tokenize r'
             Nothing -> Left ("Bad number: " ++ numText)
    | isAlpha c ->
        let (name, r') = span isAlphaNum s
        in (TName name :) <$> tokenize r'
    | otherwise -> (TSym c :) <$> tokenize r      -- 記号。使えないものは次の段階でエラーになる
  where
    startsDigit (d : _) = isDigit d
    startsDigit [] = False

readNumber :: String -> Maybe Float
readNumber t = case reads (fix t) of
  [(n, "")] -> Just n
  _ -> Nothing
  where
    fix ('-' : r) = '-' : fix r
    fix ('.' : r) = "0." ++ r
    fix r = if last_ r == '.' then r ++ "0" else r
    last_ = foldr (\c acc -> if acc == '\0' then c else acc) '\0'

-- 字句を「命令(引数, ...)」の並びにする
parseProgram :: [Tok] -> Either String [(String, [Arg])]
parseProgram toks = case toks of
  [] -> Right []
  TSym ';' : r -> parseProgram r
  TName "visual" : _ -> Left "visual() is not available: use scene('stars'), 'spin', 'orbit'"
  TName n : TSym '(' : r -> do
    (args, r') <- parseArgs r
    ((n, args) :) <$> parseProgram r'
  TName n : _ -> Left ("Unknown command: " ++ n)
  _ -> Left "Each line must look like name(...)"

parseArgs :: [Tok] -> Either String ([Arg], [Tok])
parseArgs toks = case toks of
  TSym ')' : r -> Right ([], r)
  TSym ',' : r -> parseArgs r
  TStr s : r -> more (AStr s) r
  TNum n : r -> more (ANum n) r
  TSym '{' : r -> do
    (fields, r') <- parseFields r
    more (AObj fields) r'
  _ -> Left "Could not read the arguments inside ( )"
  where
    more a r = do
      (as, r') <- parseArgs r
      Right (a : as, r')

parseFields :: [Tok] -> Either String ([(String, Arg)], [Tok])
parseFields toks = case toks of
  TSym '}' : r -> Right ([], r)
  TSym ',' : r -> parseFields r
  TName k : TSym ':' : TStr s : r -> field k (AStr s) r
  TName k : TSym ':' : TNum n : r -> field k (ANum n) r
  _ -> Left "Options must look like { wave: 'sine', vol: 0.3 }"
  where
    field k v r = do
      (fs, r') <- parseFields r
      Right ((k, v) : fs, r')

-- 命令を 1 つ実行して、次の演奏をつくる
applyCommand :: Live -> (String, [Arg]) -> Either String Live
applyCommand next (name, args) = case (name, args) of
  ("bpm", [ANum n])
    | n >= 30 && n <= 300 -> Right next { lBpm = n }
  ("bpm", _) -> Left "bpm needs a number from 30 to 300"
  ("drum", AStr sound : AStr pat : rest) -> do
    vol <- case rest of
      [] -> Right 1
      [ANum v] -> Right v
      _ -> Left "The 3rd value of drum is a number (volume)"
    if sound `notElem` ["kick", "snare", "hat", "clap"]
      then Left ("drum names: kick snare hat clap (no \"" ++ sound ++ "\")") else Right ()
    let tokens = filter (not . isSpace) pat
    case filter (`notElem` "xX.-") tokens of
      bad : _ -> Left ("drum patterns use only x X . (not \"" ++ [bad] ++ "\")")
      [] -> Right next { lTracks = Map.insert ("drum:" ++ sound)
                           (DrumTrack sound (map (: []) tokens) vol) (lTracks next) }
  ("synth", AStr track : AStr pat : rest) -> do
    fields <- case rest of
      [] -> Right []
      [AObj fs] -> Right fs
      _ -> Left "The 3rd value of synth is { options }"
    opts <- foldM applyOption (SynthOpts Triangle 4000 0.25 0.3) fields
    let tokens = words pat
    case filter (not . validToken) tokens of
      bad : _ -> Left ("synth cannot use \"" ++ bad ++ "\" (like C4 D#3 . ?)")
      [] | null tokens -> Left "The synth pattern is empty"
         | otherwise -> Right next { lTracks = Map.insert ("synth:" ++ track)
             (SynthTrack tokens (filter (`notElem` [".", "?"]) tokens) opts) (lTracks next) }
  ("scene", [AStr s])
    | s `elem` sceneNames -> Right next { lScene = s }
  ("scene", _) -> Left ("scene names: " ++ unwords sceneNames)
  ("hush", []) -> Right next { lTracks = Map.empty }
  ("drum", _) -> Left "Write it like drum('kick', 'x...x...')"
  ("synth", _) -> Left "Write it like synth('bass', 'C2 . G1 .')"
  _ -> Left ("Unknown command: " ++ name)

applyOption :: SynthOpts -> (String, Arg) -> Either String SynthOpts
applyOption o field = case field of
  ("wave", AStr "sine") -> Right o { oWave = Sine }
  ("wave", AStr "triangle") -> Right o { oWave = Triangle }
  ("wave", AStr "sawtooth") -> Right o { oWave = Sawtooth }
  ("wave", AStr "square") -> Right o { oWave = Square }
  ("cutoff", ANum n) -> Right o { oCutoff = n }
  ("len", ANum n) -> Right o { oLen = max 0.01 n }
  ("vol", ANum n) -> Right o { oVol = n }
  (k, _) -> Left ("Bad synth option: " ++ k)

-- 'C4' 'D#3' 'Bb2' '.' '?' のどれか
validToken :: String -> Bool
validToken t = case t of
  "." -> True
  "?" -> True
  [c, o] -> isNote c && isDigit o
  [c, m, o] -> isNote c && m `elem` "#b" && isDigit o
  _ -> False
  where isNote c = c >= 'A' && c <= 'G'

-- コードを実行して、演奏に反映する。エラーがなかったときだけ切りかえる（音は止まらない）
runCode :: Env -> World -> World
runCode env w =
  case tokenize (wText w) >>= parseProgram >>= foldM applyCommand fresh of
    Left err -> setStatus True ("Error: " ++ err) w
    Right live ->
      let started = if wPlaying w then w else w { wPlaying = True, wNext = envTime env + 0.1 }
      in setStatus False "OK: applied" started { wLive = live }
  where fresh = Live (lBpm (wLive w)) Map.empty "pulse"

setStatus :: Bool -> String -> World -> World
setStatus isError msg w = w { wStatus = msg, wError = isError }

-- ==================================================
-- 時間を進める
-- ==================================================

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let now = envTime env
  w1 <- schedule now w0
  let scene = lScene (wLive w1)
      old = wNow w1
      v = old { vT = now, vStep = wCount w1 `mod` 16, vBar = wCount w1 `div` 16
              , vAngle = if scene == "spin" then vAngle old + 0.01 + vKick old * 0.08 else vAngle old }
  sparks <- newSparks env scene v
  pure w1
    { wVoices = filter (\s -> now < vcStart s + vcDur s) (wVoices w1)
    , wHist = take 8 (v : wHist w1)
    , wSparks = take 8 (sparks : wSparks w1)
    , wWaves = if scene == "wave" then take 3 (wavePoints env now (wVoices w1) : wWaves w1) else []
    , wRings = updateRings scene v (wRings w1)
    -- だんだん 0 に戻す
    , wNow = v { vKick = vKick v * 0.88, vSnare = vSnare v * 0.88, vHat = vHat v * 0.88
               , vClap = vClap v * 0.88, vNote = vNote v * 0.88 }
    }

-- 時刻が来たステップを鳴らす
schedule :: Float -> World -> IO World
schedule now w
  | not (wPlaying w) = pure w
  | otherwise = go w { wNext = if wNext w < now - 0.2 then now + 0.05 else wNext w }
  where
    go x
      | wNext x > now = pure x
      | otherwise = do
          x' <- playStep now x
          go x' { wNext = wNext x' + 60 / lBpm (wLive x') / 4   -- 16 分音符 1 つ分
                , wCount = wCount x' + 1 }

playStep :: Float -> World -> IO World
playStep now w = do
  results <- mapM (playTrack now (wCount w)) (Map.elems (lTracks (wLive w)))
  let voices = concatMap fst results
      hits = concatMap snd results
  pure w { wVoices = take 24 (voices ++ wVoices w), wNow = foldr hit (wNow w) hits }

hit :: String -> V -> V
hit name v = case name of
  "kick" -> v { vKick = 1 }
  "snare" -> v { vSnare = 1 }
  "hat" -> v { vHat = 1 }
  "clap" -> v { vClap = 1 }
  _ -> v { vNote = 1 }

-- トラックのこのステップを鳴らす。(鳴らした音, 映像に伝える名前) を返す
playTrack :: Float -> Int -> Track -> IO ([Voice], [String])
playTrack now n tr = case tr of
  DrumTrack sound tokens vol -> case tokenAt n tokens of
    Just tok | tok == "x" || tok == "X" -> do
      voices <- mapM (sound_ now) (drumTones sound (vol * (if tok == "X" then 1 else 0.7)))
      pure (voices, [sound])
    _ -> pure ([], [])
  SynthTrack tokens noteList opts -> do
    tok <- case tokenAt n tokens of
      Just "?" -> randomNote noteList      -- ? = パターンの中の音からランダム
      Just t -> pure t
      Nothing -> pure "."
    if tok == "." then pure ([], []) else do
      let f = noteFreq tok
      v <- sound_ now (f, oVol opts * 0.8, oLen opts, softened opts f)
      pure ([v], ["note"])

tokenAt :: Int -> [a] -> Maybe a
tokenAt _ [] = Nothing
tokenAt n xs = case drop (n `mod` length xs) xs of
  (x : _) -> Just x
  [] -> Nothing

randomNote :: [String] -> IO String
randomNote [] = pure "."
randomNote ns = do
  i <- randomRIO (0, length ns - 1)
  pure (concat (take 1 (drop i ns)))

-- 音を鳴らして、波形の表示のために覚えておく
sound_ :: Float -> (Float, Float, Float, Wave) -> IO Voice
sound_ now (f, a, d, wv) = do
  playTone f a d wv
  pure (Voice f a wv now d)

-- ドラムの音（周波数, 音量, 長さ, 音色）。★ 数字を変えると音が変わる
drumTones :: String -> Float -> [(Float, Float, Float, Wave)]
drumTones name v = case name of
  "kick"  -> [(70, 0.72 * v, 0.3, Triangle)]
  "snare" -> [(190, 0.4 * v, 0.1, Triangle), (2300, 0.2 * v, 0.12, Sawtooth)]
  "hat"   -> [(7000, 0.1 * v, 0.05, Square)]
  _       -> [(1500, 0.25 * v, 0.12, Sawtooth)]          -- clap

-- cutoff が音に近いほど、丸い音色にする（p5.js 版のローパスフィルタの代わり）
softened :: SynthOpts -> Float -> Wave
softened o f
  | oCutoff o < f = Sine
  | oCutoff o < f * 6 && oWave o /= Sine = Triangle
  | otherwise = oWave o

-- pulse と stars の場面で散らす点
newSparks :: Env -> String -> V -> IO (Float, [Point])
newSparks env scene v = case scene of
  "pulse" -> (,) (vHat v * 200 / 255) <$> replicateM 12 randomPoint
  "stars" -> (,) 1 <$> replicateM (ceiling (20 * vHat v)) randomPoint
  _ -> pure (0, [])
  where
    randomPoint = do
      x <- randomRIO (envW env * (0.4 - 0.5), envW env * 0.5)
      y <- randomRIO (-envH env / 2, envH env / 2)
      pure (x, y)

-- 今鳴っている音を足し合わせて、音の波形（オシロスコープ）をつくる
wavePoints :: Env -> Float -> [Voice] -> [Point]
wavePoints env now voices =
  [ ( lerpF (envW env * (0.4 - 0.5)) (envW env * 0.5) u
    , clampF (-1) 1 (sum (map (voiceAt (now + u * span_)) voices)) * envH env * 0.4 )
  | i <- [0 .. points]
  , let u = fromIntegral i / fromIntegral points ]
  where
    points = 256 :: Int
    span_ = 1024 / 44100 :: Float

voiceAt :: Float -> Voice -> Float
voiceAt t s =
  let dt = t - vcStart s
      env_ = max 0 (1 - dt / vcDur s)
  in vcAmp s * env_ * osc (vcWave s) (vcFreq s * dt)

-- orbit の場面：キックのたびに輪を足す
updateRings :: String -> V -> [OrbitRing] -> [OrbitRing]
updateRings scene v rings
  | scene /= "orbit" = []
  | otherwise =
      [ r { oR = oR r + 8 } | r <- added, oR r + 8 <= 800 ]
  where
    added | vKick v > 0.95 = OrbitRing 50 (wrapF 360 (fromIntegral (vBar v) * 40)) : rings
          | otherwise = rings

-- ==================================================
-- 用意された映像（scene）
-- ==================================================

sceneNames :: [String]
sceneNames = ["pulse", "grid", "tunnel", "wave", "stars", "spin", "orbit"]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures [drawScene env w, drawEditor env w, drawHud env w]

-- 古いフレームから順に、薄くして重ねる（p5.js の半透明の background に相当）
trailed :: Float -> Int -> World -> (Float -> V -> Picture) -> Picture
trailed fade n w f =
  Pictures (reverse (zipWith f (iterate (* fade) 1) (take n (wHist w))))

drawScene :: Env -> World -> Picture
drawScene env w = case lScene (wLive w) of
  "grid" -> sceneGrid env w
  "tunnel" -> sceneTunnel env w
  "wave" -> sceneWave env w
  "stars" -> sceneStars env w
  "spin" -> sceneSpin env w
  "orbit" -> sceneOrbit env w
  _ -> scenePulse env w

-- 画面の横 65% の位置（映像の中心）
centerX :: Env -> Float
centerX env = envW env * 0.15

-- 真ん中の円がキックで脈打つ
scenePulse :: Env -> World -> Picture
scenePulse env w = Pictures [trailed 0.84 8 w body, sparksOf 1.5 w]
  where
    body a v = at (centerX env, 0) $ Pictures
      [ Color (rgba a 255 60 140) (disc ((80 + vKick v * 260) / 2))
      , if vSnare v > 0.02
          then Color (rgba (a * vSnare v) 0 230 255) (ring ((300 + (1 - vSnare v) * 300) / 2) 6)
          else Blank
      ]

-- 16 マスのグリッドが音に合わせて光る
sceneGrid :: Env -> World -> Picture
sceneGrid env w = Pictures
  [ Color (cellColor i) (roundBox (x0 + fromIntegral (i `mod` 4) * size + size / 2)
                                  (y0 - fromIntegral (i `div` 4) * size - size / 2)
                                  (size - 8) (size - 8) 6)
  | i <- [0 .. 15 :: Int] ]
  where
    v = current w
    size = min (envW env * 0.5) (envH env - 80) / 4
    x0 = envW env * 0.2 - size * 2
    y0 = size * 2
    cellColor i
      | i == vStep v = makeColor 1 1 1 (clampF 0 1 (0.5 + vKick v))
      | otherwise = gray ((30 + 80 * vSnare v) / 255)

-- 回転する四角のトンネル
sceneTunnel :: Env -> World -> Picture
sceneTunnel env w = trailed 0.76 4 w $ \a v -> at (centerX env, 0) $ Pictures
  [ Rotate (deg (vT v * 0.5 + fi * 0.12)) $
      Color (hsba (vT v * 40 + fi * 20) 80 100 (0.9 * a)) $
        square (2 + vKick v * 6) ((fi + 1) * 40 + vKick v * 30)
  | i <- [0 .. 13 :: Int], let fi = fromIntegral i ]
  where
    square t s =
      let q = s / 2
      in thickPath t [(-q, -q), (q, -q), (q, q), (-q, q), (-q, -q)]

-- 音の波形（オシロスコープ）
sceneWave :: Env -> World -> Picture
sceneWave _ w = Pictures
  (reverse (zipWith (\a pts -> Color (rgba a 0 255 200) (thickPath 3 pts))
                    (iterate (* 0.69) 1) (wWaves w)))

-- キックで大きくなる円、スネアで光る四角、ハイハットで星が散る
sceneStars :: Env -> World -> Picture
sceneStars env w = Pictures [bg, trailed 0.76 6 w body, sparksOf 2 w]
  where
    bg = Color (rgb255 10 10 30) (rectangleSolid (envW env) (envH env))
    body a v = at (centerX env, 0) $ Pictures
      [ Color (rgba a 255 80 150) (disc ((100 + vKick v * 300) / 2))
      , if vSnare v > 0.02 then Color (rgba (a * vSnare v) 0 220 255) (rect 0 0 500 20) else Blank
      ]

-- 回転する光（キックで回転が速くなる）
sceneSpin :: Env -> World -> Picture
sceneSpin env w = trailed 0.88 8 w $ \a v -> at (centerX env, 0) $ Rotate (deg (vAngle v)) $ Pictures
  (concat
    [ [ Color (hsba h 80 100 a) (thickLine (2 + vClap v * 10) (0, 0) (r * cos t, r * sin t))
      , Color (hsba h 60 100 a) (at (d * cos t, d * sin t) (disc ((12 + vHat v * 20) / 2))) ]
    | i <- [1 .. n]
    , let fi = fromIntegral i
          t = tau * fi / fromIntegral n
          h = vT v * 30 + fi * 30
          r = 150 + vKick v * 200
          d = 200 + vNote v * 60 ])
  where n = 12 :: Int        -- ★ 光の本数

-- 広がる輪、回る 16 の点、スネアのフラッシュ
sceneOrbit :: Env -> World -> Picture
sceneOrbit env w = Pictures ([bg] ++ rings ++ dots ++ [flash])
  where
    v = current w
    bg = Color (rgb255 5 5 15) (rectangleSolid (envW env) (envH env))
    cx = centerX env
    rings =
      [ Color (hsba (oHue o) 70 100 (clampF 0 1 ((100 - oR o / 8) / 100) * fade))
              (at (cx, 0) (ring (max 1 (oR o - 8 * k) / 2) 4))
      | o <- wRings w, (k, fade) <- zip [2, 1, 0] [0.64, 0.8, 1] ]
    dots =
      [ Color (if on then hsb 0 0 100 else hsb 200 60 100)
              (at (cx + cos a * 180, sin a * 180) (disc ((if on then 24 + vNote v * 20 else 8) / 2)))
      | i <- [0 .. 15 :: Int]
      , let on = i == vStep v
            a = pi / 2 - tau * fromIntegral i / 16 ]    -- 真上から時計回り
    flash = Color (makeColor 1 1 1 (vSnare v * 0.25)) (rectangleSolid (envW env) (envH env))

sparksOf :: Float -> World -> Picture
sparksOf r w = Pictures
  [ Color (makeColor 1 1 1 (a * fade)) (at q (disc r))
  | ((a, pts), fade) <- zip (wSparks w) (iterate (* 0.84) 1)
  , a * fade > 0.02
  , q <- pts ]

-- 最新のフレーム（まだ無ければ今の値）
current :: World -> V
current w = case wHist w of
  v : _ -> v
  [] -> wNow w

-- ==================================================
-- コード欄（文字は 1 文字ずつマス目に置くので、カーソルの位置が計算できる）
-- ==================================================

charW, rowH, edPad, edSize :: Float
charW = 11
rowH = 22
edPad = 16
edSize = 14

edWidth :: Env -> Float
edWidth env = envW env * 0.46

edCols, edRows :: Env -> Int
edCols env = max 10 (floor ((edWidth env - 2 * edPad) / charW))
edRows env = max 3 (floor ((envH env - 44 - 2 * edPad) / rowH))

-- 画面の行に分ける： (その行の先頭が何文字目か, 行の中身)
layoutRows :: Int -> String -> [(Int, String)]
layoutRows cols = go 0
  where
    go start s =
      let (ln, rest) = break (== '\n') s
          here = chunks start ln
      in case rest of
           _ : r -> here ++ go (start + length ln + 1) r
           [] -> here
    chunks start ln
      | length ln <= cols = [(start, ln)]
      | otherwise = (start, take cols ln) : chunks (start + cols) (drop cols ln)

-- カーソルが何行目の何文字目か
cursorRC :: [(Int, String)] -> Int -> (Int, Int)
cursorRC rows pos =
  case [ (r, pos - s) | (r, (s, _)) <- reverse (zip [0 ..] rows), s <= pos ] of
    rc : _ -> rc
    [] -> (0, 0)

topRow :: Env -> Int -> Int
topRow env curRow = max 0 (curRow - edRows env + 1)

drawEditor :: Env -> World -> Picture
drawEditor env w
  | not (wShown w) = Blank
  | otherwise = Pictures (panel : cursor : chars)
  where
    h = envH env
    x0 = -envW env / 2 + edPad
    yTop = h / 2 - edPad
    panel = Color (rgba255 0 0 0 153)
      (rect (-envW env / 2 + edWidth env / 2) (22) (edWidth env) (h - 44))
    rows = layoutRows (edCols env) (wText w)
    (curRow, curCol) = cursorRC rows (wPos w)
    top = topRow env curRow
    visible = zip [0 :: Int ..] (take (edRows env) (drop top rows))
    chars =
      [ label (x0 + fromIntegral c * charW) (yTop - (fromIntegral r + 1) * rowH + 6) edSize (gray 0.94) [ch]
      | (r, (_, str)) <- visible
      , (c, ch) <- zip [0 :: Int ..] str
      , ch /= ' ' ]
    cursor
      | fract (envTime env) < 0.6 = Color (rgb255 120 255 180)
          (rect (x0 + fromIntegral curCol * charW) (yTop - (fromIntegral (curRow - top) + 0.5) * rowH + 2) 2 18)
      | otherwise = Blank

-- 下のバー（テンポ・16 ステップ・メッセージ）
drawHud :: Env -> World -> Picture
drawHud env w = Pictures (bar : status : bpmText : steps)
  where
    ww = envW env
    y = -envH env / 2
    bar = Color (rgba255 0 0 0 200) (rect 0 (y + 22) ww 44)
    status = label (-ww / 2 + 16) (y + 15) 14
      (if wError w then rgb255 255 90 90 else rgb255 120 255 180)
      (take (floor ((ww - 16 * 18 - 150) / 12)) (wStatus w))
    x0 = ww / 2 - 16 * 18 - 110
    stepNow = wCount w `mod` 16
    steps =
      [ Color (gray (shade i / 255)) (rect (x0 + fromIntegral i * 18 + 6) (y + 22) 12 12)
      | i <- [0 .. 15 :: Int] ]
    shade i
      | wPlaying w && i == stepNow = 255
      | i `mod` 4 == 0 = 110
      | otherwise = 60
    bpmText = label (ww / 2 - 96) (y + 15) 14 (gray (200 / 255)) (show (round (lBpm (wLive w)) :: Int) ++ " BPM")

-- ==================================================
-- キーとマウス
-- ==================================================

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  -- 反映する
  EventKey (SpecialKey KeyEnter) Down m _ | ctrl m == Down -> runCode env w
  EventKey (Char '\n') Down _ _ -> runCode env w
  EventKey (SpecialKey KeyF5) Down _ _ -> runCode env w
  -- 全部止める
  EventKey (Char '.') Down m _ | ctrl m == Down -> stopAll w
  EventKey (SpecialKey KeyF6) Down _ _ -> stopAll w
  -- コード欄を隠す／出す
  EventKey (SpecialKey KeyEsc) Down _ _ -> w { wShown = not (wShown w) }
  _ | not (wShown w) -> w
  -- レシピを読みこむ
  EventKey (SpecialKey KeyPageDown) Down _ _ -> loadRecipe (wRecipe w + 1) w
  EventKey (SpecialKey KeyPageUp) Down _ _ -> loadRecipe (wRecipe w - 1) w
  -- 文字の入力
  EventKey (SpecialKey KeySpace) Down _ _ -> insert " " w
  EventKey (SpecialKey KeyTab) Down _ _ -> insert "  " w
  EventKey (SpecialKey KeyEnter) Down _ _ -> insert "\n" w
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace w
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace w   -- Mac の delete キー
  EventKey (Char '\b') Down _ _ -> backspace w
  EventKey (Char c) Down m _ | c >= ' ' && c <= '~' && ctrl m == Up -> insert [c] w
  -- カーソルの移動
  EventKey (SpecialKey KeyLeft) Down _ _ -> moveTo (wPos w - 1) w
  EventKey (SpecialKey KeyRight) Down _ _ -> moveTo (wPos w + 1) w
  EventKey (SpecialKey KeyUp) Down _ _ -> moveRow env (-1) w
  EventKey (SpecialKey KeyDown) Down _ _ -> moveRow env 1 w
  EventKey (SpecialKey KeyHome) Down _ _ -> rowEdge env False w
  EventKey (SpecialKey KeyEnd) Down _ _ -> rowEdge env True w
  EventKey (MouseButton LeftButton) Down _ p -> clickAt env p w
  _ -> w

stopAll :: World -> World
stopAll w = setStatus False "All stopped (Ctrl + Enter to play again)"
  w { wLive = (wLive w) { lTracks = Map.empty } }

insert :: String -> World -> World
insert s w
  | length (wText w) > 6000 = w     -- ★ 長くなりすぎないように
  | otherwise =
      let (a, b) = splitAt (wPos w) (wText w)
      in w { wText = a ++ s ++ b, wPos = wPos w + length s }

backspace :: World -> World
backspace w
  | wPos w <= 0 = w
  | otherwise =
      let (a, b) = splitAt (wPos w - 1) (wText w)
      in w { wText = a ++ drop 1 b, wPos = wPos w - 1 }

moveTo :: Int -> World -> World
moveTo p w = w { wPos = max 0 (min (length (wText w)) p) }

moveRow :: Env -> Int -> World -> World
moveRow env d w =
  let rows = layoutRows (edCols env) (wText w)
      (r, c) = cursorRC rows (wPos w)
  in case drop (r + d) rows of
       (s, str) : _ | r + d >= 0 -> w { wPos = s + min c (length str) }
       _ -> w

rowEdge :: Env -> Bool -> World -> World
rowEdge env toEnd w =
  let rows = layoutRows (edCols env) (wText w)
      (r, _) = cursorRC rows (wPos w)
  in case drop r rows of
       (s, str) : _ -> w { wPos = if toEnd then s + length str else s }
       [] -> w

-- クリックした場所にカーソルを動かす
clickAt :: Env -> Point -> World -> World
clickAt env (mx, my) w
  | mx > -envW env / 2 + edWidth env || my < -envH env / 2 + 44 = w
  | otherwise =
      let rows = layoutRows (edCols env) (wText w)
          (curRow, _) = cursorRC rows (wPos w)
          r = floor ((envH env / 2 - edPad - my) / rowH) + topRow env curRow
          c = max 0 (round ((mx + envW env / 2 - edPad) / charW))
      in case drop (max 0 r) rows of
           (s, str) : _ -> w { wPos = s + min c (length str) }
           [] -> moveTo (length (wText w)) w

loadRecipe :: Int -> World -> World
loadRecipe i w =
  let n = length recipes
      k = i `mod` n
  in case drop k recipes of
       text_ : _ -> setStatus False ("Recipe L0" ++ show (k + 1) ++ " loaded. Ctrl + Enter to play")
                      w { wText = text_, wPos = 0, wRecipe = k }
       [] -> w

-- ---------- やってみよう ----------
-- 1. bpm や drum のパターンを書きかえて Ctrl + Enter。音を止めずに切りかわる
-- 2. PageDown で L01〜L08 のレシピを順に読みこんで、鳴らして、壊してみよう
-- 3. この Main.hs に自分の scene を足そう（sceneNames と drawScene に 1 行ずつ）
--    V の中身： vKick vSnare vHat vClap vNote（鳴ると 1）、vStep（0〜15）、vBar、vT（秒）

-- ---------- 道具 ----------

-- 透明度 a と、0〜255 の赤緑青から色をつくる
rgba :: Float -> Float -> Float -> Float -> Color
rgba a r g b = makeColor (r / 255) (g / 255) (b / 255) a

-- ラジアン → gloss の Rotate が使う度
deg :: Float -> Float
deg r = r * 180 / pi

-- 波の形：位相 p（1 で一周）での値を -1〜1 で返す
osc :: Wave -> Float -> Float
osc wave p0 =
  let p = fract p0
  in case wave of
       Sine -> sin (tau * p)
       Triangle -> 4 * abs (p - 0.5) - 1
       Sawtooth -> 2 * p - 1
       Square -> if p < 0.5 then 1 else -1

-- 中心 (x, y)、幅 w、高さ h、角の丸み r の四角
roundBox :: Float -> Float -> Float -> Float -> Float -> Picture
roundBox x y w h r0 = Polygon (concatMap corner [(1, 1, 0), (-1, 1, 90), (-1, -1, 180), (1, -1, 270)])
  where
    r = min r0 (min w h / 2)
    corner (sx, sy, a0) =
      [ (x + sx * (w / 2 - r) + r * cos a, y + sy * (h / 2 - r) + r * sin a)
      | i <- [0 .. 4 :: Int], let a = (a0 + fromIntegral i * 22.5) * pi / 180 ]

-- ==================================================
-- 最初のコードと、お手本のレシピ（コード欄の文字は英語のみ）
-- ==================================================

startCode :: String
startCode = unlines
  [ "// Edit this, then press Ctrl + Enter (or F5)"
  , "bpm(120)"
  , ""
  , "drum('kick',  'x...x...x...x...')"
  , "drum('hat',   '..x...x...x...x.')"
  , "drum('snare', '....x.......x...')"
  , ""
  , "synth('bass', 'C2 . . C2 . . G1 . C2 . . C2 . . A#1 .', { wave: 'sawtooth', cutoff: 800 })"
  , ""
  , "// pulse grid tunnel wave stars spin orbit"
  , "scene('pulse')"
  ]

recipes :: [String]
recipes = map unlines
  [ [ "// L01 First beat"
    , "// x = play  . = rest   1 letter = one 16th note"
    , "bpm(110)"
    , ""
    , "drum('kick', 'x...x...x...x...')"
    , "drum('hat',  '..x...x...x...x.')"
    , ""
    , "scene('pulse')"
    , ""
    , "// Try: change a . to x, change bpm to 90 or 140,"
    , "// put // in front of a line to mute it"
    ]
  , [ "// L02 Drum patterns: kick snare hat clap"
    , "// X = accent"
    , "bpm(124)"
    , ""
    , "drum('kick',  'X...x...X...x...')"
    , "drum('snare', '....x.......x...')"
    , "drum('hat',   'x.x.x.x.x.xXx.x.')"
    , "drum('clap',  '............x..x')"
    , ""
    , "scene('grid')"
    , ""
    , "// Try: drum('clap', 'x..')  3 steps against 16"
    , "// Try: drum('hat', 'x.x.x.x.', 0.5)  quieter"
    ]
  , [ "// L03 Bass line"
    , "// Notes: C D E F G A B + octave. C# = a half step up"
    , "bpm(120)"
    , ""
    , "drum('kick', 'x...x...x...x...')"
    , "drum('hat',  '..x...x...x...x.')"
    , ""
    , "synth('bass', 'C2 . C2 . . . C2 . D#2 . . C2 . . G1 .', {"
    , "  wave: 'sawtooth', // sine triangle sawtooth square"
    , "  cutoff: 600,      // smaller = rounder sound"
    , "  len: 0.2,         // note length (seconds)"
    , "  vol: 0.4,         // volume (0 to 1)"
    , "})"
    , ""
    , "scene('wave')"
    ]
  , [ "// L04 Melody and randomness"
    , "// ? = pick a random note from this pattern"
    , "bpm(118)"
    , ""
    , "drum('kick',  'x...x...x...x...')"
    , "drum('snare', '....x.......x...')"
    , "drum('hat',   'x.x.x.x.x.x.x.x.', 0.6)"
    , ""
    , "synth('bass', 'A1 . . A1 . . A1 . F1 . . F1 . . G1 .', { wave: 'sawtooth', cutoff: 700 })"
    , ""
    , "synth('lead', 'A4 ? . C5 ? . E5 ? . G5 ? . ? . ? .', { wave: 'square', cutoff: 2500, vol: 0.15, len: 0.15 })"
    , ""
    , "synth('pad1', 'A3 . . . . . . . F3 . . . . . . .', { wave: 'triangle', len: 1.5, vol: 0.15 })"
    , "synth('pad2', 'C4 . . . . . . . A3 . . . . . . .', { wave: 'triangle', len: 1.5, vol: 0.15 })"
    , ""
    , "scene('tunnel')"
    ]
  , [ "// L05 Visuals that follow the sound"
    , "// To write your own, add a scene in Main.hs"
    , "bpm(120)"
    , ""
    , "drum('kick',  'x...x...x...x...')"
    , "drum('snare', '....x.......x...')"
    , "drum('hat',   '..x...x...x...x.')"
    , ""
    , "scene('stars')"
    ]
  , [ "// L06 Spinning light"
    , "bpm(128)"
    , ""
    , "drum('kick',  'x...x...x...x...')"
    , "drum('clap',  '....x.......x...')"
    , "drum('hat',   'x.xxx.xxx.xxx.xx', 0.5)"
    , "synth('bass', 'E2 . E2 E2 . . E2 . G2 . G2 G2 . . D2 .', { wave: 'square', cutoff: 500 })"
    , ""
    , "scene('spin')"
    ]
  , [ "// L07 Build up, then drop"
    , "// Remove the // one group at a time, then Ctrl + Enter"
    , "bpm(126)"
    , ""
    , "// --- STEP A: start quietly ---"
    , "synth('pad', 'D3 . . . . . . . A2 . . . . . . .', { wave: 'triangle', len: 2, vol: 0.2 })"
    , ""
    , "// --- STEP B: the rhythm comes in ---"
    , "// drum('hat', '..x...x...x...x.')"
    , "// drum('kick', 'x.......x.......')"
    , ""
    , "// --- STEP C: build up (1 2 3) ---"
    , "// drum('snare', 'x.x.x.x.x.x.x.x.')"
    , "// drum('clap',  'xxxxxxxxxxxxxxxx', 0.5)"
    , "// synth('rise', 'D5 E5 F5 G5 A5 B5 C6 D6', { wave: 'sawtooth', vol: 0.1, len: 0.1 })"
    , ""
    , "// --- STEP D: drop! (comment out 1 2 3 first) ---"
    , "// drum('kick', 'X...x...X...x...')"
    , "// drum('snare', '....x.......x...')"
    , "// synth('bass', 'D2 . D2 D3 . D2 . . F2 . F2 F3 . C2 . C3', { wave: 'sawtooth', cutoff: 900, vol: 0.4 })"
    , ""
    , "scene('pulse')"
    ]
  , [ "// L08 Full set: music and visuals together"
    , "bpm(122)"
    , ""
    , "drum('kick',  'X...x...X...x..x')"
    , "drum('snare', '....x.......x...')"
    , "drum('hat',   'x.x.x.xxx.x.x.xx', 0.6)"
    , "drum('clap',  '............x...', 0.7)"
    , ""
    , "synth('bass', 'F1 . F2 . F1 . . F2 G#1 . G#2 . A#1 . C2 .', { wave: 'sawtooth', cutoff: 800, vol: 0.4 })"
    , "synth('lead', 'C5 ? . ? G#4 ? . . F4 ? . ? A#4 . ? .', { wave: 'square', cutoff: 3000, vol: 0.12, len: 0.12 })"
    , "synth('pad',  'F3 . . . . . . . C#3 . . . . . . .', { wave: 'triangle', len: 2, vol: 0.15 })"
    , ""
    , "scene('orbit')"
    ]
  ]
