# シリーズ25：量子力学 ─ Haskell コード集

電子は 1 個ずつ点として届くのに、たまると波の縞になる。波動関数 ψ を複素数の「回る矢印」として描き、シュレーディンガー方程式を FFT で解き、トンネル効果・固有状態・スピン・もつれ・量子回路・水素原子の軌道までを、Haskell（gloss）で自分の手で動かして体験するコード集です。単位はすべて ħ = m = 1（水素原子はボーア半径 a0 = 1）です。

p5.js 版：https://r-shibusawa.github.io/p5-interactive-art/series25/

## 使い方

```
cabal run s25-00
```

番号を変えると別のスケッチ（`s25-01`、`s25-15` など）が動きます。

## スケッチ一覧

| # | タイトル | 内容 | 操作 | 使うもの |
|---|---|---|---|---|
| 00 | [波と粒子：二重スリット](sketches/00_double_slit/Main.hs) | 1 個ずつの点が縞になる、見張ると縞が消える | 1〜3 W ↑↓ ←→ T C | |
| 01 | [複素数と位相](sketches/01_complex_phase/Main.hs) | 回る矢印、らせんと色の帯、うなり | 1・2 ↑↓ ←→ D マウス スペース | |
| 02 | [波束と不確定性](sketches/02_wave_packet/Main.hs) | 位置と運動量（FFT）、Δx·Δp ≥ 1/2、広がる波束 | ←→＝σ ↑↓＝k0 スペース G R | |
| 03 | [シュレーディンガー方程式（1 次元）](sketches/03_schrodinger_1d/Main.hs) | 分割演算子法、5 つのポテンシャル | ドラッグ 1〜5 ↑↓ ←→ スペース R | |
| 04 | [トンネル効果](sketches/04_tunneling/Main.hs) | 反射と透過、理論の透過率と比べる | ↑↓＝V0 ←→＝幅 W S スペース P | |
| 05 | [無限井戸の固有状態](sketches/05_infinite_well/Main.hs) | En ∝ n²、重ね合わせの揺れ、リバイバル | クリック 1〜6 G ↑↓ スペース R | |
| 06 | [調和振動子](sketches/06_harmonic_oscillator/Main.hs) | エルミート関数、等間隔のはしご、コヒーレント状態 | 1〜3 ↑↓＝n ←→＝α K スペース | |
| 07 | [固有値を数値で求める（シューティング法）](sketches/07_shooting_eigen/Main.hs) | 描いた V(x) の固有値を Numerov 法＋二分法で | ドラッグ 1〜5 C ↑↓ P | |
| 08 | [スピンとブロッホ球](sketches/08_bloch_sphere/Main.hs) | 3D の球の上でゲート・測定・歳差運動 | X Y Z H S T M K B ←→ ↑↓ R ドラッグ | 3D |
| 09 | [シュテルン＝ゲルラッハ実験](sketches/09_stern_gerlach/Main.hs) | z → x → z で情報が消える | クリック ←→ 1〜4 ↑↓ C | |
| 10 | [量子もつれとベルの不等式](sketches/10_bell_chsh/Main.hs) | E(a,b) = −cos(a−b)、CHSH の S = 2√2 | ←→＝a ↑↓＝b H スペース C | |
| 11 | [量子コンピュータ入門](sketches/11_quantum_circuit/Main.hs) | 3 量子ビットの回路、ベル・GHZ・ドイチュ＝ジョサ・グローバー | クリック 1〜7 E B G D V C M | |
| 12 | [水素原子の軌道](sketches/12_hydrogen_orbitals/Main.hs) | ψnlm の断面、動径分布、バルマー系列 | ↑↓＝n ←→＝l M P Z G | |
| 13 | [量子の音](sketches/13_quantum_sound/Main.hs) | 準位 → 音の高さ、重ね合わせ → 和音、トンネル → うなり | クリック 1〜3 ←→ ↑↓ スペース | 🔊 |
| 14 | [波動関数ブラシ](sketches/14_psi_brush/Main.hs) | V と波を描いて時空図の絵にする。自由制作のベース | ドラッグ V W ←→ スペース 1〜3 S C | 🔊 |
| 15 | [カメラで回折](sketches/15_webcam_diffraction/Main.hs) | カメラの映像を穴にして 2D FFT の回折像 | V 1〜5 ドラッグ E C ←→ ↑↓ I L | 📷 |

## p5.js 版との違い

- 画面の文字は英語です（gloss の文字は ASCII のみ）。ψ は psi、⟨E⟩ は `<E>`、|0⟩ は `|0>` と書き、長い説明は短くして 2〜3 行に分けました。小さい文字は 14 以上にしたので、位置が少し動いている所があります。
- 角丸の四角（02・08 のパネル、09 の箱、11 のパレットとゲート）は普通の四角です。
- 00：スクリーンにたまる点は、絵を描きためる代わりに画素の配列（32 × 高さ）にためて毎フレーム表示します。点の大きさはおよそ 3 画素です。
- 02・03・04・14・15 の FFT は Float で計算します（p5.js は 64 ビット）。回転因子は漸化式ではなく、あらかじめ作った cos・sin の表から読みます。式と手順は同じです。
- 07：Numerov 法は 1 との小さな差を積み重ねるので Float では桁が足りません。ここだけ Double で計算します。
- 08：WEBGL の代わりに `V3` と `project` で投影し、線を短く切って奥から順に描きます（画家のアルゴリズム）。球はすべて平らな円です。
- 13：連続音の代わりに `Art.Sound` の短い音をくり返します（1・2 は 0.7 秒ごと、3 はうなりが聞こえるよう 1.3 秒ごと）。音の頭で切れ目が入り、鳴り始めまで少し遅れます。同時に鳴るのは 8 音までです。音量は `loudness` で決めています。
- 14：音は 0.25 秒ごとの短い三角波で、左右の振り分け（パン）はありません。時空図は 360 行ではなく 300 行で、同じ高さに引き伸ばします。S の保存は PNG ではなく SVG です。
- 15：カメラは ffmpeg で読みます（`Art.Device`）。カメラが動いていないときは p5.js 版と同じく手描きの穴を使います（`Art.Device` の合成映像は使いません）。

## 参考

Griffiths『Introduction to Quantum Mechanics』、Sakurai『Modern Quantum Mechanics』、Nielsen & Chuang『Quantum Computation and Quantum Information』。

## ライセンス

MIT License ─ 授業・ワークショップで自由に改変・再利用してください。
