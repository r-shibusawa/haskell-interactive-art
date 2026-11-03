#!/bin/zsh
# 使い方: tools/check.sh <シリーズ番号> [NN ...]
# 各スケッチを ghc で直接コンパイルし（ビルド済みライブラリを使うので並列実行できる）、
# ウィンドウを開かないテストモードで実行する。ライブラリを変えたら cabal build lib:haskell-interactive-art を先に。
cd "$(dirname "$0")/.."
source tools/env.sh
N=$1; shift
FAIL=0
for d in series$N/sketches/*(/); do
  id=${${d:t}%%_*}
  if (( $# > 0 )) && (( ${@[(Ie)$id]} == 0 )); then continue; fi
  out=.build/s$N/$id; mkdir -p $out
  if ! ghc -O1 -threaded -hidir $out -odir $out -v0 -Wno-all $d/Main.hs -o $out/app 2>$out/err.txt; then
    echo "== $N/$id COMPILE ERROR"; grep -v "^ld: warning" $out/err.txt | head -30; FAIL=1; continue
  fi
  if [[ -n $ART_SVG_ROOT ]]; then mkdir -p $ART_SVG_ROOT/s$N/$id; export ART_SVG=$ART_SVG_ROOT/s$N/$id; fi
  ART_TEST=1 perl -e 'alarm 120; exec @ARGV' $out/app || { echo "== $N/$id RUNTIME FAIL"; FAIL=1; }
done
if grep -rlE "Claude|Anthropic|Co-Authored" series$N >/dev/null 2>&1; then echo "FORBIDDEN WORD"; FAIL=1; fi
exit $FAIL
