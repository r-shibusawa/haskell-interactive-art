#!/usr/bin/env python3
"""series*/sketches/NN_slug/Main.hs を探して cabal ファイルを作りなおす。実行ファイル名は s<シリーズ>-<番号>。"""
import glob, re
head = """cabal-version: 2.4
name:          haskell-interactive-art
version:       1.0.0.0
synopsis:      Interactive art and mathematics sketches in Haskell (gloss)
license:       MIT
license-file:  LICENSE
author:        r-shibusawa
build-type:    Simple

common shared
  default-language: GHC2021
  ghc-options:      -O1 -threaded
  default-extensions: BangPatterns
  build-depends:    base >=4.16 && <5, gloss >=1.13, containers, array, vector, bytestring, random

library
  import:          shared
  hs-source-dirs:  src
  exposed-modules: Art, Art.Sketch, Art.Draw, Art.Sound, Art.Device, Art.Svg, Art.Runtime
  build-depends:   process, directory, time, OpenGL
"""
out = [head]
paths = sorted(glob.glob("series*/sketches/*/Main.hs"), key=lambda p: (int(re.search(r"series(\d+)", p).group(1)), p))
for p in paths:
    m = re.search(r"series(\d+)/sketches/(\d+)_", p)
    d = p.rsplit("/", 1)[0]
    out.append(f"executable s{int(m.group(1))}-{m.group(2)}\n  import:         shared\n  hs-source-dirs: {d}\n  main-is:        Main.hs\n  build-depends:  haskell-interactive-art\n")
open("haskell-interactive-art.cabal", "w").write("\n".join(out))
print(len(paths), "executables")
