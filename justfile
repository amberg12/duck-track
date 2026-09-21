[doc('Build The Project')]
[group('build')]
build:
    cabal build --project-dir=duck-track-generator exe:generator
    cp (cabal list-bin --project-dir=duck-track-generator exe:generator) .

[doc('Format The Project')]
[group('dev')]
format:
    find . -name '*.hs' -not -path './duck-track-generator/dist-newstyle/*' -print0 | xargs -0 fourmolu --config fourmolu.yaml --mode inplace