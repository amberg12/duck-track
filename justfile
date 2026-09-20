build:
    cabal build --project-dir=duck-track-generator exe:generator
    cp $(cabal list-bin --project-dir=duck-track-generator exe:generator) .