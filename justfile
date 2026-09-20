build:
    cabal build
    cp $(cabal list-bin exe:duck-track) .