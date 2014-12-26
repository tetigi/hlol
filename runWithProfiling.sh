#!/bin/bash
cabal clean
cabal configure --enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all

# Perform profile
cabal run -- +RTS -h -p

# View
vim hlol.prof

# Clean up
rm hlol.hp
cabal clean
