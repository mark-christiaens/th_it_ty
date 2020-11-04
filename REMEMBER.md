# Stuff to remember

## Create a new project

mkdir myproject
cabal init --cabal-version=2.4 --license=NONE -p exercise
cabal configure --ghc-options=-haddock
cabal build
