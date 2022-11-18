cd grammar
bnfc -m --haskell LambdaNat?.cf
make
cd ..
cp grammar/*.hs src
cabal build
