# Advent of code 2021

My solutions for AoC 2021.

I am using Haskell as I am learning the language.  

# Running the code

I am using Haskell (GHC 8.10.7) with `split`.

For each day, simply compile and execute the binary:
```
> ghc .\day_01.hs
[1 of 1] Compiling Main             ( day_01.hs, day_01.o )
Linking day_01.exe ...
> .\day_01.exe
(the answer)
```

## Docker image

GHCI
> docker run -it --rm haskell:8.10

## Developing

Setup:
```
> cabal v2-update
> cabal v2-build -j
```

Execute:
```
> cabal v2-run
> cabal v2-run advent-of-code2021 -- all
> cabal v2-run advent-of-code2021 -- last
```
