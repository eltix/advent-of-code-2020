# advent-of-code2020

Solutions for Advent of Code 2020 in Haskell

## Installation

We use `Nix` to manage the installation of `ghc` and `cabal` for us.
If you have `Nix` installed on your machine, simply run `nix-shell` from the root of this repository.  
If you don't, you'll have to install `cabal` and `ghc` on your own.

## Solutions to the Advent of Code 2020 puzzles

Run this command

```
cabal run day <day>
```

with `<day>` in {1, ..., 25} to get the solutions to
both part 1 and part 2 of the day.

## Testing

```
cabal test all --test-show-details=streaming
```

will compute all days solutions and check that they are correct.
