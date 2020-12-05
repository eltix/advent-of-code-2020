#!/usr/bin/env python3

import sys


def insert_in_file(d, file_name, match_functions):
    with open(file_name, "r") as f:
        content = f.readlines()
    with open(file_name, "w") as f:
        for line in content:
            f.write(line)
            for (g, suffix) in match_functions:
                if line.startswith(g(d - 1)):
                    f.write(g(d) + suffix)


def main():
    d = int(sys.argv[1])

    main_file = "app/Main.hs"
    g = (lambda d: f"import qualified Day{d:02}        (computeSolutions)", "\n")
    h = (lambda d: f"  , printSolution Day{d:02}.computeSolutions", "\n")
    insert_in_file(d, main_file, [g, h])

    test_file = "test/Main.hs"
    g = (lambda d: f"import qualified Day{d:02}", "\n")
    h = (
        lambda d: f'  , testCase "Day {d}" $ Day{d:02}.computeSolutions >>= (@?= ',
        "(0, 0))\n",
    )
    insert_in_file(d, test_file, [g, h])

    cabal_file = "advent-of-code2020.cabal"
    g = (lambda d: f"    day{d:02},", "\n")
    insert_in_file(d, cabal_file, [g])

    cabal_library = f"""
library day{d:02}
  import:          days-properties
  exposed-modules: Day{d:02}
  build-depends:
    utils
"""
    with open(cabal_file, "a") as f:
        f.writelines(cabal_library)

    haskell_module = f"""module Day{d:02} where

import           BasicPrelude

computeSolutions :: IO (Int, Int)
computeSolutions = do
  let
    -- part 1
    sol1 = 0
    -- part 2
    sol2 = 0
  return (sol1, sol2)
"""
    haskell_module_file = f"src/Day{d:02}.hs"
    with open(haskell_module_file, "w") as f:
        f.writelines(haskell_module)


if __name__ == "__main__":
    main()
