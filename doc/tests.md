# Run tests with `cabal`

## Basic usage

    cabal test -j

## Color output

    cabal test --test-option=--color --show-details=always -j

## Run more tests

    cabal test --test-option=--color --show-details=always -j --test-option=--maximum-generated-tests=10000 --test-option=--maximum-unsuitable-generated-tests=10000000

