# Data converter

## Development

To update revision of `nixpkgs`, (re)run (with new git release):

```bash
# select branch
# check info on https://github.com/NixOS/nixpkgs-channels/branches
# select revision (git log...)
nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git <rev> > nixpkgs.json
```

### Default build

```bash
nix-shell
cabal configure
cabal build -j
```

### Running (inside nix-shell)
```bash
cabal build -j && ./dist/build/vcr/vcr -h
```

## Changes in cabal file

When changing `*.cabal` file, run:

```bash
cabal2nix . > default.nix
```

## Running tests (examples)

### basic usage

```bash
cabal test -j
```

### color output with details

```bash
cabal test -j --test-option=--color --show-details=always
```

### require test more iterations
```bash
cabal test -j --test-option=--color --show-details=always --test-option=--maximum-generated-tests=10000 --test-option=--maximum-unsuitable-generated-tests=10000000
```

## nix installation
```bash
nix-env -f release0.nix -iA vcr
```
