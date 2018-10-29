# VCR recorder

## Versioning

This project uses versions as specified in `https://semver.org/`.
In short:

Given a version number *MAJOR*.*MINOR*.*PATCH*, increment the:

- MAJOR version when you make incompatible API changes,
- MINOR version when you add functionality in a backwards-compatible manner, and
- PATCH version when you make backwards-compatible bug fixes.

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
cabal configure --enable-tests
cabal build -j
```

### Running (inside nix-shell)
```bash
cabal build -j && ./dist/build/vcr/vcr +RTS -M300m -RTS {args}
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
cabal test -j --test-option=--color=always --show-details=always
# or
cabal build -j && ./dist/build/test-vcr/test-vcr
```

## nix installation
```bash
nix-env -f release0.nix -iA vcr
```
