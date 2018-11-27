# VCR recorder

## Versioning

This project uses versions as specified in `https://semver.org/`.
In short:

Given a version number *MAJOR*.*MINOR*.*PATCH*, increment the:

- MAJOR version when you make incompatible API changes,
- MINOR version when you add functionality in a backwards-compatible manner, and
- PATCH version when you make backwards-compatible bug fixes.

## Default build from nix

```bash
nix-build
```

## Development

### Changes in cabal file

When changing `*.cabal` file, run:

```bash
cabal2nix . > generated.nix
```

### Updating `nixpkgs`

(re)run (with new git release):

```bash
# select branch
# check info on https://github.com/NixOS/nixpkgs-channels/branches
# select revision (git log...)
nix-prefetch-git https://github.com/NixOS/nixpkgs-channels.git <rev> > nixpkgs.json
```

### Runing local hoogle server

Start server, for example on localhost, port 8080:

```bash
nix-shell --arg withHoogle true
hoogle server -p 8080 --local

# user web browser on localhost:8080
```

### Building and testing from shell

```bash
nix-shell
rm -rf dist/
cabal configure --enable-tests
cabal build -j

cabal build -j && cabal test -j
cabal build -j && cabal test -j --test-option=--color=always --show-details=always
cabal build -j && ./dist/build/test-vcr/test-vcr
```

### Running (inside nix-shell)

```bash
cabal build -j && ./dist/build/vcr/vcr +RTS -M300m -RTS {args}
```

### Use ghcid environment (inside nix-shell)

```bash
ghcid "--command=ghci -Wall -isrc Main"
```

## nix installation

```bash
nix-env -f default.nix -iA vcr
```

## Configuration change

Edit config file, then send SIGHUP signal to the procress.

```bash
# find pid
ps aux | grep vcr

# send signal
kill -s SIGHUP <pid>

# or as one line bash
ps aux | grep vcr | grep RTS | head -n 1 | awk '{print $2}' | xargs kill -s SIGHUP
```

