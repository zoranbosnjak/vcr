# Versioning

This project uses versions as specified in `https://semver.org/`.
In short:

Given a version number *MAJOR*.*MINOR*.*PATCH*, increment the:

- MAJOR version when you make incompatible API changes,
- MINOR version when you add functionality in a backwards-compatible manner, and
- PATCH version when you make backwards-compatible bug fixes.

# Development

To update revision of `nixpkgs`, update `nixpkgs.json` file.

To update particular `package` revision, which is included in nix rules:

```bash
cabal2nix https://{path-to-git-repo}/{package} --revision {rev} > package-ver.nix

# or for local development version
cabal2nix path/to/package > package-ver.nix
```

## Default build using nix-build

```bash
# default
nix-build release.nix
./result/bin/{prog} --version

# override git revision
nix-build release.nix --argstr gitrev {some-revision}
./result/bin/{prog} --version

# build from gitlab CI
nix-build release.nix --argstr gitrev $CI_COMMIT_SHORT_SHA
```

### Development build from nix-shell

```bash
nix-shell [--argstr compiler ghc881]
cabal configure
cabal build -j
```

### Use ghcid environment

```bash
nix-shell
ghcid "--command=ghci -Wall -ilib vcr-app/Main.hs"
```

### Running (inside nix-shell)
```bash
nix-shell
cabal build -j && ./dist... +RTS -M300m -RTS {args}
runhaskell -ilib vcr-app/Main.hs {args}
```

### Running custom script (inside nix-shell)
```bash
nix-shell
runhaskell -Wall -ilib -ivcr-app ./vcr-app/Main.hs custom --program /path/to/custom.hs --ghcOpts "-O2 -v1 -i/path/to"

# or
nix-build
./dist.../vcr custom --program /path/to/custom.hs --ghcOpts "-O2 -v1 -i/path/to -i/path/to/lib"
```

## Changes in cabal file

When changing `*.cabal` file, run:

```bash
nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
# or
cabal2nix lib > lib.nix
cabal2nix . > app.nix
```

