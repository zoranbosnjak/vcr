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
cabal2nix https://{path-to-git-repo}/{package} --revision {rev} > nix/package-ver.nix

# or if subdirectory is included
cabal2nix https://{path-to-git-repo}/{package} --revision {rev} --subpath {path} > nix/package-ver.nix

# or for local development version
cabal2nix path/to/package > nix/package-ver.nix
```

## Default build using nix-build

```bash
# default
nix-build
./result/bin/{prog} --version

# override git revision
nix-build --argstr gitrev {some-revision}
./result/bin/{prog} --version

# build from gitlab CI
nix-build --argstr gitrev $CI_COMMIT_SHORT_SHA
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
ghcid "--command=ghci -Wall -ilib -ivcr-app vcr-app/Main.hs"
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
runhaskell -Wall -ilib -ivcr-app ./vcr-app/Main.hs custom --program "/path/to/custom.hs --custom args" --ghcOpts "-i/path/to -more-opts" --run

# or
nix-build
./dist.../vcr custom --program /path/to/custom.hs --ghcOpts "-i/path/to/lib -more-opts" --run
```

## Changes in cabal file

When changing `*.cabal` file, run:

```bash
cabal2nix lib > lib.nix
cabal2nix . > app.nix
```

# Running replay in devel mode (inside nix-shell)
```bash
nix-shell
runhaskell -Wall -ilib -ivcr-app ./replay/example.hs --asterix path/to/xml
```

# Troubleshooting

```bash
# get event index from command line
curl localhost:12345/nextIndexFromUtc?t="2020-04-17T10:00:00Z"
# example output: [[2020,4,17,9,55,3],8640065]

# get events
curl localhost:12345/events
curl localhost:12345/events?includeIndex
curl "localhost:12345/events?includeIndex&ch=ch1"
curl "localhost:12345/events?includeIndex&ch=ch1|ch2"
curl "localhost:12345/events?includeIndex&ch=ch1|ch2&ix=\[\[2020,4,17,9,55,3\],8640065\]"

# skip event (get next index from a given index)
curl "localhost:12345/nextIndexFromIndex?ix=\[\[2020,4,17,9,55,3\],8640065\]"
```

