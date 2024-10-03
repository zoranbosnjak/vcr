# Versioning

This project uses versions as specified in `https://semver.org/`.
In short:

Given a version number *MAJOR*.*MINOR*.*PATCH*, increment the:

- MAJOR version when you make incompatible API changes,
- MINOR version when you add functionality in a backwards-compatible manner, and
- PATCH version when you make backwards-compatible bug fixes.

# Development

To update revision of `nixpkgs` or other packages, see `nix/README.md`.

## Default build using nix-build

```bash
# default
nix-build
./result/bin/vcr -h

# override git revision
nix-build --argstr gitrev {some-revision}
./result/bin/vcr --version

# build from gitlab CI
nix-build --argstr gitrev $CI_COMMIT_SHORT_SHA
```

## Development from nix-shell

```bash
nix-shell

# cabal build
cabal configure
cabal build -j

# auto adjust style
stylish-haskell --inplace {path}
find . | grep "\.hs$" | xargs stylish-haskell --inplace

# lint
hlint {path}
find . | grep "\.hs$" | xargs hlint

# ghcid monitor
ghcid --lint=hlint --command="ghci -Wall -ilib -iapp app/Main.hs"

# Running unit/property tests
ghcid --lint=hlint --command="ghci -Wall -ilib -itest test/Main.hs"
runhaskell -Wall -ilib -itest test/Main.hs
cabal test --test-show-details=always

# Running
cabal build -j
vcr=$(find dist-newstyle/ -executable -type f | grep "vcr$")
${vcr} +RTS -M300m -RTS -h
runhaskell -ilib -iapp app/Main.hs -h

# Running replay example
# In real configuration, this will be part of a separate project
# with some additional external libraries (like Asterx processing)
ghcid --lint=hlint --command="ghci -Wall -ilib examples/replay.hs"
runhaskell -ilib examples/replay.hs
```

# Http server API

- ping
- startTime
- uptime
- limits
- middle
- peek
- events
- next

`curl` examples (with running 'vcr server' on 'localhost:12345')

```bash
curl localhost:12345/ping

curl localhost:12345/limits
# example output: [[2021,1,6,9,19,28,0],[2021,1,6,9,21,28,21725]]

curl "localhost:12345/middle?ix1=\[2021,1,6,9,19,28,0\]&ix2=\[2021,1,6,9,21,28,21725\]"
# example output: [2021,1,6,9,20,28,52771]

curl "localhost:12345/peek?ix=\[2021,1,6,9,20,28,52771\]"

curl "localhost:12345/next?t=\"2021-01-06T09:20:29.2250Z\""
# example output: [2021,1,6,9,20,28,7653]

# get events
curl "localhost:12345/events"
curl localhost:12345/events?includeIndex
curl "localhost:12345/events?includeIndex&channels=ch1"
curl "localhost:12345/events?includeIndex&channels=ch1|ch2"
```
