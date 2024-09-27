# Update procedures

## sources.json

Run commands in project base (outside of `nix/`) directory.

```bash
# update niv
nix-shell -p niv --run "niv init"

# update packages
nix-shell -p niv --run "niv update nixpkgs -b master"
nix-shell -p niv --run "niv update nixpkgs -b release-..."
```

## haskell packages

```bash
# (marked broken in nixpkgs)
cabal2nix https://github.com/l0negamer/ekg > nix/extra/ekg.nix
cabal2nix https://github.com/L0neGamer/ekg-json > nix/extra/ekg-json.nix

# (marked broken in nixpkgs)
# cabal2nix https://codeberg.org/wxHaskell/wxHaskell --subpath wx > nix/extra/wx.nix
# cabal2nix https://codeberg.org/wxHaskell/wxHaskell --subpath wxcore > nix/extra/wxcore.nix
# cabal2nix https://codeberg.org/wxHaskell/wxHaskell --subpath wxdirect > nix/extra/wxdirect.nix
# (does not work) cabal2nix https://codeberg.org/wxHaskell/wxHaskell --subpath wxc > nix/extra/wxc.nix

cabal2nix https://github.com/zoranbosnjak/deseo > nix/extra/deseo.nix
```
