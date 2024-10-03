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
# ekg, ekg-json - marked broken in nixpkgs
cabal2nix https://github.com/l0negamer/ekg > nix/extra/ekg.nix
cabal2nix https://github.com/L0neGamer/ekg-json > nix/extra/ekg-json.nix

# wxHaskell - marked broken in nixpkgs
hsh=...
nix-prefetch-git --rev $hsh https://codeberg.org/wxHaskell/wxHaskell.git \
    > nix/extra/wxHaskell.json
cabal2nix --revision $hsh https://codeberg.org/wxHaskell/wxHaskell \
    --subpath wxcore > nix/extra/wxcore.nix
vi nix/extra/wxcore.nix
# append the following mkDerivation attribute
# (to workaround gcc arg too long problem)
__propagatePkgConfigDepends = false;

# keera-hails - marked broken in nixpkgs
hsh=...
nix-prefetch-git --rev $hsh https://github.com/zoranbosnjak/keera-hails.git \
    > nix/extra/keera-hails.json
```
