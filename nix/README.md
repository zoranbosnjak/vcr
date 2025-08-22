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
# wxHaskell - marked broken in nixpkgs
src=https://codeberg.org/wxHaskell/wxHaskell.git
hsh=HEAD
nix-prefetch-git --rev $hsh $src > nix/extra/wxHaskell.json
cabal2nix --revision $hsh $src --subpath wxcore > nix/extra/wxcore.nix
vi nix/extra/wxcore.nix
# append the following mkDerivation attribute
# (to workaround gcc arg too long problem)
__propagatePkgConfigDepends = false;

# keera-hails - marked broken in nixpkgs
hsh=HEAD
nix-prefetch-git --rev $hsh https://github.com/zoranbosnjak/keera-hails.git \
    > nix/extra/keera-hails.json
```
