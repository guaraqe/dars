{ forShell ? false }:

with import ./nix;
with builtins;

let
  # Set of package sources parsed from cabal.project.
  targets =
    import ./nix/cabal.nix;

  # Add local packages to haskellPackages.
  localHaskellPackages =
    haskellPackages.extend (haskell.lib.packageSourceOverrides targets);

  # Set of local packages, built from targets.
  packages =
    mapAttrs (name: _: localHaskellPackages.${name}) targets;

  # Shell for developing the local packages.
  shell =
    localHaskellPackages.shellFor
      {
        packages = _: attrValues packages;
        withHoogle = true;
        buildInputs =
          [
            # Haskell
            localHaskellPackages.ghc
            localHaskellPackages.cabal-install
            localHaskellPackages.ghcid
            localHaskellPackages.ormolu
            localHaskellPackages.cabal-fmt
            # Database
            postgresql
            # Deployment
            flyctl
          ];
      };
in
  if forShell
  then shell
  else packages
