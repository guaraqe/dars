_: pkgs:

let
  # Contains the auth with supports namedroutes
  servantSrc = pkgs.fetchFromGitHub {
    repo = "servant";
    owner = "haskell-servant";
    rev = "bd9151b9de579e98d14add3328933d155df25fc9";
    sha256 = "sha256-eGZGxKU5mvzDrL2q2omIXzJjbjwvmQzh+eYukYzb3Dc=";
  };

  overrides = _: hspkgs: with pkgs.haskell.lib;
    {
      servant-auth-server = doJailbreak (hspkgs.callCabal2nix "servant-auth-server" "${servantSrc}/servant-auth/servant-auth-server" {});
    };
in

{
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions
        (old.overrides or (_: _: {}))
        overrides;
  });
}
