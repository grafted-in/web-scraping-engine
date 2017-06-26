defaultNix:  # default.nix file to import for shell

{ pkgs            ? import ./nixpkgs.nix {}
, withHoogle      ? true
, haskellPackages ? pkgs.callPackage ./haskell-packages.nix { inherit withHoogle; }
, ...
}:
let

  package = pkgs.callPackage defaultNix {
    inherit haskellPackages;
    extraBuildDepends = [
      haskellPackages.ghcid
      haskellPackages.hlint_2_0_9
      haskellPackages.intero
      haskellPackages.stylish-haskell
      pkgs.cabal-install
      pkgs.stack
    ];
  };

in package.env
