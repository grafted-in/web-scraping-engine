
packageDir:  # path to package directory to build

{ pkgs              ? import ./nixpkgs.nix {}
, haskellPackages   ? pkgs.callPackage ./haskell-packages.nix {}
, extraBuildDepends ? []
, ...
}:
let

  cleanHaskellSourceFilter = name: type:
    let
      baseName = baseNameOf (toString name);
    in   baseName != "dist"
      && baseName != ".stack-work"
    ;

  composeFilters = f1: f2: name: type: (f1 name type) && (f2 name type);

  cleanHaskellSource = builtins.filterSource
    (composeFilters pkgs.lib.sources.cleanSourceFilter cleanHaskellSourceFilter);

in pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "haskell-package" packageDir {}) (
  { src, buildDepends ? [], ...}: {
    src = cleanHaskellSource src;
    buildDepends = buildDepends ++ extraBuildDepends;
  }
)
