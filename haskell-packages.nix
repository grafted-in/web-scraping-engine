{ pkgs
, withHoogle ? false
, ...
}:

let

  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      web-scraping-engine = dontCheck (self.callPackage ./lib { haskellPackages = self; });

      # add customized packages here
    }
    //
    (
      if withHoogle
      then {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      } else {}
    );
  };

in myHaskellPackages
