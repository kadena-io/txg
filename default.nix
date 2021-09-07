{ kpkgs ? import ./deps/kpkgs {}
}:
let pkgs = kpkgs.pkgs;
    haskellPackages = kpkgs.rp.ghc8_6;
    nix-thunk = import ./deps/nix-thunk {};
in haskellPackages.developPackage {
  name = builtins.baseNameOf ./.;
  root = kpkgs.gitignoreSource ./.;
  overrides = self: super: with pkgs.haskell.lib; rec {
    optparse-applicative = self.callHackageDirect {
        pkg = "optparse-applicative";
        ver = "0.15.1.0";
        sha256 = "1mii408cscjvids2xqdcy2p18dvanb0qc0q1bi7234r23wz60ajk";
    } {};
    base64 = self.callHackageDirect {
      pkg = "base64";
      ver = "0.4.2.3";
      sha256 = "1i4cf1xfbkxlxshwlsxgw2w5gi3hkkfm1n99vnzq7rixz8nxcw7r";
    } {};
    ghc-byteorder = self.callHackageDirect {
      pkg = "ghc-byteorder";
      ver = "4.11.0.0.10";
      sha256 = "1inrpb74i811k0gh8iazfqayrqrl86pwsm3zqgxn0aivxjh2ygsz";
    } {};
    hostaddress = overrideCabal (enableCabalFlag (enableCabalFlag (doJailbreak (self.callHackageDirect {
      pkg = "hostaddress";
      ver = "0.1.0.0";
      sha256 = "13wqzkw32inc8v03vl94ibzxdy0hmiybch62c8rana38r4yn4fnl";
    } {})) "with-configuration-tools") "with-aeson") (drv: {
      librarySystemDepends = drv.librarySystemDepends or [] ++ [self.aeson self.configuration-tools optparse-applicative];
    });
    pact = dontCheck super.pact;
    sqlite-simple = self.callHackageDirect {
      pkg = "sqlite-simple";
      ver = "0.4.18.0";
      sha256 = "1crp86argxqv5ryfiyj5v17a3wb8ngnb1zbhhx6d99i83skm5i86";
    } {};
  };
  source-overrides = {
    chainweb-api = nix-thunk.thunkSource ./deps/chainweb-api;
    pact = nix-thunk.thunkSource ./deps/pact;
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  });
}
