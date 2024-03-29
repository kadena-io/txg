{ compiler ? "ghc884"
, kpkgs ? import ./deps/kpkgs {}
, rev ? "7e9b0dff974c89e070da1ad85713ff3c20b0ca97"
, sha256 ? "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36"
, withHoogle ? false
, pkgs ?
  import (builtins.fetchTarball {
    url ="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;}) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
}:
let nix-thunk = import ./deps/nix-thunk {};
    gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
      sha256 = "04n9chlpbifgc5pa3zx6ff3rji9am6msrbn1z3x1iinjz2xjfp4p";
    }) {};
in pkgs.haskell.packages.${compiler}.developPackage {
  withHoogle = false;
  name = builtins.baseNameOf ./.;
  root = gitignoreSrc.gitignoreSource ./.;
  overrides = self: super: with pkgs.haskell.lib; rec {
    aeson = dontCheck (self.callHackageDirect {
      pkg = "aeson";
      ver = "1.5.6.0";
      sha256 = "18yb8j0jvvzp275ylj16hskgxwdy55hljch9bjvpxl25vvslnk1n";
    } {});
    base64 = self.callHackageDirect {
      pkg = "base64";
      ver = "0.4.2.3";
      sha256 = "1i4cf1xfbkxlxshwlsxgw2w5gi3hkkfm1n99vnzq7rixz8nxcw7r";
    } {};
    configuration-tools = dontBenchmark (dontCheck (self.callHackageDirect {
      pkg = "configuration-tools";
      ver = "0.6.0";
      sha256 = "0ia2bhy35qv1xgbqrx0jalxznj8zgg97y0zkp8cnr1r3pq5adbcd";
      } {}));
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
    optparse-applicative = dontCheck (self.callHackageDirect {
      pkg = "optparse-applicative";
      ver = "0.15.1.0";
      sha256 = "1mii408cscjvids2xqdcy2p18dvanb0qc0q1bi7234r23wz60ajk";
    } {});
    pact = overrideCabal (enableCabalFlag (dontCheck (doJailbreak super.pact)) "build-tool") (drv:
      { patches = drv.patches or [] ++ [./pact-base16-bytestring.patch]; });
    pretty-simple = dontCheck (self.callHackageDirect {
      pkg = "pretty-simple";
      ver = "3.3.0.0";
      sha256 = "19zwzzvjgadmzp9gw235bsr6wkljr8k0cqy75h5q8n0d5m60ip7m";
    } {});
    prettyprinter = dontCheck (self.callHackageDirect {
      pkg = "prettyprinter";
      ver = "1.6.0";
      sha256 = "0f8wqaj3cv3yra938afqf62wrvq20yv9jd048miw5zrfavw824aa";
    } {});
  };
  source-overrides = {
    chainweb-api = nix-thunk.thunkSource ./deps/chainweb-api;
    pact = nix-thunk.thunkSource ./deps/pact;
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
      pkgs.haskell.packages.${compiler}.ghcid
    ];
  });
}
