{ kpkgs ? import ./deps/kpkgs {}, txg ? import ./. {}, ... }:

let pkgs = kpkgs.pkgs;
  in pkgs.dockerTools.buildImage {
  name = "mempool-p2p-tester-with-nix";
  tag = "latest";

  contents = [pkgs.curl pkgs.sqlite];
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    ${pkgs.dockerTools.shadowSetup}
    mkdir -p /txg
  '';

  config = {
    Cmd = [ "--help" ];
    WorkingDir = "/txg";
    Volumes = { "/txg" = { }; };
    Entrypoint = [ "${pkgs.haskell.lib.justStaticExecutables txg}/bin/mempool-p2p-tester"];
  };

}
