{ kpkgs ? import ./deps/kpkgs {}, txg ? import ./. {}, dockerTag ? "latest", ... }:

let pkgs = kpkgs.pkgs;
  in pkgs.dockerTools.buildImage {
  name = "txg-with-nix";
  tag = dockerTag;

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
    Entrypoint = [ "${pkgs.haskell.lib.justStaticExecutables txg}/bin/txg"];
  };

}
