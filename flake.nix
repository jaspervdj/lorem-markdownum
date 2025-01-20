{
  description = "lorem-markdownum";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskell.packages.ghc96;
        package = haskell.callCabal2nix "lorem-markdownum" ./. { };
        packageExes = pkgs.haskell.lib.justStaticExecutables package;
        share = pkgs.stdenv.mkDerivation {
          name = "share";
          src = ./.;
          installPhase = ''
            mkdir -p $out/usr/share/lorem-markdownum
            cp -r data/ $out/usr/share/lorem-markdownum/data/
            cp -r static/ $out/usr/share/lorem-markdownum/static/
          '';
        };
      in {
        packages = {
          default = package;
          docker = pkgs.dockerTools.buildLayeredImage {
            name = "jaspervdj/lorem-markdownum";
            tag = "latest";
            contents = [ share ];
            config = {
              Cmd = "${packageExes}/bin/lorem-markdownum-web";
              Env = [
                "LOREM_MARKDOWNUM_DATA_DIR=/usr/share/lorem-markdownum/data"
                "LOREM_MARKDOWNUM_STATIC_DIR=/usr/share/lorem-markdownum/static"
                "LOREM_MARKDOWNUM_BIND_ADDRESS=0.0.0.0"
                "LOREM_MARKDOWNUM_BIND_PORT=80"
              ];
            };
          };
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [ pkgs.zlib.dev ];
            packages = [
              pkgs.cabal-install
              pkgs.entr
              haskell.stylish-haskell
              (haskell.ghc.withPackages
                (p: package.buildInputs ++ [ p.random ]))
            ];
          };
        };
        formatter = pkgs.nixfmt;
      });
}
