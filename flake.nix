{
  description = "numis";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: 
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function rec {
          inherit system;
          compilerVersion = "ghc962";
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev: with pkgs.haskell.lib; {
              numis = hfinal.callCabal2nix "numis" ./. {};
              pandoc = dontCheck (hprev.callHackage "pandoc" "3.1.2" {});
              pandoc-types = hprev.callHackage "pandoc-types" "1.23.1" {};
            };
          };
        });
    in
    {
      # nix fmt
      formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

      # nix develop
      devShell = forAllSystems ({hsPkgs, pkgs, ...}:
        hsPkgs.shellFor {
          # withHoogle = true;
          packages = p: [
            p.numis
          ];
          buildInputs = with pkgs;
            [
              hsPkgs.haskell-language-server
              haskellPackages.cabal-install
              cabal2nix
              haskellPackages.ghcid
              haskellPackages.fourmolu
              haskellPackages.cabal-fmt
              pandoc
              texlive.combined.scheme-full
            ]
            ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}))
            ++ # SDL dependencies
            [ 
              SDL2
              glew
              pkg-config
            ];
        });

      # nix build
      packages = forAllSystems ({hsPkgs, ...}: {
          numis = hsPkgs.numis;
          default = hsPkgs.numis;
      });

      # You can't build the numis package as a check because of IFD in cabal2nix
      checks = {};

      # nix run
      apps = forAllSystems ({system, ...}: {
        numis-cli = { 
          type = "app"; 
          program = "${self.packages.${system}.numis}/bin/cli-exe"; 
        };
        numis-gui = { 
          type = "app"; 
          program = "${self.packages.${system}.numis}/bin/gui-exe"; 
        };
        default = self.apps.${system}.numis-cli;
      });
    };
}
