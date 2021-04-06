{
  description = "Package build for accelerate";
  nixConfig.bash-prompt = "\[nix-develop\]$ ";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-18.09";
  inputs.nixpkgs.flake = false;
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system; 
        # overlays = [ overlay ]; 
        config.allowUnfree = true; 
        config.allowBroken = true; 
      };
      ghcVersion = "ghc7103";

      gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

      myHaskellPackages = pkgs.haskell.packages.${ghcVersion}.override (old: {
        overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) 
          (hself: hsuper: {
            # cufft = pkgs.haskell.lib.overrideCabal hsuper.cufft (drv: {
            #   preConfigure = ''
            #     export CUDA_PATH=${pkgs.cudatoolkit}'';
            # });
            "transformers-compat" = hsuper.callPackage
              ({ mkDerivation, base, ghc-prim, transformers }:
              mkDerivation {
                pname = "transformers-compat";
                version = "0.6.2";
                sha256 = "sha256-3AYii3uKVG+dJXtP4rNp/CyyeSQLvkMSqo9HuydS5L4=";
                configureFlags = [
                  "-f-generic-deriving" 
                ];
                libraryHaskellDepends = [ base ghc-prim transformers ];
                description = "A small compatibility shim for the transformers library";
                license = pkgs.stdenv.lib.licenses.bsd3;
              }) {};
          });
      });

      pkg = myHaskellPackages.callCabal2nix "structures" (gitignore ./.) {};

    in {
      inherit myHaskellPackages pkgs nixpkgs;
      defaultPackage = pkg;
      packages = { inherit pkg; };
      devShell = myHaskellPackages.shellFor { # development environment
        packages = p: [ p.structures ];
        # nativeBuildInputs = [ pkgs.cudatoolkit_10_2 pkgs.cudatoolkit_10_2.lib ];
        buildInputs = with pkgs.haskellPackages; [
          # cabal-install
          # ghcid
          # haskell-language-server
          # ormolu
          # hlint
          # cuda
          # pkgs.nixpkgs-fmt
        ];
        withHoogle = false;
      };
      apps.repl = flake-utils.lib.mkApp {
        drv = pkgs.writeShellScriptBin "repl" ''
          confnix=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
          trap "rm $confnix" EXIT
          nix repl $confnix
        '';
      };
      # devShell = pkg.env.overrideAttrs (super: {
      #   nativeBuildInputs = with pkgs; super.nativeBuildInputs ++ [
      #     hs.cabal-install
      #     zlib
      #   ];
      # });
    }
  );
}
