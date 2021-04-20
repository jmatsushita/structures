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
            # "array" = hsuper.callPackage
            #   ({ mkDerivation, base }:
            #   mkDerivation {
            #     pname = "array";
            #     version = "0.4.0.1";
            #     sha256 = "sha256-OdEfH+cr7vAzPgvgKhia5pyrDiSBMvne+dvi9n9Inpw=";
            #     revision = "1";
            #     editedCabalFile = "sha256-XmULh2dklrXVwZqEItH5bavzBrJoHeoIvtli3ky9ypE=";
            #     libraryHaskellDepends = [ base ];
            #     description = "Mutable and immutable arrays";
            #     license = pkgs.stdenv.lib.licenses.bsd3;
            #     hydraPlatforms = pkgs.stdenv.lib.platforms.none;
            #   }) {};

            # "QuickCheck" = hsuper.callPackage
            #   ({ mkDerivation, base, containers, deepseq, random
            #   , template-haskell, tf-random, transformers, test-framework
            #   }:
            #   mkDerivation {
            #     pname = "QuickCheck";
            #     version = "2.7.6";
            #     sha256 = "sha256-gNiXCgwd/LcUIVzxHyvw95M59wwBmZ2uLDFRYQCgxSc=";
            #     libraryHaskellDepends = [
            #       base containers deepseq random template-haskell tf-random
            #       transformers
            #     ];
            #     testHaskellDepends = [ base test-framework ];
            #     description = "Automatic testing of Haskell programs";
            #     license = pkgs.stdenv.lib.licenses.bsd3;
            #   }) {};

            # "primitive" = hsuper.callPackage
            #   ({ mkDerivation, base, ghc-prim, transformers }:
            #   mkDerivation {
            #     pname = "primitive";
            #     version = "0.5.4.0";
            #     sha256 = "sha256-IDwqKIpl72t10hUNb5H4IEqkN63RCNjhbLn1gYZ87RU=";
            #     libraryHaskellDepends = [ base ghc-prim transformers ];
            #     testHaskellDepends = [ base ghc-prim ];
            #     description = "Primitive memory-related operations";
            #     license = pkgs.stdenv.lib.licenses.bsd3;
            #   }) {};

            # "vector" = hsuper.callPackage
            #   ({ mkDerivation, base, deepseq, ghc-prim, HUnit, primitive
            #   , QuickCheck, random, template-haskell, test-framework
            #   , test-framework-hunit, test-framework-quickcheck2, transformers
            #   }:
            #   mkDerivation {
            #     pname = "vector";
            #     version = "0.10.12.3";
            #     sha256 = "sha256-D3Zc4CONif/bZ2VqNk979QEtaN6WQtprsqQlth+I6Jo=";
            #     # revision = "3";
            #     # editedCabalFile = "0y5rh8k710i2a3p1h2rghvr5cfg78p5h0kbfi7ifxqqf6pzlyr1x";
            #     libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
            #     testHaskellDepends = [
            #       base HUnit QuickCheck random template-haskell test-framework
            #       test-framework-hunit test-framework-quickcheck2 transformers
            #     ];
            #     description = "Efficient Arrays";
            #     license = pkgs.stdenv.lib.licenses.bsd3;
            #   }) {};

            "mwc-random" = hsuper.callPackage
              ({ mkDerivation, base, math-functions, primitive, time, vector }:
              mkDerivation {
                pname = "mwc-random";
                version = "0.14.0.0";
                sha256 = "18pg24sw3b79b32cwx8q01q4k0lm34mwr3l6cdkchl8alvd0wdq0";
                libraryHaskellDepends = [
                  base math-functions primitive time vector
                ];
                doCheck = false;
                description = "Fast, high quality pseudo random number generation";
                license = pkgs.stdenv.lib.licenses.bsd3;
                hydraPlatforms = pkgs.stdenv.lib.platforms.none;
              }) {};

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
          ghcid
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
