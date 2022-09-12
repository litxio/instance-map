{
  description = "Instance Map";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      nonReinstallablePkgs = nixpkgs.lib.mkOption {
        apply = x: [
          "rts"
          "ghc-heap"
          "ghc-prim"
          "integer-gmp"
          "integer-simple"
          "base"
          "deepseq"
          "array"
          "ghc-boot-th"
          "pretty"
          "template-haskell"
          "ghcjs-prim"
          "ghcjs-th"
          "ghc-bignum"
          "exceptions"
          "stm"
          "ghc-boot"
          "ghc"
          "Win32"
          "array"
          "binary"
          "bytestring"
          "containers"
          "directory"
          "filepath"
          "ghc-boot"
          "ghc-compact"
          "ghc-prim"
          "hpc"
          "mtl"
          "parsec"
          "process"
          "text"
          "time"
          "transformers"
          "unix"
          "xhtml"
          "terminfo"
          "ghci"];
      };

      modules = [
        ({ lib, ... }: {
          # https://github.com/input-output-hk/haskell.nix/issues/829
          config.dontStrip = true;
          config.reinstallableLibGhc = false;
          #config.reinstallableLibGhc = true;
          options.nonReinstallablePkgs = nonReinstallablePkgs;
        })
      ];

      tools =
        {
          cabal = {
            version = "latest";
            inherit modules;
          };
          hlint = {
            version = "latest";
            inherit modules;
          };
        };
      overlays = [
        haskellNix.overlay

        (final: prev:
          # This overlay adds our project to pkgs
          let buildInstanceMap =
               extraModules:
                final.haskell-nix.project' {
                  src = final.haskell-nix.haskellLib.cleanGit { src = ./.; };
                  compiler-nix-name = "ghc924";

                  modules = modules ++ extraModules;
                  shell.tools = tools;
                  # Non-Haskell shell tools go here
                  shell.buildInputs = with pkgs; [
                    # glpk nodejs postgresql 
                    (final.haskell-nix.cabalProject ( {
                      name = "haskell-language-server";
                      src = final.fetchFromGitHub {
                        owner = "haskell";
                        repo = "haskell-language-server";
                        rev="c3c73cf30bf8b59182e6df674e3f804b55b062ea";
                        sha256 = "sha256-5/WNPM1egl3euQ0ljf0fq3xueChxOP6oNaNta8VvQe8=";
                      };
                      sha256map = {
                        "https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
                      #  "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
                      };
                      # Plan issues with the benchmarks, can try removing later
                      configureArgs = "--disable-benchmarks";
                      modules = modules ++ [{
                        # Tests don't pass for some reason, but this is a somewhat random revision.
                        packages.haskell-language-server.doCheck = false;
                      }];
                      compiler-nix-name = "ghc924";
                    })).haskell-language-server.components.exes.haskell-language-server
                  ];
                  shell.withHoogle = true;

                  # Needed when branch for a stack.yaml dep is not 'master'
                  #branchMap = {
                  #  "https://github.com/well-typed/large-records" = "main";
                  #};
                  #lookupBranch = { location, ... }: branchMap."${location}" or null;
                };
          in
            {
              instance-map = buildInstanceMap [];
              instance-map-profiling = buildInstanceMap [
                {
                  enableProfiling = true;
                  enableLibraryProfiling = true;
                }
              ];
            })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.instance-map.flake { };
      flake-profiling = pkgs.instance-map-profiling.flake { };
    in (flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."instance-map:exe:instance-map-exe";
      profiling = flake-profiling.packages."instance-map:exe:instance-map-exe";
      runShell = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [];
      };
    }));
}
