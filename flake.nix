{
  description = "LDAbot Flake";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
  };

  outputs = { self, flake-utils, haskell-nix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        with haskell-nix.legacyPackages.${system};
        let
          compiler-nix-name = "ghc8102";
          project = pkgs.haskell-nix.cabalProject {
            inherit compiler-nix-name;
            src = pkgs.haskell-nix.haskellLib.cleanGit {
              name = "sources";
              src = ./.;
            };
            index-state = "2020-11-19T00:00:00Z";
            plan-sha256 = "08256pxhqb5rgxh605kxb4d8vpqvnvva8r7j5584hrblj9zrgnx0";
            materialized = ./materialized/project;
          };
        in
        rec {
          defaultApp = {
            type = "app";
            program = "${defaultPackage}/bin/ldabot-exe";
          };

          defaultPackage = project.ldabot.components.exes.ldabot-exe;

          devShell = project.shellFor {
            tools = {
              cabal-install = {
                version = "3.2.0.0";
                index-state = "2020-11-19T00:00:00Z";
                plan-sha256 = "1kcgyw40svk41liwsa7xwx04alfsf68i54w8vghr63gr5ccggzgi";
                materialized = ./materialized/cabal-install;
              };
              haskell-language-server = {
                version = "0.6.0";
                index-state = "2020-11-19T00:00:00Z";
                plan-sha256 = "1symglyngily0k99gyr594aibkl4v5rd4k3qzv24ri9qmc49z27v";
                materialized = ./materialized/haskell-language-server;
              };
              hoogle = {
                version = "5.0.17.15";
                index-state = "2020-11-19T00:00:00Z";
                plan-sha256 = "1slrp7dm1fnl9sp1hiby5ncah8x52k7hz1mc4sa619i0hl9nhfq8";
                materialized = ./materialized/hoogle;
              };
            };
          };
        }
      );
}
