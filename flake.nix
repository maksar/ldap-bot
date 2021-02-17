{
  description = "LDAP-bot Flake";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = "github:NixOS/nixpkgs/master";
    };

    haskell-nix = {
      url = "github:hackworthltd/haskell.nix/flake-fixes";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs = { self, flake-utils, nixpkgs, haskell-nix }:
    flake-utils.lib.eachSystem (builtins.attrNames haskell-nix.legacyPackages)
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
            plan-sha256 = "165f82bgvn70drlqn2zdrj2zrapg0nnxmm3r86chz5aamksc75al";
            materialized = ./materialized/ldap-bot;
          };
        in
        rec {
          defaultApp = {
            type = "app";
            program = "${defaultPackage}/bin/ldap-bot-facebook";
          };

          defaultPackage = project.ldap-bot.components.exes.ldap-bot-facebook;

          devShell = project.shellFor {
            tools = {
              ormolu = {
                 version = "0.1.4.1";
                index-state = "2021-01-03T00:00:00Z";
                plan-sha256 = "1al44yikrb1aa4cnmd0g3vz31w6z29h684j78gk0m00qhkpzj71d";
                materialized = ./materialized/ormolu;
              };
              hlint = {
                version = "3.2.6";
                index-state = "2021-01-03T00:00:00Z";
                plan-sha256 = "1rh3vz14wr87ccbq1qrq4kb7mbkcmzn2l2xbrk2y8g1a3dz06i38";
                materialized = ./materialized/hlint;
              };
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
              hspec-discover = {
                version = "2.7.4";
                index-state = "2020-11-19T00:00:00Z";
                plan-sha256 = "09c6s3mkq1v5iypwiwfhkphxnxrvrwpxq947vmgvald8g0r6sbij";
                materialized = ./materialized/hspec-discover;
              };
            };
          };
        }
      );
}
