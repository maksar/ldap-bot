{
  description = "LDAP-bot Flake";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs = {
        nixpkgs = {
          follows = "haskell-nix/nixpkgs-unstable";
        };
      };
    };
  };

  outputs = { self, flake-utils, haskell-nix }:
    flake-utils.lib.eachSystem (builtins.attrNames haskell-nix.legacyPackages)
      (system:
        with haskell-nix.legacyPackages.${system};
        let
          compiler-nix-name = "ghc8104";
          project = pkgs.haskell-nix.cabalProject {
            inherit compiler-nix-name;
            src = pkgs.haskell-nix.haskellLib.cleanGit {
              name = "sources";
              src = ./.;
            };
            index-state = "2021-03-10T00:00:00Z";
            plan-sha256 = "1rsd9xcvkgmjx68zgnfz3rdfg5f3yfn7b7j7k7aqyicnxfdjs21k";
            materialized = ./materialized/ldap-bot;
          };
        in
        rec {
          defaultApp = {
            type = "app";
            program = "${defaultPackage}/bin/ldap-bot-facebook";
          };

          packages = {
            facebook = project.ldap-bot.components.exes.ldap-bot-facebook;
            console = project.ldap-bot.components.exes.ldap-bot-console;
          };

          defaultPackage = packages.facebook;

          devShell = project.shellFor {
            tools = {
              ormolu = "latest";
              hlint = "latest";
              cabal = "latest";
              haskell-language-server = "latest";
              hoogle = "latest";
              hspec-discover = "latest";
            };
          };
        }
      );
}
