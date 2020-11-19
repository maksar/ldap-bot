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
    flake-utils.lib.eachDefaultSystem
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
              cabal-install = "3.2.0.0";
              haskell-language-server = "0.6.0";
            };
          };
        }
      );
}
