{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "QuickCheck".flags.old-random = false;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "HUnit".revision = (((hackage."HUnit")."1.6.1.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "hspec-expectations".revision = (((hackage."hspec-expectations")."0.8.2").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.2.0").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.0").revisions).default;
        "setenv".revision = (((hackage."setenv")."0.1.1.3").revisions).default;
        "base".revision = (((hackage."base")."4.14.1.0").revisions).default;
        "hspec-meta".revision = (((hackage."hspec-meta")."2.6.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "quickcheck-io".revision = (((hackage."quickcheck-io")."0.2.0").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.2").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        };
      compiler = {
        version = "8.10.2";
        nix-name = "ghc8102";
        packages = {
          "ghc-prim" = "0.6.1";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "directory" = "1.3.6.0";
          "template-haskell" = "2.16.0.0";
          "containers" = "0.6.2.1";
          "bytestring" = "0.10.10.0";
          "base" = "4.14.1.0";
          "time" = "1.9.3";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.10.2";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          };
        };
      };
  extras = hackage:
    { packages = { hspec-discover = ./.plan.nix/hspec-discover.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "hspec-discover" = { flags = {}; }; }; })
    ];
  }