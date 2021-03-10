{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "ldap-bot"; version = "0.5.0.0"; };
      license = "NONE";
      copyright = "2020 Itransition";
      maintainer = "a.shestakov@itransition.com";
      author = "Alexander Shestakov";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-qq" or (errorHandler.buildDepError "aeson-qq"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."ldap-client" or (errorHandler.buildDepError "ldap-client"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
          (hsPkgs."polysemy" or (errorHandler.buildDepError "polysemy"))
          (hsPkgs."polysemy-plugin" or (errorHandler.buildDepError "polysemy-plugin"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
          ];
        buildable = true;
        modules = [
          "API"
          "App"
          "Client/API"
          "Client/Facebook"
          "Client/Model"
          "Env"
          "Server/API"
          "Server/Command"
          "Server/Hook"
          "Server/LDAP"
          "Server/Model"
          "Server/Registry"
          "Server/Verify"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "ldap-bot-console" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."pretty-terminal" or (errorHandler.buildDepError "pretty-terminal"))
            (hsPkgs."ldap-bot" or (errorHandler.buildDepError "ldap-bot"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Console.hs" ];
          };
        "ldap-bot-facebook" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ldap-bot" or (errorHandler.buildDepError "ldap-bot"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Facebook.hs" ];
          };
        };
      tests = {
        "ldap-bot-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."generic-arbitrary" or (errorHandler.buildDepError "generic-arbitrary"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."ldap-bot" or (errorHandler.buildDepError "ldap-bot"))
            (hsPkgs."ldap-client" or (errorHandler.buildDepError "ldap-client"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
            (hsPkgs."polysemy" or (errorHandler.buildDepError "polysemy"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Client/FacebookSpec"
            "Client/ModelSpec"
            "EnvSpec"
            "Server/CommandSpec"
            "Server/LDAPSpec"
            "Server/MessagesSpec"
            "Server/RegistrySpec"
            "Server/VerifySpec"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }