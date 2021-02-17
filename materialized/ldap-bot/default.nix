{
  pkgs = hackage:
    {
      packages = {
        "warp".revision = (((hackage."warp")."3.3.13").revisions).default;
        "warp".flags.allow-sendfilefd = true;
        "warp".flags.network-bytestring = false;
        "warp".flags.warp-debug = false;
        "http-client".revision = (((hackage."http-client")."0.6.4.1").revisions).default;
        "http-client".flags.network-uri = true;
        "cookie".revision = (((hackage."cookie")."0.4.5").revisions).default;
        "void".revision = (((hackage."void")."0.7.3").revisions).default;
        "void".flags.safe = false;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.4").revisions).default;
        "semigroupoids".flags.comonad = true;
        "semigroupoids".flags.doctests = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.contravariant = true;
        "byteorder".revision = (((hackage."byteorder")."1.0.4").revisions).default;
        "singleton-bool".revision = (((hackage."singleton-bool")."0.1.5").revisions).default;
        "free".revision = (((hackage."free")."5.1.3").revisions).default;
        "tf-random".revision = (((hackage."tf-random")."0.5").revisions).default;
        "first-class-families".revision = (((hackage."first-class-families")."0.8.0.0").revisions).default;
        "ghc".revision = (((hackage."ghc")."8.10.2").revisions).default;
        "polysemy-plugin".revision = (((hackage."polysemy-plugin")."0.2.5.1").revisions).default;
        "polysemy-plugin".flags.corelint = false;
        "cereal".revision = (((hackage."cereal")."0.5.8.1").revisions).default;
        "cereal".flags.bytestring-builder = false;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "ghc-boot".revision = (((hackage."ghc-boot")."8.10.2").revisions).default;
        "attoparsec-iso8601".revision = (((hackage."attoparsec-iso8601")."1.0.1.0").revisions).default;
        "attoparsec-iso8601".flags.fast = false;
        "attoparsec-iso8601".flags.developer = false;
        "pretty-terminal".revision = (((hackage."pretty-terminal")."0.1.0.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.1.1").revisions).default;
        "old-time".revision = (((hackage."old-time")."1.1.0.3").revisions).default;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.7").revisions).default;
        "bifunctors".flags.semigroups = true;
        "bifunctors".flags.tagged = true;
        "haskeline".revision = (((hackage."haskeline")."0.8.0.1").revisions).default;
        "x509-validation".revision = (((hackage."x509-validation")."1.6.11").revisions).default;
        "fail".revision = (((hackage."fail")."4.9.0.0").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.0").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "dec".revision = (((hackage."dec")."0.0.3").revisions).default;
        "unix-time".revision = (((hackage."unix-time")."0.4.7").revisions).default;
        "http2".revision = (((hackage."http2")."2.0.5").revisions).default;
        "http2".flags.devel = false;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.11.2").revisions).default;
        "appar".revision = (((hackage."appar")."0.1.8").revisions).default;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "network-byte-order".revision = (((hackage."network-byte-order")."0.1.6").revisions).default;
        "sop-core".revision = (((hackage."sop-core")."0.5.0.1").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.10.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.3.0").revisions).default;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.5").revisions).default;
        "zlib".revision = (((hackage."zlib")."0.6.2.2").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "mmorph".revision = (((hackage."mmorph")."1.1.3").revisions).default;
        "unagi-chan".revision = (((hackage."unagi-chan")."0.4.1.3").revisions).default;
        "unagi-chan".flags.compare-benchmarks = false;
        "easy-file".revision = (((hackage."easy-file")."0.2.2").revisions).default;
        "ghci".revision = (((hackage."ghci")."8.10.2").revisions).default;
        "th-expand-syns".revision = (((hackage."th-expand-syns")."0.4.6.0").revisions).default;
        "cryptonite".revision = (((hackage."cryptonite")."0.27").revisions).default;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.use_target_attributes = true;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.support_aesni = true;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.check_alignment = false;
        "cryptonite".flags.old_toolchain_inliner = false;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "safe-exceptions".revision = (((hackage."safe-exceptions")."0.1.7.1").revisions).default;
        "adjunctions".revision = (((hackage."adjunctions")."4.4").revisions).default;
        "invariant".revision = (((hackage."invariant")."0.5.3").revisions).default;
        "th-orphans".revision = (((hackage."th-orphans")."0.13.11").revisions).default;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "http-api-data".revision = (((hackage."http-api-data")."0.4.2").revisions).default;
        "http-api-data".flags.use-text-show = false;
        "with-utf8".revision = (((hackage."with-utf8")."1.0.2.1").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."9.0.1").revisions).default;
        "megaparsec".flags.dev = false;
        "syb".revision = (((hackage."syb")."0.7.1").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2").revisions).default;
        "distributive".flags.semigroups = true;
        "distributive".flags.tagged = true;
        "generic-arbitrary".revision = (((hackage."generic-arbitrary")."0.1.0").revisions).default;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.6").revisions).default;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "QuickCheck".flags.old-random = false;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "time-manager".revision = (((hackage."time-manager")."0.0.0").revisions).default;
        "hspec-discover".revision = (((hackage."hspec-discover")."2.7.4").revisions).default;
        "aeson-qq".revision = (((hackage."aeson-qq")."0.8.3").revisions).default;
        "parallel".revision = (((hackage."parallel")."3.2.2.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "haskell-src-meta".revision = (((hackage."haskell-src-meta")."0.8.5").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.3").revisions).default;
        "string-conversions".revision = (((hackage."string-conversions")."0.4.0.1").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.16.0.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.0").revisions).default;
        "network".flags.devel = false;
        "word8".revision = (((hackage."word8")."0.1.3").revisions).default;
        "connection".revision = (((hackage."connection")."0.3.1").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "x509-store".revision = (((hackage."x509-store")."1.6.7").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.19.1").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.template-haskell = true;
        "data-default".revision = (((hackage."data-default")."0.7.1.1").revisions).default;
        "HUnit".revision = (((hackage."HUnit")."1.6.1.0").revisions).default;
        "data-default-instances-old-locale".revision = (((hackage."data-default-instances-old-locale")."0.0.1").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "th-reify-many".revision = (((hackage."th-reify-many")."0.1.9").revisions).default;
        "http-media".revision = (((hackage."http-media")."0.8.0.0").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.6").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "neat-interpolation".revision = (((hackage."neat-interpolation")."0.5.1.2").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "hspec-expectations".revision = (((hackage."hspec-expectations")."0.8.2").revisions).default;
        "psqueues".revision = (((hackage."psqueues")."0.2.7.2").revisions).default;
        "vector".revision = (((hackage."vector")."0.12.1.2").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "call-stack".revision = (((hackage."call-stack")."0.2.0").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.1.0").revisions).default;
        "profunctors".revision = (((hackage."profunctors")."5.6").revisions).default;
        "safe".revision = (((hackage."safe")."0.3.19").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.1.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.11.2").revisions).default;
        "silently".revision = (((hackage."silently")."1.2.5.1").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.4").revisions).default;
        "time-compat".flags.old-locale = false;
        "x509-system".revision = (((hackage."x509-system")."1.6.6").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "quickcheck-instances".revision = (((hackage."quickcheck-instances")."0.3.25.1").revisions).default;
        "quickcheck-instances".flags.bytestring-builder = false;
        "th-env".revision = (((hackage."th-env")."0.1.0.2").revisions).default;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "x509".revision = (((hackage."x509")."1.7.5").revisions).default;
        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.23.1").revisions).default;
        "lens".revision = (((hackage."lens")."4.19.2").revisions).default;
        "lens".flags.j = false;
        "lens".flags.test-properties = true;
        "lens".flags.old-inline-pragmas = false;
        "lens".flags.test-templates = true;
        "lens".flags.trustworthy = true;
        "lens".flags.test-doctests = true;
        "lens".flags.benchmark-uniplate = false;
        "lens".flags.inlining = true;
        "lens".flags.dump-splices = false;
        "lens".flags.test-hunit = true;
        "lens".flags.safe = false;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "reflection".revision = (((hackage."reflection")."2.1.6").revisions).default;
        "reflection".flags.slow = false;
        "reflection".flags.template-haskell = true;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "socks".revision = (((hackage."socks")."0.6.1").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.2.1").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.0").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "wai".revision = (((hackage."wai")."3.2.2.1").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.11").revisions).default;
        "setenv".revision = (((hackage."setenv")."0.1.1.3").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "StateVar".revision = (((hackage."StateVar")."1.2").revisions).default;
        "vault".revision = (((hackage."vault")."0.3.1.4").revisions).default;
        "vault".flags.useghc = true;
        "mime-types".revision = (((hackage."mime-types")."0.1.0.9").revisions).default;
        "http-client-tls".revision = (((hackage."http-client-tls")."0.3.5.3").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.2").revisions).default;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.tagged = true;
        "contravariant".flags.statevar = true;
        "atomic-primops".revision = (((hackage."atomic-primops")."0.8.4").revisions).default;
        "atomic-primops".flags.debug = false;
        "data-default-instances-dlist".revision = (((hackage."data-default-instances-dlist")."0.0.1").revisions).default;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.2.1").revisions).default;
        "parser-combinators".flags.dev = false;
        "blaze-markup".revision = (((hackage."blaze-markup")."0.8.2.7").revisions).default;
        "text".revision = (((hackage."text")."1.2.3.2").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.2.0.0").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.13.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.1.0.0").revisions).default;
        "base".revision = (((hackage."base")."4.14.1.0").revisions).default;
        "servant-client".revision = (((hackage."servant-client")."0.18.1").revisions).default;
        "ldap-client".revision = (((hackage."ldap-client")."0.4.1").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.6").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.test-doctests = true;
        "comonad".flags.containers = true;
        "hspec".revision = (((hackage."hspec")."2.7.4").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "th-compat".revision = (((hackage."th-compat")."0.1").revisions).default;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "type-errors-pretty".revision = (((hackage."type-errors-pretty")."0.0.1.1").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.4").revisions).default;
        "iproute".revision = (((hackage."iproute")."1.7.10").revisions).default;
        "polysemy".revision = (((hackage."polysemy")."1.3.0.0").revisions).default;
        "polysemy".flags.dump-core = false;
        "polysemy".flags.error-messages = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "quickcheck-io".revision = (((hackage."quickcheck-io")."0.2.0").revisions).default;
        "wai-extra".revision = (((hackage."wai-extra")."3.1.2").revisions).default;
        "wai-extra".flags.build-example = false;
        "data-default-instances-containers".revision = (((hackage."data-default-instances-containers")."0.0.1").revisions).default;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.4").revisions).default;
        "attoparsec".flags.developer = false;
        "blaze-html".revision = (((hackage."blaze-html")."0.9.1.2").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "file-embed".revision = (((hackage."file-embed")."0.0.13.0").revisions).default;
        "strict".revision = (((hackage."strict")."0.4").revisions).default;
        "strict".flags.assoc = true;
        "hpc".revision = (((hackage."hpc")."0.6.1.0").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "auto-update".revision = (((hackage."auto-update")."0.1.6").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.4").revisions).default;
        "servant-client-core".revision = (((hackage."servant-client-core")."0.18.1").revisions).default;
        "ghc-tcplugins-extra".revision = (((hackage."ghc-tcplugins-extra")."0.4").revisions).default;
        "ghc-tcplugins-extra".flags.deverror = false;
        "hspec-core".revision = (((hackage."hspec-core")."2.7.4").revisions).default;
        "unix-compat".revision = (((hackage."unix-compat")."0.5.2").revisions).default;
        "unix-compat".flags.old-time = false;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "tls".revision = (((hackage."tls")."1.5.4").revisions).default;
        "tls".flags.compat = true;
        "tls".flags.network = true;
        "tls".flags.hans = false;
        "kan-extensions".revision = (((hackage."kan-extensions")."5.2.1").revisions).default;
        "wai-logger".revision = (((hackage."wai-logger")."2.3.6").revisions).default;
        "th-lift".revision = (((hackage."th-lift")."0.8.2").revisions).default;
        "resourcet".revision = (((hackage."resourcet")."1.2.4.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.8").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.5.4.1").revisions).default;
        "aeson".flags.cffi = false;
        "aeson".flags.fast = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.developer = false;
        "wai-app-static".revision = (((hackage."wai-app-static")."3.1.7.2").revisions).default;
        "wai-app-static".flags.print = false;
        "type-errors".revision = (((hackage."type-errors")."0.2.0.0").revisions).default;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.2").revisions).default;
        "th-lift-instances".revision = (((hackage."th-lift-instances")."0.1.18").revisions).default;
        "servant-server".revision = (((hackage."servant-server")."0.18.1").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.3").revisions).default;
        "http-date".revision = (((hackage."http-date")."0.0.10").revisions).default;
        "servant".revision = (((hackage."servant")."0.18.1").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.3.2.0").revisions).default;
        "memory".revision = (((hackage."memory")."0.15.0").revisions).default;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_basement = true;
        "memory".flags.support_foundation = true;
        "memory".flags.support_deepseq = true;
        "fast-logger".revision = (((hackage."fast-logger")."3.0.2").revisions).default;
        "bsb-http-chunked".revision = (((hackage."bsb-http-chunked")."0.0.0.4").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "simple-sendfile".revision = (((hackage."simple-sendfile")."0.2.30").revisions).default;
        "simple-sendfile".flags.allow-bsd = true;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        };
      compiler = {
        version = "8.10.2";
        nix-name = "ghc8102";
        packages = {
          "ghc" = "8.10.2";
          "exceptions" = "0.10.4";
          "binary" = "0.8.8.0";
          "ghc-boot" = "8.10.2";
          "ghc-prim" = "0.6.1";
          "haskeline" = "0.8.0.1";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "ghc-heap" = "8.10.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "ghci" = "8.10.2";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.14.0";
          "directory" = "1.3.6.0";
          "template-haskell" = "2.16.0.0";
          "containers" = "0.6.2.1";
          "bytestring" = "0.10.10.0";
          "text" = "1.2.3.2";
          "Cabal" = "3.2.0.0";
          "base" = "4.14.1.0";
          "time" = "1.9.3";
          "terminfo" = "0.4.1.4";
          "transformers" = "0.5.6.2";
          "hpc" = "0.6.1.0";
          "filepath" = "1.4.2.1";
          "process" = "1.6.9.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.10.2";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          };
        };
      };
  extras = hackage:
    { packages = { ldap-bot = ./.plan.nix/ldap-bot.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "ldap-bot" = { flags = {}; }; }; })
    ];
  }