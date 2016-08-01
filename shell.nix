with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "opt-ghcjs";
  ghc = haskell.packages.ghc7103.ghc;
  shellHook = "export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt";
  buildInputs =
    [ nodejs
      ncurses
      zlib
      cabal-install
      git
      haskellPackages.happy ];
}
