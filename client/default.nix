# { rev ? "bc5f34aed92a04aa7feea0d65d47d676b47118af"
# , outputSha256 ? "1c6qyj4mflj4rfz701f4ypp1w5i57w2wwsbpxa0hx4jk5rgvyxhl"
# }:
{ rev ? "fdfe5b028bd4da08f0c8aabf9fb5e143ce96c56f"
, outputSha256 ? "0x0p418csdmpdfp6v4gl5ahzqhg115bb3cvrz1rb1jc7n4vxhcc8"
}:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  };
  pkgs = import nixpkgs {};

  chatclient-src = pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(pkgs.lib.hasSuffix ".nix" base)
    );
    src = pkgs.lib.cleanSource ./.;
  };

  chatclient-overrides = self: super: {
    chatclient = super.callCabal2nix "chatclient" chatclient-src {};
    http-types = super.callHackage "http-types" "0.11" {};
    resourcet = super.callHackage "resourcet" "1.1.11" {};
    servant = super.callHackage "servant" "0.12.1" {};
    servant-server = super.callHackage "servant-server" "0.12" {};
    conduit = super.callHackage "conduit" "1.2.13.1" {};
    miso = super.callCabal2nix "miso" (pkgs.fetchFromGitHub {
      owner = "dmjio";
      repo = "miso";
      rev = "c0a3ec5f6309cdff2b732507f6ce9db992da3cd3";
      sha256 = "15n813j9h8lszg8b0491s2nhpn3k37910h0hggc576rmdk74db5c";
      }) {};
  };

  cabal-hashes = builtins.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b894fb11eb8b8fb550ad22fbbac3d782758a9faf.tar.gz";
    sha256 = "02pgkxa4rljczfcc7hh622hv215r7xl9a3m2kghf0jlcyhwwdikf";
  };

  ghcPackages = pkgs.haskell.packages.ghc822.override(old: {
    all-cabal-hashes = cabal-hashes;
    overrides = self: super: chatclient-overrides self super // {
      base-compat = super.callHackage "base-compat" "0.9.3" {};
      cabal-plan = pkgs.haskell.lib.overrideCabal (
        super.callCabal2nix "cabal-plan" (pkgs.fetchFromGitHub {
          owner = "hvr";
          repo = "cabal-plan";
          rev = "67d6b9b3f15fde3f3fc38d4bccc589d2e1a5420c";
          sha256 = "1rl4xaln0akcx8n7vai6ajyp16v5bg7x23f1g0ly7cvi5mvi945w";
          }) {})
        { editedCabalFile = null; };
    };
    });

  ghcjsPackages = pkgs.haskell.packages.ghcjs80.override(old: {
    all-cabal-hashes = cabal-hashes;
    overrides = self: super: chatclient-overrides self super // {
      servant-client-ghcjs = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "servant-client-ghcjs" ((pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant";
        rev = "544bb8184e1adbcc2359767c172b4922f8b2d650";
        sha256 = "0hkyim72sk0c1p7rwv0dggk3j8zlxpgkjl14skqkrm9yas88r5yn";
        }) + /servant-client-ghcjs) {});
      servant-client-core = super.callHackage "servant-client-core" "0.12" {};
    };
  });
in rec
{ 
  server = pkgs.haskell.lib.justStaticExecutables ghcPackages.chatclient;
  server-shell = ghcPackages.shellFor {
    packages = p: [p.chatclient];
  };

  client = ghcjsPackages.chatclient;
  client-shell = ghcjsPackages.shellFor {
    packages = p: [p.chatclient];
    buildInputs = [ghcPackages.cabal-plan];
  };
}

