let
  nixpkgsRev = "ae47c79479a086e96e2977c61e538881913c0c08"; /*v20.09*/
  nixpkgsSha256 = "1yaa02zdbmd4j5lrhbynk5xdv6dkfhajlbsf5fkhx73hj9knmfgn";

  nixpkgs = fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha256;
  };

  pkgs = import nixpkgs {};
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    basic-prelude
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    pkgs.cabal-install
    ghc
    pkgs.stylish-haskell
  ];

  in

  pkgs.mkShell {
    LANG="C.UTF-8";
    LC_ALL="";
    buildInputs = [
      nixPackages
    ];
  }
