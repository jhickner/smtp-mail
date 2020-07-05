let
  envSrc = fetchTarball "https://github.com/typetetris/ghcide-nixpkgs-20.03/tarball/master";
  env = import "${envSrc}/shell.nix";
in
  env {
    nixpkgssrc = import ./nixpkgs-commit.nix;
    nixpkgs = import ./nixpkgs.nix;
    p = f: [f.smtp-mail f.integration-test];
  }
